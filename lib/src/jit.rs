use crate::ast::{Expr, FnCall, FnDecl, Ident, If, Literal, Stmt, Var};
use crate::builtins::{self, BUILTINS};
use crate::ice::IceExt;
use crate::parse::parse_text;
use anyhow::{anyhow, bail, Result};
pub use cranelift::prelude::settings::OptLevel;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, FuncOrDataId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::mem::transmute;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};

#[derive(Debug)]
pub enum Options {
    Jit(JitOptions),
    Build(BuildOptions),
}

#[derive(Debug)]
pub struct JitOptions {
    pub opt_level: OptLevel,
}

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub opt_level: OptLevel,
    pub output: PathBuf,
}

struct Jit<T>
where
    T: Module,
{
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: T,
}

impl Jit<JITModule> {
    fn new_jit(options: JitOptions) -> Self {
        let mut flags = settings::builder();
        flags.set("use_colocated_libcalls", "false").unwrap_or_ice();
        flags.set("is_pic", "false").unwrap_or_ice();
        flags
            .set("opt_level", &options.opt_level.to_string())
            .unwrap_or_ice();
        let isa_builder =
            cranelift_native::builder().unwrap_or_ice_msg("host machine is not supported fot JIT");
        let isa = isa_builder
            .finish(settings::Flags::new(flags))
            .unwrap_or_ice();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        builder.symbol("print_char", builtins::print_char as *const u8);
        let module = JITModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }

    fn run(mut self, input: &str) -> Result<()> {
        self.compile(input)?;
        self.module.finalize_definitions()?;
        let main = self
            .module
            .get_name("main")
            .ok_or_else(|| anyhow!("main function is not defined"))?;
        let main = match main {
            FuncOrDataId::Func(id) => id,
            FuncOrDataId::Data(_) => bail!("main is not a function"),
        };

        let code_ptr = self.module.get_finalized_function(main);

        // SAFETY: this might be completely unsafe
        unsafe {
            let code_fn: fn() -> i32 = transmute(code_ptr);
            exit(code_fn())
        }
    }
}

impl Jit<ObjectModule> {
    fn try_pathbuf_to_str(buf: &Path) -> Result<&str> {
        buf.to_str()
            .ok_or_else(|| anyhow!("path contains invalid characters"))
    }

    fn new_build(options: BuildOptions) -> Result<Self> {
        let mut flags = settings::builder();
        flags
            .set("opt_level", &options.opt_level.to_string())
            .unwrap_or_ice();
        let isa_builder =
            cranelift_native::builder().unwrap_or_ice_msg("host machine is not supported fot JIT");
        let isa = isa_builder
            .finish(settings::Flags::new(flags))
            .unwrap_or_ice();
        let mut out_path = options.output;
        out_path.set_extension("o");
        let builder = ObjectBuilder::new(
            isa,
            Self::try_pathbuf_to_str(&out_path)?,
            cranelift_module::default_libcall_names(),
        )
        .unwrap_or_ice();
        // builder.symbol("print_char", builtins::print_char as *const u8); // FIXME: make builtins
        //                                                                  // work here
        let module = ObjectModule::new(builder);

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        })
    }

    fn build(mut self, output: PathBuf, input: &str) -> Result<()> {
        self.compile(input)?;
        let obj = self.module.finish();
        let mut object = output.clone();
        object.set_extension("o");
        {
            let mut file = File::create(&object)?;
            file.write_all(&obj.object.write()?)?;
        }
        Command::new("cc")
            .args([
                "-o",
                Self::try_pathbuf_to_str(&output)?,
                Self::try_pathbuf_to_str(&object)?,
            ])
            .status()?;
        Ok(())
    }
}

impl<T> Jit<T>
where
    T: Module,
{
    fn compile(&mut self, input: &str) -> Result<()> {
        let program = parse_text(input)?;
        for func in program.0 {
            let name = func.name.clone();
            self.translate_func(func)?;
            let id =
                self.module
                    .declare_function(&name.0, Linkage::Export, &self.ctx.func.signature)?;
            self.module.define_function(id, &mut self.ctx)?;
            self.module.clear_context(&mut self.ctx);
        }

        Ok(())
    }

    fn translate_func(&mut self, func: FnDecl) -> Result<()> {
        for param in func.args.iter() {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(ident_to_type(&param.1)?));
        }

        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(ident_to_type(&func.ret)?));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);
        let vars = init_func_args(&mut builder, &func, block)?;
        let mut translator = FunctionTranslator {
            builder,
            vars,
            module: &mut self.module,
        };

        for stmt in func.block.0 {
            translator.translate_stmt(stmt)?;
        }

        translator.builder.finalize();
        Ok(())
    }
}

struct FunctionTranslator<'a, T>
where
    T: Module,
{
    builder: FunctionBuilder<'a>,
    vars: HashMap<Ident, (bool, Variable)>,
    module: &'a mut T,
}

impl<'a, T> FunctionTranslator<'a, T>
where
    T: Module,
{
    fn translate_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Var(var) => self.translate_var(var)?,
            Stmt::If(if_stmt) => self.translate_if(if_stmt)?,
            Stmt::Return(ret) => {
                let value = self.translate_expr(ret.expr)?;
                self.builder.ins().return_(&[value]);
            }
            Stmt::Expr(expr) => {
                self.translate_expr(expr)?;
            }
        }

        Ok(())
    }

    fn translate_if(&mut self, if_stmt: If) -> Result<()> {
        let if_condition = self.translate_expr(if_stmt.if_stmt.0)?;
        let if_block = self.builder.create_block();
        if if_stmt.else_ifs.len() > 1 {
            bail!("elseifs are not supported (yet)"); // TODO: support elseifs
        }
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(if_condition, if_block, &[], else_block, &[]);
        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);
        for stmt in if_stmt.if_stmt.1 .0 {
            self.translate_stmt(stmt)?;
        }
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        for stmt in if_stmt.else_block.0 {
            self.translate_stmt(stmt)?;
        }
        self.builder.ins().jump(merge_block, &[]);
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        Ok(())
    }

    fn translate_expr(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Literal(lit) => self.translate_literal(lit),
            Expr::FnCall(call) => self.translate_call(call),
            Expr::Ident(ident) => {
                let var = self
                    .vars
                    .get(&ident)
                    .ok_or_else(|| anyhow!("variable {ident} is not defined"))?
                    .1;

                Ok(self.builder.use_var(var))
            }
        }
    }

    fn translate_literal(&mut self, literal: Literal) -> Result<Value> {
        match literal {
            Literal::Int(int) => Ok(self.builder.ins().iconst(types::I32, int as i64)), // FIXME: use int's real type
            Literal::Float(float) => Ok(self.builder.ins().f32const(float)),
            Literal::Bool(bool) => Ok(self.builder.ins().iconst(types::I8, bool as i64)),
            Literal::Str(_str) => todo!("strings are not supported (yet)"),
            Literal::Char(char) => Ok(self.builder.ins().iconst(types::I8, char as i64)),
        }
    }

    fn translate_var(&mut self, var: Var) -> Result<()> {
        match var {
            Var::Let(name, _, value) | Var::Mut(name, _, value) => {
                let new_value = self.translate_expr(value)?;
                let variable = self.vars.get(&name).unwrap_or_ice();
                self.builder.def_var(variable.1, new_value);
            }
            Var::ReAssign(name, value) => {
                let new_value = self.translate_expr(value)?; // PERF: somehow optimize this so that
                                                             // it is not executed before checking mutability
                let variable = self.vars.get(&name).unwrap_or_ice();
                if !variable.0 {
                    bail!("variable {name} is not mutable");
                }

                self.builder.def_var(variable.1, new_value);
            }
        }

        Ok(())
    }

    fn translate_call(&mut self, call: FnCall) -> Result<Value> {
        let sig = if !BUILTINS.contains_key(&call.name) {
            let func_or_data_id = self
                .module
                .get_name(&call.name.0)
                .ok_or_else(|| anyhow!("function not found: {}", call.name))?;
            match func_or_data_id {
                FuncOrDataId::Func(func) => self
                    .module
                    .declarations()
                    .get_function_decl(func)
                    .signature
                    .clone(),
                FuncOrDataId::Data(_) => bail!("not a function: {}", call.name),
            }
        } else {
            let mut sig = self.module.make_signature();
            let rust_sig = BUILTINS.get(&call.name).unwrap_or_ice();
            for ty in rust_sig.0.iter() {
                sig.params.push(AbiParam::new(ident_to_type(ty)?));
            }
            sig.returns.push(AbiParam::new(ident_to_type(&rust_sig.1)?));

            sig
        };

        let callee = self
            .module
            .declare_function(&call.name.0, Linkage::Import, &sig)?; // FIXME: is the signature
                                                                     // wacky
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let arg_values: Result<Vec<Value>> = call
            .args
            .into_iter()
            .map(|arg| self.translate_expr(arg))
            .collect();
        let call = self.builder.ins().call(local_callee, &arg_values?);
        Ok(self.builder.inst_results(call)[0])
    }
}

fn ident_to_type(ident: &Ident) -> Result<types::Type> {
    let ty = match &*ident.0 {
        "float" => types::F32,
        "int" => types::I32,
        "bool" => types::I8,
        "string" => todo!("strings are not supported (yet)"),
        "char" => types::I8,
        ty => bail!("type not found: {ty}"),
    };

    Ok(ty)
}

fn init_func_args(
    builder: &mut FunctionBuilder<'_>,
    func: &FnDecl,
    block: Block,
) -> Result<HashMap<Ident, (bool, Variable)>> {
    let mut vars = HashMap::new();
    let mut idx = 0;
    for (i, (name, ty)) in func.args.iter().enumerate() {
        let val = builder.block_params(block)[i];
        let var = declare_variable(
            ident_to_type(ty)?,
            builder,
            &mut vars,
            &mut idx,
            name,
            false,
        );
        builder.def_var(var, val);
    }

    for stmt in &func.block.0 {
        declare_variables_in_stmt(builder, &mut vars, &mut idx, stmt)?;
    }

    Ok(vars)
}

fn declare_variables_in_stmt(
    builder: &mut FunctionBuilder<'_>,
    vars: &mut HashMap<Ident, (bool, Variable)>,
    idx: &mut usize,
    stmt: &Stmt,
) -> Result<()> {
    match stmt {
        Stmt::If(if_stmt) => {
            for stmt in &if_stmt.if_stmt.1 .0 {
                declare_variables_in_stmt(builder, vars, idx, stmt)?;
            }

            for else_if in &if_stmt.else_ifs {
                for stmt in &else_if.1 .0 {
                    declare_variables_in_stmt(builder, vars, idx, stmt)?;
                }
            }

            for stmt in &if_stmt.else_block.0 {
                declare_variables_in_stmt(builder, vars, idx, stmt)?;
            }
        }
        Stmt::Var(Var::Let(ref name, ref ty, _)) => {
            declare_variable(ident_to_type(ty)?, builder, vars, idx, name, false);
        }
        Stmt::Var(Var::Mut(ref name, ref ty, _)) => {
            declare_variable(ident_to_type(ty)?, builder, vars, idx, name, true);
        }
        _ => {}
    }

    Ok(())
}

fn declare_variable(
    ty: types::Type,
    builder: &mut FunctionBuilder<'_>,
    vars: &mut HashMap<Ident, (bool, Variable)>,
    idx: &mut usize,
    name: &Ident,
    mutable: bool,
) -> Variable {
    let var = Variable::new(*idx);
    if !vars.contains_key(name) {
        vars.insert(name.clone(), (mutable, var));
        builder.declare_var(var, ty);
        *idx += 1;
    }

    var
}

pub fn jit_text(options: Options, input: impl AsRef<str>) -> Result<()> {
    match options {
        Options::Jit(jit_opts) => Jit::new_jit(jit_opts).run(input.as_ref()),
        Options::Build(build_opts) => {
            Jit::new_build(build_opts.clone())?.build(build_opts.output, input.as_ref())
        }
    }
}
