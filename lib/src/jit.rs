use crate::ast::{self, Expr, FnCall, FnDecl, Ident, If, Literal, Stmt, Var};
use crate::ice::IceExt;
use crate::parse::parse_text;
use crate::rustic_std::{self, PRELUDE, PRELUDE_SRC};
use anyhow::{anyhow, bail, Result};
pub use cranelift::prelude::settings::OptLevel;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, FuncOrDataId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use log::*;
use std::collections::HashMap;
use std::env::set_current_dir;
use std::fs::{create_dir_all, File};
use std::io::{BufWriter, Write};
use std::mem::transmute;
use std::path::{Path, PathBuf};
use std::process::exit;
pub use target_lexicon::Triple;

fn get_pointer_type() -> Result<Type> {
    let bits = Triple::host()
        .pointer_width()
        .map_err(|_| anyhow!("could not get pointer width"))?
        .bits();
    Ok(Type::int(bits as u16).unwrap_or_ice())
}

#[derive(Debug)]
pub enum Options {
    Jit(JitOptions),
    Build(BuildOptions),
}

trait IsOption {}

#[derive(Debug)]
pub struct JitOptions {
    pub opt_level: OptLevel,
}

impl IsOption for JitOptions {}

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub output: PathBuf,
    pub opt_level: OptLevel,
    pub target: Triple,
}

impl IsOption for BuildOptions {}

struct Jit<T, U>
where
    T: Module,
    U: IsOption,
{
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: T,
    options: U,
}

impl Jit<JITModule, JitOptions> {
    fn new(options: JitOptions) -> Self {
        info!("creating new jit");
        info!("configuring cranelift");
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
        info!("adding builtin symbols");
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        builder.symbol("print", rustic_std::print as *const u8);
        builder.symbol("print_char", rustic_std::print_char as *const u8);
        let module = JITModule::new(builder);
        info!("ready to jit");
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            options,
            module,
        }
    }

    fn run(mut self, input: &str) -> Result<()> {
        info!("compiling input");
        self.compile(input)?;
        self.module.finalize_definitions()?;
        info!("locating main");
        let main = self
            .module
            .get_name("main")
            .ok_or_else(|| anyhow!("main function is not defined"))?;
        let main = match main {
            FuncOrDataId::Func(id) => id,
            FuncOrDataId::Data(_) => bail!("main is not a function"),
        };

        let code_ptr = self.module.get_finalized_function(main);

        info!("jumping");
        // SAFETY: this might be completely unsafe
        unsafe {
            let code_fn: fn() -> i32 = transmute(code_ptr);
            exit(code_fn())
        }
    }
}

impl Jit<ObjectModule, BuildOptions> {
    fn try_path_to_str(buf: &Path) -> Result<&str> {
        buf.to_str()
            .ok_or_else(|| anyhow!("path contains invalid characters"))
    }

    fn new(options: BuildOptions) -> Result<Self> {
        info!("creating new build");
        info!("configuring cranelift");
        let mut flags = settings::builder();
        flags.enable("is_pic").unwrap_or_ice();
        flags
            .set("opt_level", &options.opt_level.to_string())
            .unwrap_or_ice();
        let isa_builder = isa::lookup(options.target.clone())?;
        let isa = isa_builder
            .finish(settings::Flags::new(flags))
            .unwrap_or_ice();
        let mut out_path = options.output.clone();
        out_path.set_extension("o");
        let builder = ObjectBuilder::new(
            isa,
            Self::try_path_to_str(&out_path)?,
            cranelift_module::default_libcall_names(),
        )
        .unwrap_or_ice();
        let module = ObjectModule::new(builder);

        info!("ready to build");
        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            options,
            module,
        })
    }

    fn build(mut self, input: &str) -> Result<()> {
        create_dir_all("./output/artifacts/")?;
        set_current_dir("./output/artifacts/")?;
        info!("compiling input");
        self.compile(input)?;
        let obj = self.module.finish();
        info!("writing object file");
        let mut object_file = self.options.output.clone();
        object_file.set_extension("o");
        {
            let file = BufWriter::new(File::create(&object_file)?);
            obj.object
                .write_stream(file)
                .map_err(|err| anyhow!("{err}"))?;
        }

        let file = File::create("./prelude.c")?;
        {
            let mut writer = BufWriter::new(&file);
            writer.write_all(PRELUDE_SRC)?;
        }

        let host = Triple::host().to_string();
        info!("configuring link command");
        let mut cmd = cc::Build::new()
            .cargo_metadata(false)
            .opt_level_str(match self.options.opt_level {
                OptLevel::None => "0",
                OptLevel::Speed => "3",
                OptLevel::SpeedAndSize => "1",
            })
            .host(&host)
            .target(&self.options.target.to_string()) // TODO: allow for custom targets
            .debug(false)
            .flag("./prelude.c")
            .flag(Self::try_path_to_str(&object_file)?)
            .try_get_compiler()?
            .to_command();
        if host.contains("msvc") {
            cmd.arg(format!("-out:{}", self.options.output.display()));
        } else {
            cmd.arg("-o");
            cmd.arg(format!("../{}", self.options.output.display()));
        }
        debug!("link command: {cmd:?}");
        info!("running link command");
        cmd.status()?;
        Ok(())
    }
}

impl<T, U> Jit<T, U>
where
    T: Module,
    U: IsOption,
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
            data_ctx: &mut self.data_ctx,
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
    data_ctx: &'a mut DataContext,
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

    fn translate_if_block(&mut self, merge_block: Block, ast_block: ast::Block) -> Result<Block> {
        let block = self.builder.create_block();
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);
        for stmt in ast_block.0 {
            self.translate_stmt(stmt)?;
        }
        self.builder.ins().jump(merge_block, &[]);
        Ok(block)
    }

    fn translate_if(&mut self, if_stmt: If) -> Result<()> {
        let merge_block = self.builder.create_block();
        let if_condition = self.translate_expr(if_stmt.if_stmt.0)?;
        let if_block = self.translate_if_block(merge_block, if_stmt.if_stmt.1)?;
        // TODO: ELSEIFS!!!
        let else_block = self.translate_if_block(merge_block, if_stmt.else_block)?;

        self.builder
            .ins()
            .brif(if_condition, if_block, &[], else_block, &[]);

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
            Literal::Int(int) => Ok(self.builder.ins().iconst(types::I32, int as i64)),
            Literal::Float(float) => Ok(self.builder.ins().f32const(float)),
            Literal::Bool(bool) => Ok(self.builder.ins().iconst(types::I8, bool as i64)),
            Literal::Str(str) => Ok(self.translate_string(str)?),
            Literal::Char(char) => Ok(self.builder.ins().iconst(types::I8, char as i64)),
        }
    }

    fn translate_string(&mut self, string: String) -> Result<Value> {
        self.data_ctx
            .define(format!("{string}\0").as_bytes().to_vec().into_boxed_slice());
        let sym = self.module.declare_anonymous_data(true, false)?;
        let _result = self.module.define_data(sym, self.data_ctx);
        let local_id = self.module.declare_data_in_func(sym, self.builder.func);
        self.data_ctx.clear();
        Ok(self
            .builder
            .ins()
            .symbol_value(get_pointer_type()?, local_id))
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
        let sig = if !PRELUDE.contains_key(&call.name) {
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
            let rust_sig = PRELUDE.get(&call.name).unwrap_or_ice();
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

fn ident_to_type(ident: &Ident) -> Result<Type> {
    match &*ident.0 {
        "float" => Ok(types::F32),
        "int" => Ok(types::I32),
        "bool" => Ok(types::I8),
        "string" => get_pointer_type(),
        "char" => Ok(types::I8),
        ty => bail!("type not found: {ty}"),
    }
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
    ty: Type,
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
        Options::Jit(jit_opts) => <Jit<JITModule, JitOptions>>::new(jit_opts).run(input.as_ref()),
        Options::Build(build_opts) => {
            <Jit<ObjectModule, BuildOptions>>::new(build_opts)?.build(input.as_ref())
        }
    }
}
