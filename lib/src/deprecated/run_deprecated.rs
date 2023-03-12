use crate::ast::*;
use crate::env::{Env, SharedEnv};
use crate::function::*;
use crate::parse::parse_text;
use crate::value::Value;
use anyhow::{anyhow, bail, Ok, Result};
use std::iter::once;
use std::process::exit;
use std::rc::Rc;

fn run_literal(lit: Literal) -> Result<Value> {
    Ok(match lit {
        Literal::Str(s) => Value::Str(s),
        Literal::Bool(b) => Value::Bool(b),
        Literal::Char(c) => Value::Char(c),
        Literal::Float(f) => Value::Float(f),
        Literal::Int(n) => Value::Number(n),
    })
}

fn run_ident(ident: Ident, env: SharedEnv) -> Result<Value> {
    Ok(env.borrow().get(&ident)?)
}

fn run_fn_decl(fn_decl: FnDecl, env: SharedEnv) -> Result<Value> {
    env.borrow_mut().set(
        false,
        fn_decl.name,
        Value::Fn(Rc::new(Box::new(DynFunc::new(
            fn_decl.args.into_iter().map(|(arg, _ty)| arg).collect(), // FIXME: add static typing
            fn_decl.block,
        )))),
    )?;

    Ok(Value::Nothing)
}

fn run_if(if_stmt: If, env: SharedEnv) -> Result<Value> {
    once(if_stmt.if_stmt)
        .chain(if_stmt.else_ifs)
        .into_iter()
        .find_map(|(cond, block)| match run_expr(cond, env.clone()) {
            Result::Ok(Value::Bool(true)) => Some(Ok(block)),
            Result::Ok(Value::Bool(false)) => None,
            Result::Ok(_) => Some(Err(anyhow!("not a boolean"))),
            Err(e) => Some(Err(e)),
        })
        .unwrap_or(Ok(if_stmt.else_block))
        .and_then(|block| run_block(block, env))
}

fn run_fn_call(call: FnCall, env: SharedEnv) -> Result<Value> {
    match env.borrow().get(&call.name)? {
        Value::Fn(func) => {
            let child = Env::new_child_scope(&env);
            func(
                call.args
                    .into_iter()
                    .map(|expr| run_expr(expr, child.clone()))
                    .collect::<Result<_>>()?,
                child,
            )
        }
        _ => bail!("`{name}` is not a function", name = call.name),
    }
}

fn run_expr(expr: Expr, env: SharedEnv) -> Result<Value> {
    match expr {
        Expr::Ident(i) => run_ident(i, env),
        Expr::FnCall(call) => run_fn_call(call, env),
        Expr::Literal(l) => run_literal(l),
    }
}

fn run_stmt(stmt: Stmt, env: SharedEnv) -> Result<Option<Value>> {
    match stmt {
        Stmt::If(if_stmt) => {
            run_if(if_stmt, env)?;
        }
        Stmt::Var(decl) => match decl {
            Var::Let(name, _ty, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().set(false, name, res)?;
            }
            Var::Mut(name, _ty, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().set(true, name, res)?;
            }
            Var::ReAssign(name, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().reassign(&name, res)?;
            }
        },
        Stmt::Return(return_) => return Ok(Some(run_expr(return_.expr, env)?)),
        Stmt::Expr(expr) => {
            run_expr(expr, env)?;
        }
    }

    Ok(None)
}

pub(crate) fn run_block(block: Block, env: SharedEnv) -> Result<Value> {
    let child = Env::new_child_scope(&env);
    for stmt in block.0 {
        if let Some(value) = run_stmt(stmt, child.clone())? {
            return Ok(value);
        }
    }

    Ok(Value::Nothing)
}

pub fn run_text(raw: &str) -> Result<()> {
    let ast = parse_text(raw)?;
    let global = Env::global();

    for decl in ast.0 {
        run_fn_decl(decl, global.clone())?;
    }

    match run_fn_call(
        FnCall {
            name: "main".into(),
            args: vec![],
        },
        global,
    )? {
        Value::Number(n) => exit(n),
        Value::Nothing => {}
        other => bail!("invalid exit code: `{other}`"),
    }

    Ok(())
}
