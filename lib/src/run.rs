use crate::ast::*;
use crate::env::{Env, SharedEnv};
use crate::function::*;
use crate::parse::parse_text;
use anyhow::{anyhow, bail, Ok, Result};
use std::fmt::{Debug, Display};
use std::process::exit;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) enum Value {
    Float(f32),
    Number(i32),
    Bool(bool),
    Char(char),
    Str(String),
    Fn(Function),
    Nothing,
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => Debug::fmt(&fl, f),
            Self::Number(n) => Debug::fmt(&n, f),
            Self::Bool(b) => Debug::fmt(&b, f),
            Self::Char(c) => Debug::fmt(&c, f),
            Self::Str(s) => Debug::fmt(&s, f),
            Self::Fn(_) => {
                write!(f, "<function>")
            }
            Self::Nothing => write!(f, "<nothing>"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => Display::fmt(&fl, f),
            Self::Number(n) => Display::fmt(&n, f),
            Self::Bool(b) => Display::fmt(&b, f),
            Self::Char(c) => Display::fmt(&c, f),
            Self::Str(s) => Display::fmt(&s, f),
            Self::Fn(_) => {
                write!(f, "<function>")
            }
            Self::Nothing => write!(f, "<nothing>"),
        }
    }
}

fn run_literal(lit: Literal) -> Result<Value> {
    Ok(match lit {
        Literal::Str(s) => Value::Str(s),
        Literal::Bool(b) => Value::Bool(b),
        Literal::Char(c) => Value::Char(c),
        Literal::Float(f) => Value::Float(f),
        Literal::Number(n) => Value::Number(n),
    })
}

fn run_ident(ident: Ident, env: SharedEnv) -> Result<Value> {
    Ok(env.borrow().get(&ident)?)
}

fn run_fn_decl(fn_decl: FnDecl, env: SharedEnv) -> Result<Value> {
    env.borrow_mut().set(
        false,
        fn_decl.0,
        Value::Fn(Rc::new(Box::new(DynFunc::new(
            fn_decl.1.into_iter().map(|(arg, _ty)| arg).collect(), // FIXME: add static typing
            fn_decl.3,
        )))),
    )?;

    Ok(Value::Nothing)
}

fn run_if(if_stmt: IfStmt, env: SharedEnv) -> Result<Value> {
    if_stmt
        .0
        .into_iter()
        .find_map(|(cond, block)| match run_expr(cond, env.clone()) {
            Result::Ok(Value::Bool(true)) => Some(Ok(block)),
            Result::Ok(Value::Bool(false)) => None,
            Result::Ok(_) => Some(Err(anyhow!("not a boolean"))),
            Err(e) => Some(Err(e)),
        })
        .unwrap_or(Ok(if_stmt.1))
        .and_then(|block| run_block(block, env))
}

fn run_expr(expr: Expr, env: SharedEnv) -> Result<Value> {
    match expr {
        Expr::Ident(i) => run_ident(i, env),
        Expr::FnCall(name, args) => match env.borrow().get(&name)? {
            Value::Fn(func) => {
                let child = Env::new_child_scope(&env);
                func(
                    args.into_iter()
                        .map(|expr| run_expr(expr, child.clone()))
                        .try_collect()?,
                    child.clone(),
                )
            }
            _ => bail!("{name:?} is not a function"),
        },
        Expr::Literal(l) => run_literal(l),
    }
}

fn run_stmt(stmt: Stmt, env: SharedEnv) -> Result<Option<Value>> {
    match stmt {
        Stmt::Fn(decl) => {
            run_fn_decl(decl, env)?;
        }
        Stmt::If(if_stmt) => {
            run_if(if_stmt, env)?;
        }
        Stmt::Var(decl) => match decl {
            VarDecl::Let(name, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().set(false, name, res)?;
            }
            VarDecl::Mut(name, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().set(true, name, res)?;
            }
            VarDecl::ReAssign(name, val) => {
                let res = run_expr(val, env.clone())?;
                env.borrow_mut().reassign(&name, res)?;
            }
        },
        Stmt::Answer(answer) => return Ok(Some(run_expr(answer, env)?)),
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

    for stmt in ast.0 {
        if let Some(code) = run_stmt(stmt, global.clone())? {
            match code {
                Value::Number(n) => exit(n),
                Value::Nothing => break,
                other => bail!("invalid exit code: {other:?}"),
            }
        }
    }

    Ok(())
}
