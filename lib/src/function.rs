use crate::ast::{Block, Ident};
use crate::env::SharedEnv;
use crate::run::run_block;
use crate::value::Value;
use anyhow::{bail, Ok, Result};
use std::rc::Rc;

pub(crate) type Function =
    Rc<Box<(dyn Fn(Vec<Value>, SharedEnv) -> Result<Value> + Send + Sync + 'static)>>;

pub(crate) struct DynFunc {
    names: Vec<Ident>,
    code: Block,
}

impl DynFunc {
    pub(crate) fn new(names: Vec<Ident>, code: Block) -> Self {
        Self { names, code }
    }
}

impl FnOnce<(Vec<Value>, SharedEnv)> for DynFunc {
    type Output = Result<Value>;

    extern "rust-call" fn call_once(self, (args, env): (Vec<Value>, SharedEnv)) -> Self::Output {
        if args.len() != self.names.len() {
            bail!("wrong arg count");
        }
        {
            let mut borrow_mut = env.borrow_mut();
            self.names
                .into_iter()
                .zip(args.into_iter())
                .try_for_each(|(name, val)| {
                    borrow_mut.set(false, name, val)?;
                    Ok(())
                })?;
        }
        run_block(self.code, env)
    }
}

impl FnMut<(Vec<Value>, SharedEnv)> for DynFunc {
    extern "rust-call" fn call_mut(
        &mut self,
        (args, env): (Vec<Value>, SharedEnv),
    ) -> Self::Output {
        if args.len() != self.names.len() {
            bail!("wrong arg count");
        }
        {
            let mut borrow_mut = env.borrow_mut();
            self.names
                .iter()
                .cloned()
                .zip(args.into_iter())
                .try_for_each(|(name, val)| {
                    borrow_mut.set(false, name, val)?;
                    Ok(())
                })?;
        }
        run_block(self.code.clone(), env)
    }
}

impl Fn<(Vec<Value>, SharedEnv)> for DynFunc {
    extern "rust-call" fn call(&self, (args, env): (Vec<Value>, SharedEnv)) -> Self::Output {
        if args.len() != self.names.len() {
            bail!("wrong arg count");
        }
        {
            let mut borrow_mut = env.borrow_mut();
            self.names
                .iter()
                .cloned()
                .zip(args.into_iter())
                .try_for_each(|(name, val)| {
                    borrow_mut.set(false, name, val)?;
                    Ok(())
                })?;
        }
        run_block(self.code.clone(), env)
    }
}
