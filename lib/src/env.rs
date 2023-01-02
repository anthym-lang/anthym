use crate::ast::Ident;
use crate::builtins::*;
use crate::value::Value;
use anyhow::{anyhow, bail, Result};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SharedEnv = Rc<RefCell<Env>>;

#[derive(Default)]
pub struct Env {
    pub values: HashMap<Ident, (bool, Rc<RefCell<Value>>)>,
    parent: Option<SharedEnv>,
}

impl Env {
    pub fn global() -> SharedEnv {
        Rc::new(RefCell::new(Self {
            values: builtins![
                println: builtin_to_function(println),
                throw: builtin_to_function(throw),
                u8: U8
            ],
            ..Default::default()
        }))
    }

    fn get_inner(&self, name: &Ident) -> Result<(bool, Rc<RefCell<Value>>)> {
        match self.values.get(name) {
            Some(got) => Ok(got.clone()),
            None => self
                .parent
                .as_ref()
                .ok_or_else(|| anyhow!("`{name}` is not defined"))?
                .borrow()
                .get_inner(name),
        }
    }

    pub fn get(&self, name: &Ident) -> Result<Value> {
        self.get_inner(name).map(|x| x.1.borrow().clone())
    }

    pub fn reassign(&self, name: &Ident, value: Value) -> Result<()> {
        if let (true, inner) = self.get_inner(name)? {
            inner.replace(value);
            Ok(())
        } else {
            bail!("`{name}` is not mutable")
        }
    }

    pub fn set(&mut self, mutable: bool, name: Ident, value: Value) -> Result<()> {
        self.values
            .insert(name, (mutable, Rc::new(RefCell::new(value))));
        Ok(())
    }

    pub fn new_child_scope(this: &SharedEnv) -> SharedEnv {
        Rc::new(RefCell::new(Self {
            parent: Some(Rc::clone(this)),
            ..Default::default()
        }))
    }
}
