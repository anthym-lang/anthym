use crate::ast::Ident;
use crate::builtins::*;
use crate::run::Value;
use anyhow::{anyhow, bail, Result};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

macro_rules! builtins {
    [$($name: ident),+] => {
        HashMap::from([$((stringify!($name).into(), (false, Rc::new(RefCell::new(Value::Fn(Rc::new(Box::new($name)))))))),+])
    };
}

pub(crate) type SharedEnv = Rc<RefCell<Env>>;

#[derive(Default)]
pub(crate) struct Env {
    pub values: HashMap<Ident, (bool, Rc<RefCell<Value>>)>,
    parent: Option<SharedEnv>,
}

impl Env {
    pub fn global() -> SharedEnv {
        Rc::new(RefCell::new(Self {
            values: builtins![println],
            ..Default::default()
        }))
    }

    fn get_inner(&self, name: &Ident) -> Result<(bool, Rc<RefCell<Value>>)> {
        match self.values.get(name) {
            Some(got) => Ok(got.clone()),
            None => self
                .parent
                .as_ref()
                .ok_or_else(|| anyhow!("variable not found: {name:?}"))?
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
            bail!("variable `{name:?}` is not mutable")
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
