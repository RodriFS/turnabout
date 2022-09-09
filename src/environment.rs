use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{errors::Error, types::Type};

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    pub vars: HashMap<String, Type>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            parent,
            vars: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Type> {
        match self.vars.get(name) {
            Some(v) => Some(v.clone().to_owned()),
            None => self.parent.as_ref().and_then(|p| p.borrow().lookup(name)),
        }
    }

    pub fn assign(&mut self, name: String, value: Type) -> Result<(), Error> {
        if !self.vars.contains_key(&name) {
            match self.parent.as_mut() {
                Some(p) => {
                    p.borrow_mut().vars.insert(name, value);
                    return Ok(());
                }
                None => return Err(Error::RuntimeError("Variable not declared")),
            }
        };
        self.vars.insert(name, value);
        Ok(())
    }
}
