use std::{cell, collections::HashMap, rc, sync};

use crate::jar::Jar;

// pub type EnvironmentRef = rc::Rc<cell::RefCell<Environment>>;
pub type EnvironmentRef = sync::Arc<sync::Mutex<Environment>>;

#[derive(Debug)]
pub struct Environment {
    pub store: HashMap<String, sync::Arc<Jar>>,
    pub parent: Option<EnvironmentRef>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn set_parent(&mut self, parent: EnvironmentRef) {
        self.parent = Some(parent);
    }

    pub fn set(&mut self, identifier: &str, value: Jar) {
        self.store
            .insert(identifier.to_string(), sync::Arc::new(value));
    }

    pub fn get<'a>(&'a self, identifier: &str) -> Option<sync::Arc<Jar>> {
        match self.store.get(identifier) {
            Some(jar) => Some(jar.clone()),
            None => match &self.parent {
                Some(parent) => parent.lock().unwrap().get(identifier),
                None => None,
            },
        }
    }
}
