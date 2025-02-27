use std::cell::Cell;

use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};

struct NameEnvironment<'env, 'input> {
    parent: Option<&'env mut NameEnvironment<'env, 'input>>,
    map: HashMap<&'input str, NameID, DefaultHashBuilder, &'env Bump>,
}

impl<'env, 'input> NameEnvironment<'env, 'input> {
    fn new(parent: Option<&'env mut Self>, allocator: &'env Bump) -> &'env mut Self {
        allocator.alloc(Self {
            parent,
            map: HashMap::new_in(allocator),
        })
    }

    fn register(&mut self, name: &'input str) {
        self.map.insert(name, NameID::from(name));
    }

    fn resolve(&self, name: &'input str) -> Option<NameID> {
        match self.map.get(name) {
            Some(id) => Some(*id),
            None => self.parent.as_ref()?.resolve(name),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct NameID(usize);

impl From<&str> for NameID {
    fn from(value: &str) -> Self {
        NameID(value.as_ptr().addr())
    }
}
