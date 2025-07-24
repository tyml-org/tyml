use std::cell::RefCell;

use bumpalo::Bump;
use hashbrown::{DefaultHashBuilder, HashMap};
use tyml_parser::ast::TypeDefine;

pub(crate) struct NameEnvironment<'env, 'input> {
    parent: Option<&'env NameEnvironment<'env, 'input>>,
    map: RefCell<HashMap<&'input str, NameID, DefaultHashBuilder, &'env Bump>>,
}

impl<'env, 'input> NameEnvironment<'env, 'input> {
    pub fn new(parent: Option<&'env Self>, allocator: &'env Bump) -> &'env Self {
        allocator.alloc(Self {
            parent,
            map: RefCell::new(HashMap::new_in(allocator)),
        })
    }

    pub fn register(&self, type_define: &TypeDefine<'input, '_>) {
        let name = match type_define {
            TypeDefine::Struct(struct_define) => struct_define.name.value,
            TypeDefine::Enum(enum_define) => enum_define.name.value,
        };

        let name_id = NameID(name.as_ptr().addr());
        self.map.borrow_mut().insert(name, name_id);
    }

    pub fn resolve(&self, name: &'input str) -> Option<NameID> {
        match self.map.borrow().get(name) {
            Some(id) => Some(*id),
            None => self.parent.as_ref()?.resolve(name),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NameID(usize);

impl<'input, 'allocator> From<&TypeDefine<'input, 'allocator>> for NameID {
    fn from(value: &TypeDefine<'input, 'allocator>) -> Self {
        let name = match value {
            TypeDefine::Struct(struct_define) => struct_define.name.value,
            TypeDefine::Enum(enum_define) => enum_define.name.value,
        };
        NameID(name.as_ptr().addr())
    }
}
