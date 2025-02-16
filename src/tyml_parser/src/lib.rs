use std::{mem::transmute, ops::Deref, sync::Arc};

use allocator_api2::vec::Vec;
use ast::Defines;
use bumpalo::Bump;
use error::ParseError;
use lexer::Lexer;
use parser::parse_defines;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

#[derive(Clone)]
pub struct Tyml {
    inner: Arc<TymlInner>,
}

impl Tyml {
    pub fn source_code(&self) -> &Arc<String> {
        &self.inner.source_code
    }

    pub fn ast<'this>(&'this self) -> &'this Defines<'this, 'this> {
        unsafe { transmute(self.inner.ast) }
    }

    pub fn errors<'this>(&'this self) -> &'this Vec<ParseError<'this, 'this>, &'this Bump> {
        unsafe { transmute(&self.inner.errors) }
    }
}

impl Tyml {
    pub fn parse(source_code: String) -> Result<Tyml, Tyml> {
        let source_code = Arc::new(source_code);
        let allocator = Box::new(Bump::new());

        let mut lexer = Lexer::new(source_code.as_ref().as_str());
        let mut errors = Vec::new_in(allocator.deref());

        let ast = parse_defines(&mut lexer, &mut errors, allocator.deref());

        let no_error = errors.is_empty();

        let fake_static_ast = unsafe { transmute(ast) };
        let fake_static_errors = unsafe { transmute(errors) };

        let tyml_inner = TymlInner {
            source_code,
            ast: fake_static_ast,
            errors: fake_static_errors,
            _allocator: allocator,
        };

        let tyml = Tyml {
            inner: Arc::new(tyml_inner),
        };

        if no_error { Ok(tyml) } else { Err(tyml) }
    }
}

/// READ ONLY!!
struct TymlInner {
    source_code: Arc<String>,
    /// fake static
    ast: &'static Defines<'static, 'static>,
    /// fake static
    errors: Vec<ParseError<'static, 'static>, &'static Bump>,
    /// freezed
    _allocator: Box<Bump>,
}

unsafe impl Send for TymlInner {}
unsafe impl Sync for TymlInner {}

#[cfg(test)]
mod test {

    use crate::lexer::Lexer;

    #[test]
    fn test() {
        let source = "
settings: {
    number = -3.65e-10
    binary = 0xFF
    string = \"aaaa\"
}

type Type {}
enum Enum {}
";
        let lexer = Lexer::new(source);

        for token in lexer {
            dbg!(token);
        }
    }
}
