use std::{fmt::Debug, mem::transmute, ops::Deref, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use tyml_diagnostic::DiagnosticSpan;
use tyml_parser::{ast::Defines, error::ParseError, lexer::Lexer, parser::parse_defines};
use tyml_type::{
    error::TypeError,
    resolver::resolve_type,
    types::{NamedTypeMap, TypeTree},
};
use tyml_validate::validate::{AnyStringEvaluator, ValueTypeChecker};

pub extern crate tyml_diagnostic;
pub extern crate tyml_parser;
pub extern crate tyml_type;
pub extern crate tyml_validate;

#[derive(Debug, Clone)]
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

    pub fn type_tree<'this>(&'this self) -> &'this TypeTree<'this, 'this> {
        unsafe { transmute(&self.inner.type_tree) }
    }

    pub fn named_type_map<'this>(&'this self) -> &'this NamedTypeMap<'this, 'this> {
        unsafe { transmute(&self.inner.named_type_map) }
    }

    pub fn parse_errors<'this>(&'this self) -> &'this Vec<ParseError<'this, 'this>, &'this Bump> {
        unsafe { transmute(&self.inner.parse_errors) }
    }

    pub fn type_errors<'this>(&'this self) -> &'this Vec<TypeError<'this, 'this>, &'this Bump> {
        unsafe { transmute(&self.inner.type_errors) }
    }

    pub fn value_type_checker<'this, 'section, 'value>(
        &'this self,
        any_string_evaluator_override: Option<Box<dyn AnyStringEvaluator>>,
    ) -> ValueTypeChecker<'this, 'this, 'this, 'this, 'section, 'value, DiagnosticSpan> {
        ValueTypeChecker::new(
            self.type_tree(),
            self.named_type_map(),
            any_string_evaluator_override,
        )
    }
}

impl Tyml {
    pub fn parse<T: Into<Arc<String>>>(source_code: T) -> Result<Tyml, Tyml> {
        let source_code = source_code.into();
        let allocator = Box::new(Bump::new());

        let mut lexer = Lexer::new(source_code.as_ref().as_str());
        let mut parse_errors = Vec::new_in(allocator.deref());

        let ast = parse_defines(&mut lexer, &mut parse_errors, allocator.deref());

        let (type_tree, named_type_map, type_errors) = resolve_type(ast, allocator.deref());

        let no_error = parse_errors.is_empty() && type_errors.is_empty();

        let fake_static_ast = unsafe { transmute(ast) };
        let fake_static_type_tree = unsafe { transmute(type_tree) };
        let fake_static_named_type_map = unsafe { transmute(named_type_map) };
        let fake_static_parse_errors = unsafe { transmute(parse_errors) };
        let fake_static_type_errors = unsafe { transmute(type_errors) };

        let tyml_inner = TymlInner {
            source_code,
            ast: fake_static_ast,
            type_tree: fake_static_type_tree,
            named_type_map: fake_static_named_type_map,
            parse_errors: fake_static_parse_errors,
            type_errors: fake_static_type_errors,
            _allocator: allocator,
        };

        let tyml = Tyml {
            inner: Arc::new(tyml_inner),
        };

        if no_error {
            Ok(tyml)
        } else {
            Err(tyml)
        }
    }
}

/// READ ONLY!!
#[derive(Debug)]
struct TymlInner {
    source_code: Arc<String>,
    /// fake static
    ast: &'static Defines<'static, 'static>,
    type_tree: TypeTree<'static, 'static>,
    named_type_map: NamedTypeMap<'static, 'static>,
    parse_errors: Vec<ParseError<'static, 'static>, &'static Bump>,
    type_errors: Vec<TypeError<'static, 'static>, &'static Bump>,
    /// freezed
    _allocator: Box<Bump>,
}

unsafe impl Send for TymlInner {}
unsafe impl Sync for TymlInner {}

#[cfg(test)]
mod tests {
    use std::convert::identity;

    use allocator_api2::vec;
    use hashbrown::HashMap;
    use tyml_diagnostic::DiagnosticSpan;
    use tyml_validate::validate::{Value, ValueTree};

    use crate::Tyml;

    #[test]
    fn test() {
        let source = "
settings: {
    number = -3.65e-10
    binary = 0xFF
    string: string? = \"aaaa\"
}

type Server {
    name: string
    ip: string
    port: int? = 25565
    whitelist: [ string ]
}
enum Enum {
    Element0
    Element1
}
";
        let tyml = Tyml::parse(source.to_string()).unwrap_or_else(identity);

        let mut checker = tyml.value_type_checker(None);

        let mut elements = HashMap::new();
        elements.insert(
            "number".into(),
            vec![ValueTree::Value {
                value: Value::Float(10.0),
                span: DiagnosticSpan::UnicodeCharacter(0..0),
            }],
        );
        elements.insert(
            "binary".into(),
            vec![ValueTree::Value {
                value: Value::Int(0xFF),
                span: DiagnosticSpan::UnicodeCharacter(0..0),
            }],
        );
        elements.insert(
            "string".into(),
            vec![ValueTree::Value {
                value: Value::None,
                span: DiagnosticSpan::UnicodeCharacter(0..0),
            }],
        );

        let settings = ValueTree::Section {
            elements,
            span: DiagnosticSpan::UnicodeCharacter(0..0),
        };

        checker.set_value(
            [("settings", DiagnosticSpan::UnicodeCharacter(0..0))].into_iter(),
            settings,
        );

        checker.validate().unwrap();
    }
}
