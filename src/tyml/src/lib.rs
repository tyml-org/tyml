use std::{fmt::Debug, mem::transmute, ops::Deref, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use tyml_diagnostic::{DiagnosticBuilder, DiagnosticSpan};
use tyml_parser::{ast::Defines, error::ParseError, lexer::Lexer, parser::parse_defines};
use tyml_source::SourceCode;
use tyml_type::{
    error::TypeError,
    resolver::resolve_type,
    types::{NamedTypeMap, TypeTree},
};
use tyml_validate::{
    error::TymlValueValidateError,
    validate::{ValidateEvaluator, ValueTypeChecker},
};

pub extern crate tyml_diagnostic;
pub extern crate tyml_parser;
pub extern crate tyml_type;
pub extern crate tyml_validate;

#[derive(Debug)]
pub struct TymlContext<State = Initial> {
    state: State,
    pub tyml_source: SourceCode,
    pub validate_target_source: SourceCode,
}

impl TymlContext {
    pub fn new(tyml_source: SourceCode, validate_target_source: SourceCode) -> Self {
        TymlContext {
            state: Initial {},
            tyml_source,
            validate_target_source,
        }
    }
}

impl<State> TymlContext<State> {
    pub fn parse(&self) -> TymlContext<Parsed>
    where
        State: IInitial,
    {
        TymlContext {
            state: Parsed {
                tyml: Tyml::parse(self.tyml_source.code.clone()),
            },
            tyml_source: self.tyml_source.clone(),
            validate_target_source: self.validate_target_source.clone(),
        }
    }

    pub fn tyml(&self) -> &Tyml
    where
        State: IParsed,
    {
        self.state.tyml()
    }

    pub fn print_tyml_error(&self, lang: &str)
    where
        State: IParsed,
    {
        for error in self.tyml().parse_errors().iter() {
            error.build(self.tyml().named_type_map()).print(
                lang,
                &self.tyml_source,
                &self.validate_target_source,
            );
        }

        for error in self.tyml().type_errors().iter() {
            error.build(self.tyml().named_type_map()).print(
                lang,
                &self.tyml_source,
                &self.validate_target_source,
            );
        }
    }

    pub fn has_tyml_error(&self) -> bool
    where
        State: IParsed,
    {
        self.tyml().has_error()
    }

    pub fn validate(&self, validate_evaluator: ValidateEvaluator) -> TymlContext<Validated>
    where
        State: IParsed,
    {
        let validator = self.tyml().value_type_checker(validate_evaluator);
        let result = validator.validate();

        TymlContext {
            state: Validated {
                tyml: self.tyml().clone(),
                result,
            },
            tyml_source: self.tyml_source.clone(),
            validate_target_source: self.validate_target_source.clone(),
        }
    }

    pub fn validate_result(&self) -> &Result<(), Vec<TymlValueValidateError<DiagnosticSpan>>>
    where
        State: IValidated,
    {
        self.state.result()
    }

    pub fn print_validate_error(&self, lang: &str)
    where
        State: IValidated,
    {
        if let Err(errors) = self.state.result() {
            for error in errors.iter() {
                error.build(self.tyml().named_type_map()).print(
                    lang,
                    &self.tyml_source,
                    &self.validate_target_source,
                );
            }
        }
    }

    pub fn has_validate_error(&self) -> bool
    where
        State: IValidated,
    {
        self.state.result().is_err()
    }

    pub fn has_error(&self) -> bool
    where
        State: IValidated,
    {
        self.has_tyml_error() || self.has_validate_error()
    }
}

#[derive(Debug)]
pub struct Initial {}

#[derive(Debug)]
pub struct Parsed {
    pub tyml: Tyml,
}

#[derive(Debug)]
pub struct Validated {
    pub tyml: Tyml,
    pub result: Result<(), Vec<TymlValueValidateError<DiagnosticSpan>>>,
}

pub trait IInitial {}

impl IInitial for Initial {}

pub trait IParsed {
    fn tyml(&self) -> &Tyml;
}

impl IParsed for Parsed {
    fn tyml(&self) -> &Tyml {
        &self.tyml
    }
}

impl IParsed for Validated {
    fn tyml(&self) -> &Tyml {
        &self.tyml
    }
}

pub trait IValidated: IParsed {
    fn result(&self) -> &Result<(), Vec<TymlValueValidateError<DiagnosticSpan>>>;
}

impl IValidated for Validated {
    fn result(&self) -> &Result<(), Vec<TymlValueValidateError<DiagnosticSpan>>> {
        &self.result
    }
}

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

    pub fn has_error(&self) -> bool {
        !self.parse_errors().is_empty() || !self.type_errors().is_empty()
    }

    pub fn value_type_checker<'this, 'section, 'value>(
        &'this self,
        validate_evaluator: ValidateEvaluator,
    ) -> ValueTypeChecker<'this, 'this, 'this, 'this, 'section, 'value, DiagnosticSpan> {
        ValueTypeChecker::new(self.type_tree(), self.named_type_map(), validate_evaluator)
    }
}

impl Tyml {
    pub fn parse<T: Into<Arc<String>>>(source_code: T) -> Self {
        let source_code = source_code.into();
        let allocator = Box::new(Bump::new());

        let mut lexer = Lexer::new(source_code.as_ref().as_str());
        let mut parse_errors = Vec::new_in(allocator.deref());

        let ast = parse_defines(&mut lexer, &mut parse_errors, allocator.deref());

        let (type_tree, named_type_map, type_errors) = resolve_type(ast, allocator.deref());

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

        Self {
            inner: Arc::new(tyml_inner),
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

    use allocator_api2::vec;
    use hashbrown::HashMap;
    use tyml_diagnostic::{message::Lang, DiagnosticBuilder, DiagnosticSpan};
    use tyml_source::SourceCode;
    use tyml_validate::validate::{ValidateEvaluator, Value, ValueTree};

    use crate::TymlContext;

    #[test]
    fn test() {
        let source = "
settings: {
    number = -3.65e-10
    binary = 0xFF
    string: string?
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
        let tyml_source = SourceCode::new("test.tyml".to_string(), source.to_string());
        let validate_target_source = SourceCode::new("test.ml".to_string(), "".to_string());

        let tyml = TymlContext::new(tyml_source, validate_target_source).parse();
        let mut validator = tyml.tyml().value_type_checker(ValidateEvaluator {
            any_string_evaluator_override: None,
        });

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

        validator.set_value(
            [("settings", DiagnosticSpan::UnicodeCharacter(0..0))].into_iter(),
            settings,
        );

        tyml.print_tyml_error(Lang::ja_JP);

        if let Err(errors) = validator.validate() {
            for error in errors.iter() {
                error.build(tyml.tyml().named_type_map()).print(
                    Lang::ja_JP,
                    &tyml.tyml_source,
                    &tyml.validate_target_source,
                );
            }
        }
    }
}
