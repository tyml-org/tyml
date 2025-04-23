use std::{fmt::Debug, mem::transmute, ops::Deref, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use tyml_diagnostic::DiagnosticBuilder;
use tyml_parser::{ast::Defines, error::ParseError, lexer::Lexer, parser::parse_defines};
use tyml_source::SourceCode;
use tyml_type::{
    error::TypeError,
    resolver::resolve_type,
    types::{NamedTypeMap, TypeTree},
};
use tyml_validate::validate::ValueTypeChecker;

pub extern crate tyml_diagnostic;
pub extern crate tyml_parser;
pub extern crate tyml_type;
pub extern crate tyml_validate;

#[derive(Debug)]
pub struct TymlContext<State = Initial> {
    state: State,
    pub tyml_source: SourceCode,
}

impl TymlContext {
    pub fn new(tyml_source: SourceCode) -> Self {
        TymlContext {
            state: Initial {},
            tyml_source,
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
                &self.tyml_source,
            );
        }

        for error in self.tyml().type_errors().iter() {
            error.build(self.tyml().named_type_map()).print(
                lang,
                &self.tyml_source,
                &self.tyml_source,
            );
        }
    }

    pub fn has_tyml_error(&self) -> bool
    where
        State: IParsed,
    {
        self.tyml().has_error()
    }
}

#[derive(Debug, Clone)]
pub struct Initial {}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub tyml: Tyml,
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
    ) -> ValueTypeChecker<'this, 'this, 'this, 'this, 'section, 'value> {
        ValueTypeChecker::new(self.type_tree(), self.named_type_map())
    }
}

impl Tyml {
    pub fn parse<T: Into<Arc<String>>>(source_code: T) -> Self {
        let source_code = source_code.into();
        let allocator = Box::new(Bump::new());

        let mut lexer = Lexer::new(&source_code);
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

    use allocator_api2::vec::Vec;
    use bumpalo::Bump;
    use tyml_diagnostic::{message::Lang, DiagnosticBuilder};
    use tyml_generator::{
        lexer::{GeneratorLexer, TokenizerRegistry},
        style::{
            key_value::{KeyValue, KeyValueKind},
            language::LanguageStyle,
            literal::{
                CustomLiteralOption, CustomRegexLiteral, EscapeOption, FloatLiteral, InfNanKind,
                Literal, UnicodeFormatKind,
            },
            section::{Section, SectionKind},
            value::Value,
            Parser, ParserGenerator, AST,
        },
    };
    use tyml_source::SourceCode;

    use crate::TymlContext;

    #[test]
    fn lib_test() {
        let source = "
settings: {
    ip: string
    port: int
}
";

        let ini_source = "
[settings]
ip = 192.168.1.8
port = 25565
";

        let tyml_source = SourceCode::new("test.tyml".to_string(), source.to_string());
        let validate_target_source =
            SourceCode::new("test.ini".to_string(), ini_source.to_string());

        let tyml = TymlContext::new(tyml_source).parse();
        let mut validator = tyml.tyml().value_type_checker();

        let ini_literal = CustomRegexLiteral {
            regex: r"[^ 　\t\[\]\n\r=;][^\[\]\n\r=;]+".into(),
            option: CustomLiteralOption {
                trim_space: true,
                escape: EscapeOption {
                    allow_escape: true,
                    unicode: UnicodeFormatKind::Normal,
                },
            },
        };

        let literal = Literal::Custom(ini_literal.clone());

        let any_string_literal = Literal::Custom(ini_literal);

        let language = LanguageStyle::Section {
            section: Section {
                literal: literal.clone(),
                kind: SectionKind::Bracket,
            },
            key_value: KeyValue {
                key: literal.clone(),
                kind: KeyValueKind::Equal,
                value: Value {
                    float: Some(FloatLiteral {
                        allow_e: true,
                        inf_nan_kind: InfNanKind::Insensitive,
                        allow_plus_minus: true,
                        allow_under_line: true,
                    }),
                    any_string: Some(any_string_literal),
                    ..Default::default()
                },
            },
        };

        let mut registry = TokenizerRegistry::new();

        let ml_parser = language.generate(&mut registry);

        registry.freeze();

        let mut errors = Vec::new();

        let allocator = Bump::new();

        let mut lexer = GeneratorLexer::new(&validate_target_source.code, &registry, &allocator);

        let ast = ml_parser.parse(&mut lexer, &mut errors).unwrap();

        ast.take_value(&mut Vec::new_in(&allocator), &mut validator);

        tyml.print_tyml_error(Lang::ja_JP);

        if let Err(errors) = validator.validate() {
            for error in errors.iter() {
                error.build(tyml.tyml().named_type_map()).print(
                    Lang::ja_JP,
                    &tyml.tyml_source,
                    &validate_target_source,
                );
            }
        }
    }
}
