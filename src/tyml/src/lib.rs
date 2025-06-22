pub(crate) mod cache;
pub mod header;

use std::{
    collections::BTreeMap,
    fmt::Debug,
    mem::transmute,
    ops::{Deref, Range},
    sync::Arc,
};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use tyml_diagnostic::DiagnosticBuilder;
use tyml_generator::{
    lexer::{GeneratorLexer, TokenizerRegistry},
    style::{
        error::GeneratedParseError, language::LanguageStyle, ASTTokenKind, Parser, ParserGenerator,
        AST,
    },
};
use tyml_parser::{ast::Defines, error::ParseError, lexer::Lexer, parser::parse_defines};
use tyml_source::SourceCode;
use tyml_type::{
    error::TypeError,
    resolver::resolve_type,
    types::{NamedTypeMap, TypeTree},
};
use tyml_validate::{error::TymlValueValidateError, validate::ValueTypeChecker};

pub extern crate tyml_diagnostic;
pub extern crate tyml_generator;
pub extern crate tyml_parser;
pub extern crate tyml_source;
pub extern crate tyml_type;
pub extern crate tyml_validate;

#[derive(Debug, Clone)]
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

    pub fn ml_parse_and_validate(
        &self,
        ml_language_style: &LanguageStyle,
        ml_source_code: &SourceCode,
        tokens: Option<&mut BTreeMap<usize, (ASTTokenKind, Range<usize>)>>,
    ) -> TymlContext<Validated>
    where
        State: IParsed,
    {
        let mut registry = TokenizerRegistry::new();

        let ml_parser = ml_language_style.generate(&mut registry);

        registry.freeze();

        let mut errors = Vec::new();
        let allocator = Bump::new();

        let mut lexer = GeneratorLexer::new(&ml_source_code.code, &registry, &allocator);

        let ast = ml_parser.parse(&mut lexer, &mut errors).unwrap();

        if let Some(tokens) = tokens {
            ast.take_token(tokens);

            for comment_span in lexer.comment_spans.iter() {
                tokens.insert(
                    comment_span.start,
                    (ASTTokenKind::Comment, comment_span.clone()),
                );
            }
        }

        let mut validator = self.tyml().value_type_checker();

        ast.take_value(&mut Vec::new_in(&allocator), &mut validator);

        let ml_validate_error = validator.validate().err().unwrap_or_default();

        let validator =
            ValidatorHolder::new(self.tyml().clone(), ml_source_code.clone(), validator);

        TymlContext {
            state: Validated {
                tyml: self.tyml().clone(),
                ml_source_code: ml_source_code.clone(),
                validator,
                ml_parse_error: Arc::new(errors),
                ml_validate_error: Arc::new(ml_validate_error),
            },
            tyml_source: self.tyml_source.clone(),
        }
    }

    pub fn ml_source_code(&self) -> &SourceCode
    where
        State: IValidated,
    {
        self.state.ml_source_code()
    }

    pub fn validator(&self) -> &ValueTypeChecker
    where
        State: IValidated,
    {
        &self.state.validator()
    }

    pub fn print_ml_parse_error(&self, lang: &str)
    where
        State: IValidated,
    {
        for error in self.ml_parse_error().iter() {
            error.build(self.tyml().named_type_map()).print(
                lang,
                &self.tyml_source,
                &self.state.ml_source_code(),
            );
        }
    }

    pub fn has_ml_parse_error(&self) -> bool
    where
        State: IValidated,
    {
        !self.state.ml_parse_error().is_empty()
    }

    pub fn ml_parse_error(&self) -> &Arc<Vec<GeneratedParseError>>
    where
        State: IValidated,
    {
        self.state.ml_parse_error()
    }

    pub fn print_ml_validate_error(&self, lang: &str)
    where
        State: IValidated,
    {
        for error in self.ml_validate_error().iter() {
            error.build(self.tyml().named_type_map()).print(
                lang,
                &self.tyml_source,
                &self.state.ml_source_code(),
            );
        }
    }

    pub fn has_ml_validate_error(&self) -> bool
    where
        State: IValidated,
    {
        !self.state.ml_validate_error().is_empty()
    }

    pub fn ml_validate_error(&self) -> &Arc<Vec<TymlValueValidateError>>
    where
        State: IValidated,
    {
        self.state.ml_validate_error()
    }
}

#[derive(Debug, Clone)]
pub struct Initial {}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub tyml: Tyml,
}

#[derive(Debug, Clone)]
pub struct Validated {
    pub tyml: Tyml,
    pub ml_source_code: SourceCode,
    pub validator: ValidatorHolder,
    pub ml_parse_error: Arc<Vec<GeneratedParseError>>,
    pub ml_validate_error: Arc<Vec<TymlValueValidateError>>,
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
    fn ml_source_code(&self) -> &SourceCode;

    fn validator(&self) -> &ValueTypeChecker;

    fn ml_parse_error(&self) -> &Arc<Vec<GeneratedParseError>>;

    fn ml_validate_error(&self) -> &Arc<Vec<TymlValueValidateError>>;
}

impl IValidated for Validated {
    fn ml_source_code(&self) -> &SourceCode {
        &self.ml_source_code
    }

    fn validator(&self) -> &ValueTypeChecker {
        self.validator.validator().as_ref()
    }

    fn ml_parse_error(&self) -> &Arc<Vec<GeneratedParseError>> {
        &self.ml_parse_error
    }

    fn ml_validate_error(&self) -> &Arc<Vec<TymlValueValidateError>> {
        &self.ml_validate_error
    }
}

/// validator contains part of tyml and ml_source_code
#[derive(Debug, Clone)]
#[allow(unused)]
pub struct ValidatorHolder {
    tyml: Tyml,
    ml_source_code: SourceCode,
    validator: Arc<ValueTypeChecker<'static, 'static, 'static, 'static, 'static, 'static>>,
}

unsafe impl Send for ValidatorHolder {}
unsafe impl Sync for ValidatorHolder {}

impl ValidatorHolder {
    pub fn new(tyml: Tyml, ml_source_code: SourceCode, validator: ValueTypeChecker) -> Self {
        Self {
            tyml,
            ml_source_code,
            validator: unsafe { transmute(Arc::new(validator)) },
        }
    }

    pub fn validator<'this>(
        &'this self,
    ) -> &'this Arc<ValueTypeChecker<'this, 'this, 'this, 'this, 'this, 'this>> {
        &self.validator
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

    pub fn comment_ranges(&self) -> &Vec<Range<usize>> {
        &self.inner.comments
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

        let comments = lexer.comments.into_iter().collect();

        let (type_tree, named_type_map, type_errors) = resolve_type(ast, allocator.deref());

        let fake_static_ast = unsafe { transmute(ast) };
        let fake_static_type_tree = unsafe { transmute(type_tree) };
        let fake_static_named_type_map = unsafe { transmute(named_type_map) };
        let fake_static_parse_errors = unsafe { transmute(parse_errors) };
        let fake_static_type_errors = unsafe { transmute(type_errors) };

        let tyml_inner = TymlInner {
            source_code,
            comments,
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
    comments: Vec<Range<usize>>,
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

    use std::{fs::File, io::Read};

    use tyml_diagnostic::message::Lang;
    use tyml_generator::registry::STYLE_REGISTRY;
    use tyml_source::SourceCode;

    use crate::{cache::get_cached_file, TymlContext};

    #[test]
    fn lib_test() {
        let source = r#"
settings: [Setting] | int
test: {
    "test\"": [int]
}

test1: {
    test2: bool
    test3: int @value 0..=200
}
test2: string @regex "\\d+"

type Setting {
    ip: string
    port: int
    mode: Mode?
}

enum Mode {
    "Debug"
    "Release"
}
"#;

        let ini_source = r#"
test."test\"" = [0xFF, ""]

test1 = { test2 = false, test3 = 200 }

test2 = "100"

[[settings]]
ip = "192.168.1.1"
port = 25565
mode = "Debug"

[[settings]]
ip = "192.168.1.6"
port = 25565
"#;

        let tyml_source = SourceCode::new("test.tyml".to_string(), source.to_string());
        let ml_source = SourceCode::new("test.toml".to_string(), ini_source.to_string());

        let tyml = TymlContext::new(tyml_source).parse();

        let language = STYLE_REGISTRY.resolve("toml").unwrap();

        let tyml = tyml.ml_parse_and_validate(&language, &ml_source, None);

        tyml.print_tyml_error(Lang::system());
        tyml.print_ml_parse_error(Lang::system());
        tyml.print_ml_validate_error(Lang::system());
    }

    #[test]
    fn cache_test() {
        let file_path = tokio::runtime::Runtime::new().unwrap().block_on(async {
            get_cached_file(
                "https://raw.githubusercontent.com/tyml-org/tyml/refs/heads/main/Cargo.toml",
            )
            .await
            .unwrap()
        });
        let mut file = File::open(file_path).unwrap();

        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();

        dbg!(content);
    }
}
