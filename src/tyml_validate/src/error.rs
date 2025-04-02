use tyml_parser::ast::Spanned;

pub enum TymlValueValidateError<Span> {
    NotTreeValue {
        found: Span,
        path: String,
    },
    NoValueFound {
        required: Spanned<String>,
        required_in: Vec<Span>,
    },
    DuplicatedValue {
        exists: Vec<Span>,
        duplicated: Span,
    },
    UnknownValue {
        values: Vec<Span>,
        path: String,
    },
    InvalidValue {
        found: Vec<Span>,
        expected: Spanned<String>,
    },
    NotArrayValue {
        found: Vec<Span>,
        expected: Spanned<String>,
    },
}
