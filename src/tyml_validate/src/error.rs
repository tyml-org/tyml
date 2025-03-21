use tyml_parser::ast::Spanned;

pub enum TymlValueValidateError<Span> {
    InvalidValueType {
        expected: Spanned<String>,
        found: Span,
    },
    NoValueFound {
        required: Spanned<Vec<String>>,
        required_in_value_section: Vec<Span>,
    },
    DuplicatedValue {
        exists: Span,
        duplicated: Span,
    },
}
