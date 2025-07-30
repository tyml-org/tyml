use std::borrow::Cow;

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use either::Either;

use crate::{
    ast::{
        ArrayType, AttributeAnd, AttributeOr, BaseType, BinaryLiteral, DefaultValue, Define,
        Defines, Documents, ElementDefine, ElementInlineType, ElementType, EnumDefine, EnumElement,
        EscapedLiteral, FloatLiteral, FromTo, Function, FunctionArgument, Interface, IntoLiteral,
        JsonArray, JsonObject, JsonObjectElement, JsonValue, Literal, NamedType, NodeLiteral,
        NumericAttribute, NumericAttributeKind, OrType, Properties, Property, RegexAttribute,
        ReturnBlock, ReturnExpression, ReturnType, Spanned, StructDefine, TypeAttribute,
        TypeDefine, ValueLiteral,
    },
    error::{recover_until, Expected, ParseError, ParseErrorKind, Scope},
    lexer::{GetKind, Lexer, TokenKind},
};

pub fn parse_defines<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> &'allocator Defines<'input, 'allocator> {
    let anchor = lexer.cast_anchor();

    let mut defines = Vec::new_in(allocator);

    lexer.skip_line_feed();

    loop {
        if let TokenKind::BraceRight | TokenKind::None = lexer.current().get_kind() {
            break;
        }

        let define = match parse_define(lexer, errors, allocator) {
            Some(define) => define,
            None => {
                let error = recover_until(
                    ParseErrorKind::InvalidDefineElement,
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::Define,
                    Scope::Defines,
                    allocator,
                );
                errors.push(error);

                lexer.skip_line_feed();

                continue;
            }
        };

        defines.push(define);

        match lexer.current().get_kind() {
            TokenKind::Comma | TokenKind::LineFeed => {
                lexer.next();
                lexer.skip_line_feed();
            }
            TokenKind::BraceRight | TokenKind::None => {
                continue;
            }
            _ => {
                let error = recover_until(
                    ParseErrorKind::InvalidDefineSeparator,
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::DefineSeparator,
                    Scope::Defines,
                    allocator,
                );
                errors.push(error);

                continue;
            }
        }
    }

    allocator.alloc(Defines {
        defines,
        span: anchor.elapsed(lexer),
    })
}

fn parse_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<Define<'input, 'allocator>> {
    if let Some(element_define) = parse_element_define(lexer, errors, allocator) {
        return Some(Define::Element(element_define));
    }
    if let Some(type_define) = parse_type_define(lexer, errors, allocator) {
        return Some(Define::Type(type_define));
    }
    if let Some(interface) = parse_interface(lexer, errors, allocator) {
        return Some(Define::Interface(interface));
    }
    None
}

fn parse_documents<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    allocator: &'allocator Bump,
) -> Documents<'input, 'allocator> {
    let anchor = lexer.cast_anchor();
    let mut lines = Vec::new_in(allocator);

    loop {
        let Some(current) = lexer.current() else {
            break;
        };

        if current.kind != TokenKind::Document {
            break;
        }

        lines.push(&current.text[3..]);

        lexer.next();
    }

    Documents {
        lines,
        span: anchor.elapsed(lexer),
    }
}

fn parse_element_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ElementDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    let node = match parse_node_literal(lexer) {
        Some(literal) => literal,
        None => {
            lexer.back_to_anchor(anchor);
            return None;
        }
    };

    let current_token_kind = lexer.current().get_kind();
    if current_token_kind != TokenKind::Colon && current_token_kind != TokenKind::Equal {
        let error = recover_until(
            ParseErrorKind::NotFoundElementTypeAndDefaultValue,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma],
            Expected::TypeOrValue,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        return None;
    }

    let inline_type = parse_element_inline_type(lexer, errors, allocator);

    let (ty, default) = if inline_type.is_none() {
        (
            parse_element_type(lexer, errors, allocator),
            parse_default_value(lexer, errors, allocator),
        )
    } else {
        (None, None)
    };

    Some(ElementDefine {
        documents,
        node,
        inline_type,
        ty,
        default,
        span: anchor.elapsed(lexer),
    })
}

fn parse_node_literal<'input>(lexer: &mut Lexer<'input>) -> Option<NodeLiteral<'input>> {
    let token = lexer.current();

    match token.get_kind() {
        TokenKind::Literal => Some(NodeLiteral::Literal(
            lexer.next().unwrap().into_literal().map(|text| text.into()),
        )),
        TokenKind::StringLiteral => Some(match token.unwrap().text.chars().next().unwrap() {
            '"' => NodeLiteral::Literal(escape_literal(
                lexer
                    .next()
                    .unwrap()
                    .into_literal()
                    .map(|text| &text[1..(text.len() - 1)]),
            )),
            '\'' => {
                let token = lexer.next().unwrap();
                NodeLiteral::Literal(EscapedLiteral::new(
                    token.text[1..token.text.len() - 1].into(),
                    token.span,
                ))
            }
            _ => unreachable!(),
        }),
        TokenKind::Asterisk => Some(NodeLiteral::Asterisk(lexer.next().unwrap().into_literal())),
        _ => None,
    }
}

fn escape_literal<'input>(literal: Literal<'input>) -> EscapedLiteral<'input> {
    let input = literal.value;
    let span = literal.span();

    if !input.contains('\\') {
        return Spanned::new(Cow::Borrowed(input), span);
    }

    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }

        match chars.next() {
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('t') => out.push('\t'),
            Some('\\') => out.push('\\'),
            Some('\'') => out.push('\''),
            Some('"') => out.push('"'),

            // \xNN
            Some('x') => {
                let hi = chars.next();
                let lo = chars.next();
                if let (Some(hi), Some(lo)) = (
                    hi.and_then(|c| c.to_digit(16)),
                    lo.and_then(|c| c.to_digit(16)),
                ) {
                    out.push(char::from_u32((hi * 16 + lo) as u32).unwrap());
                } else {
                    // unknown format
                    out.push_str("\\x");
                    if let Some(h) = hi {
                        out.push(h);
                    }
                    if let Some(l) = lo {
                        out.push(l);
                    }
                }
            }

            // \u{...}
            Some('u') if matches!(chars.peek(), Some('{')) => {
                chars.next(); // '{' を消費
                let mut buf = String::new();
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '}' {
                        break;
                    }
                    buf.push(ch);
                }
                if let Ok(code) = u32::from_str_radix(&buf, 16) {
                    if let Some(ch) = char::from_u32(code) {
                        out.push(ch);
                    }
                }
            }

            Some(other) => out.push(other),
            None => {}
        }
    }

    Spanned::new(Cow::Owned(out), span)
}

fn parse_element_inline_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ElementInlineType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Colon
        && lexer.current().get_kind() != TokenKind::Equal
    {
        return None;
    }
    lexer.next();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        if let TokenKind::Literal
        | TokenKind::BracketLeft
        | TokenKind::StringLiteral
        | TokenKind::FloatNumeric
        | TokenKind::BinaryNumeric
        | TokenKind::Null = lexer.current().get_kind()
        {
            // This should be a ElementType or DefaultValue,
            // so return to parent layer and try to parse it as ElementType or DefaultValue.
            lexer.back_to_anchor(anchor);
            return None;
        }

        let error = recover_until(
            ParseErrorKind::InvalidElementTypeFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma],
            Expected::StructElementBlockOrTypeName,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        return None;
    }
    lexer.next();

    let defines = parse_defines(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBrace,
            lexer,
            &[],
            Expected::BraceRight,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);
    }
    lexer.next();

    Some(ElementInlineType {
        defines,
        span: anchor.elapsed(lexer),
    })
}

fn parse_element_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ElementType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Colon {
        return None;
    }
    lexer.next();

    let Some(type_info) = parse_or_type(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::InvalidElementTypeFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma],
            Expected::Type,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        return None;
    };

    Some(ElementType {
        type_info,
        span: anchor.elapsed(lexer),
    })
}

fn parse_or_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<OrType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let first_type = parse_base_type(lexer, errors, allocator);

    let Some(first_type) = first_type else {
        return None;
    };

    let mut or_types = Vec::with_capacity_in(1, allocator);
    or_types.push(first_type);

    loop {
        if lexer.current().get_kind() != TokenKind::VerticalLine {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();

        let ty = parse_base_type(lexer, errors, allocator);

        let Some(ty) = ty else {
            let error = recover_until(
                ParseErrorKind::InvalidOrTypeFormat,
                lexer,
                &[
                    TokenKind::LineFeed,
                    TokenKind::Comma,
                    TokenKind::VerticalLine,
                ],
                Expected::Type,
                Scope::ElementDefine,
                allocator,
            );
            errors.push(error);

            if lexer.current().get_kind() == TokenKind::VerticalLine {
                continue;
            } else {
                break;
            }
        };

        or_types.push(ty);
    }

    Some(OrType {
        or_types,
        span: anchor.elapsed(lexer),
    })
}

fn parse_base_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<BaseType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let ty = parse_named_type(lexer)
        .map(|base_type| Either::Left(base_type))
        .or_else(|| {
            parse_array_type(lexer, errors, allocator).map(|array_type| Either::Right(array_type))
        });

    let Some(ty) = ty else { return None };

    let optional = match lexer.current().get_kind() {
        TokenKind::QuestionMark => Some(lexer.next().unwrap().span),
        _ => None,
    };

    let attribute = parse_attribute_or(lexer, errors, allocator);

    Some(BaseType {
        ty,
        optional,
        attribute,
        span: anchor.elapsed(lexer),
    })
}

fn parse_attribute_or<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<AttributeOr<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_attribute_and(lexer, errors, allocator) else {
        return None;
    };

    let mut attributes = Vec::new_in(allocator);
    attributes.push(first);

    let mut or_spans = Vec::new_in(allocator);

    loop {
        if lexer.current().get_kind() != TokenKind::Or {
            break;
        }
        let or = lexer.next().unwrap();
        or_spans.push(or.span);

        let Some(next) = parse_attribute_and(lexer, errors, allocator) else {
            let error = recover_until(
                ParseErrorKind::InvalidAndOrAttributeFormat,
                lexer,
                &[TokenKind::LineFeed],
                Expected::TypeAttribute,
                Scope::TypeAttribute,
                allocator,
            );
            errors.push(error);

            break;
        };
        attributes.push(next);
    }

    Some(AttributeOr {
        attributes,
        or_spans,
        span: anchor.elapsed(lexer),
    })
}

fn parse_attribute_and<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<AttributeAnd<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_type_attribute(lexer, errors, allocator) else {
        return None;
    };

    let mut attributes = Vec::new_in(allocator);
    attributes.push(first);

    let mut and_spans = Vec::new_in(allocator);

    loop {
        if lexer.current().get_kind() != TokenKind::And {
            break;
        }
        let and = lexer.next().unwrap();
        and_spans.push(and.span);

        let Some(next) = parse_type_attribute(lexer, errors, allocator) else {
            let error = recover_until(
                ParseErrorKind::InvalidAndOrAttributeFormat,
                lexer,
                &[TokenKind::LineFeed, TokenKind::Comma],
                Expected::TypeAttribute,
                Scope::TypeAttribute,
                allocator,
            );
            errors.push(error);

            break;
        };
        attributes.push(next);
    }

    Some(AttributeAnd {
        attributes,
        and_spans,
        span: anchor.elapsed(lexer),
    })
}

fn parse_type_attribute<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<TypeAttribute<'input, 'allocator>> {
    if let Some(attribute) = parse_numeric_attribute(lexer, errors, allocator) {
        return Some(TypeAttribute::NumericAttribute(attribute));
    }
    if let Some(attribute) = parse_regex_attribute(lexer, errors, allocator) {
        return Some(TypeAttribute::RegexAttribute(attribute));
    }
    if let Some(attribute) = parse_type_attribute_recursive(lexer, errors, allocator) {
        return Some(TypeAttribute::AttributeTree(attribute));
    }

    None
}

fn parse_type_attribute_recursive<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<AttributeOr<'input, 'allocator>> {
    if lexer.current().get_kind() != TokenKind::ParenthesisLeft {
        return None;
    }
    lexer.next();

    let Some(attribute) = parse_attribute_or(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::NonTypeAttribute,
            lexer,
            &[TokenKind::LineFeed, TokenKind::ParenthesisRight],
            Expected::TypeAttribute,
            Scope::TypeAttribute,
            allocator,
        );
        errors.push(error);

        return None;
    };

    if lexer.current().get_kind() == TokenKind::ParenthesisRight {
        lexer.next();
    } else {
        let error = recover_until(
            ParseErrorKind::NonClosedParenthesis,
            lexer,
            &[TokenKind::LineFeed, TokenKind::ParenthesisRight],
            Expected::ParenthesisRight,
            Scope::TypeAttribute,
            allocator,
        );
        errors.push(error);

        if lexer.current().get_kind() == TokenKind::ParenthesisRight {
            lexer.next();
        }

        return Some(attribute);
    }

    Some(attribute)
}

fn parse_numeric_attribute<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<NumericAttribute> {
    let anchor = lexer.cast_anchor();

    let kind = match lexer.current().get_kind() {
        TokenKind::AtValue => Spanned::new(NumericAttributeKind::Value, lexer.next().unwrap().span),
        TokenKind::AtLength => {
            Spanned::new(NumericAttributeKind::Length, lexer.next().unwrap().span)
        }
        TokenKind::AtU8Size => {
            Spanned::new(NumericAttributeKind::U8Size, lexer.next().unwrap().span)
        }
        _ => return None,
    };

    let from_to_anchor = lexer.cast_anchor();

    let from = match lexer.current().get_kind() {
        TokenKind::FloatNumeric => Some(lexer.next().unwrap()),
        _ => None,
    };

    let from_to_kind = match lexer.current().get_kind() {
        TokenKind::FromTo | TokenKind::FromToInclusive | TokenKind::FromToExclusive => {
            lexer.next().get_kind()
        }
        _ => {
            let error = recover_until(
                ParseErrorKind::NonFromTo,
                lexer,
                &[TokenKind::LineFeed, TokenKind::VerticalLine],
                Expected::FromTo,
                Scope::TypeAttribute,
                allocator,
            );
            errors.push(error);
            return None;
        }
    };

    let to = match lexer.current().get_kind() {
        TokenKind::FloatNumeric => Some(lexer.next().unwrap()),
        _ => None,
    };

    let from_to_span = from_to_anchor.elapsed(lexer);

    let from_to = match from_to_kind {
        TokenKind::FromTo => {
            let Some(from) = from else {
                let error = ParseError {
                    kind: ParseErrorKind::InvalidFromToFormat,
                    scope: Scope::TypeAttribute,
                    expected: Expected::NumericLiteral,
                    error_tokens: Vec::new_in(allocator),
                    span: from_to_span,
                };
                errors.push(error);

                return None;
            };

            let from_span = from.span.clone();

            let Ok(from) = from
                .text
                .parse::<i128>()
                .map(|int| Either::Right(int))
                .or(from.text.parse::<f64>().map(|float| Either::Left(float)))
            else {
                let error = ParseError {
                    kind: ParseErrorKind::NonNumeric,
                    scope: Scope::TypeAttribute,
                    expected: Expected::NumericLiteral,
                    error_tokens: Vec::new_in(allocator),
                    span: from.span,
                };
                errors.push(error);
                return None;
            };

            let from = Spanned::new(from, from_span);

            if let Some(_) = to {
                let error = ParseError {
                    kind: ParseErrorKind::InvalidFromToFormat,
                    scope: Scope::TypeAttribute,
                    expected: Expected::Unnecessary,
                    error_tokens: Vec::new_in(allocator),
                    span: from_to_span,
                };
                errors.push(error);
                return None;
            }

            FromTo::From { from }
        }
        TokenKind::FromToExclusive | TokenKind::FromToInclusive => {
            let Some(to) = to else {
                let error = ParseError {
                    kind: ParseErrorKind::InvalidFromToFormat,
                    scope: Scope::TypeAttribute,
                    expected: Expected::NumericLiteral,
                    error_tokens: Vec::new_in(allocator),
                    span: from_to_span,
                };
                errors.push(error);

                return None;
            };

            let to_span = to.span.clone();

            let Ok(to) = to
                .text
                .parse::<i128>()
                .map(|int| Either::Right(int))
                .or(to.text.parse::<f64>().map(|float| Either::Left(float)))
            else {
                let error = ParseError {
                    kind: ParseErrorKind::NonNumeric,
                    scope: Scope::TypeAttribute,
                    expected: Expected::NumericLiteral,
                    error_tokens: Vec::new_in(allocator),
                    span: to.span,
                };
                errors.push(error);
                return None;
            };

            let to = Spanned::new(to, to_span);

            match from {
                Some(from) => {
                    let from_span = from.span;

                    let Ok(from) = from
                        .text
                        .parse::<i128>()
                        .map(|int| Either::Right(int))
                        .or(from.text.parse::<f64>().map(|float| Either::Left(float)))
                    else {
                        let error = ParseError {
                            kind: ParseErrorKind::NonNumeric,
                            scope: Scope::TypeAttribute,
                            expected: Expected::NumericLiteral,
                            error_tokens: Vec::new_in(allocator),
                            span: from_to_span,
                        };
                        errors.push(error);
                        return None;
                    };

                    let bigger = match (from, to.value) {
                        (Either::Left(from), Either::Left(to)) => from > to,
                        (Either::Left(from), Either::Right(to)) => from > to as f64,
                        (Either::Right(from), Either::Left(to)) => from as f64 > to,
                        (Either::Right(from), Either::Right(to)) => from > to,
                    };

                    if bigger {
                        let error = ParseError {
                            kind: ParseErrorKind::BiggerFrom,
                            scope: Scope::TypeAttribute,
                            expected: Expected::SmallerNumericLiteral,
                            error_tokens: Vec::new_in(allocator),
                            span: from_to_span,
                        };
                        errors.push(error);
                        return None;
                    }

                    let from = Spanned::new(from, from_span);

                    match from_to_kind {
                        TokenKind::FromToExclusive => FromTo::FromToExclusive { from, to },
                        TokenKind::FromToInclusive => FromTo::FromToInclusive { from, to },
                        _ => unreachable!(),
                    }
                }
                None => match from_to_kind {
                    TokenKind::FromToExclusive => FromTo::ToExclusive { to },
                    TokenKind::FromToInclusive => FromTo::ToInclusive { to },
                    _ => unreachable!(),
                },
            }
        }
        _ => unreachable!(),
    };

    Some(NumericAttribute {
        kind,
        from_to,
        span: anchor.elapsed(lexer),
    })
}

fn parse_regex_attribute<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<RegexAttribute<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::AtRegex {
        return None;
    }
    let regex_keyword_span = lexer.next().unwrap().span;

    let regex_literal = match lexer.current().get_kind() {
        TokenKind::StringLiteral => match lexer.current().unwrap().text.chars().next().unwrap() {
            '"' => escape_literal(
                lexer
                    .next()
                    .unwrap()
                    .into_literal()
                    .map(|text| &text[1..text.len() - 1]),
            ),
            '\'' => {
                let token = lexer.next().unwrap();
                EscapedLiteral::new(token.text[1..token.text.len() - 1].into(), token.span)
            }
            _ => unreachable!(),
        },
        _ => {
            let error = recover_until(
                ParseErrorKind::InvalidRegexAttributeFormat,
                lexer,
                &[TokenKind::LineFeed, TokenKind::VerticalLine],
                Expected::StringLiteral,
                Scope::TypeAttribute,
                allocator,
            );
            errors.push(error);

            return None;
        }
    };

    Some(RegexAttribute {
        regex_keyword_span,
        regex_literal,
        span: anchor.elapsed(lexer),
    })
}

fn parse_array_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ArrayType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BracketLeft {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let base = match parse_or_type(lexer, errors, allocator) {
        Some(ty) => ty,
        None => {
            let error = recover_until(
                ParseErrorKind::NotFoundArrayBaseType,
                lexer,
                &[
                    TokenKind::LineFeed,
                    TokenKind::Comma,
                    TokenKind::BracketRight,
                ],
                Expected::Type,
                Scope::ElementDefine,
                allocator,
            );
            errors.push(error);

            if lexer.current().get_kind() == TokenKind::BracketRight {
                lexer.next();
            }

            return None;
        }
    };

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BracketRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBracket,
            lexer,
            &[
                TokenKind::LineFeed,
                TokenKind::Comma,
                TokenKind::BracketRight,
            ],
            Expected::BracketRight,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        if lexer.current().get_kind() != TokenKind::BracketRight {
            return None;
        }
    }
    lexer.next();

    Some(ArrayType {
        base,
        span: anchor.elapsed(lexer),
    })
}

fn parse_named_type<'input, 'allocator>(lexer: &mut Lexer<'input>) -> Option<NamedType<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Literal {
        return None;
    }

    let name = lexer.next().unwrap().into_literal();

    Some(NamedType {
        name,
        span: anchor.elapsed(lexer),
    })
}

fn parse_default_value<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<DefaultValue<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Equal {
        return None;
    }
    lexer.next();

    let Some(value) = parse_value_literal(lexer) else {
        let error = recover_until(
            ParseErrorKind::UnknownDefaultValueFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma],
            Expected::Value,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        return None;
    };

    Some(DefaultValue {
        value,
        span: anchor.elapsed(lexer),
    })
}

fn parse_value_literal<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
) -> Option<ValueLiteral<'input>> {
    let value = match lexer.current().get_kind() {
        TokenKind::FloatNumeric => {
            ValueLiteral::Float(FloatLiteral::Float(lexer.next().unwrap().into_literal()))
        }
        TokenKind::Inf => {
            ValueLiteral::Float(FloatLiteral::Inf(lexer.next().unwrap().into_literal()))
        }
        TokenKind::Nan => {
            ValueLiteral::Float(FloatLiteral::Nan(lexer.next().unwrap().into_literal()))
        }
        TokenKind::BinaryNumeric => {
            let current_text = lexer.current().unwrap().text;

            let prefix = match current_text.chars().next() {
                Some('+') | Some('-') => &current_text[1..3],
                _ => &current_text[0..2],
            };

            let binary_literal = match &prefix[0..2] {
                "0x" => BinaryLiteral::Hex(lexer.next().unwrap().into_literal()),
                "0o" => BinaryLiteral::Oct(lexer.next().unwrap().into_literal()),
                "0b" => BinaryLiteral::Bin(lexer.next().unwrap().into_literal()),
                _ => unreachable!(),
            };

            ValueLiteral::Binary(binary_literal)
        }
        TokenKind::StringLiteral => ValueLiteral::String(lexer.next().unwrap().into_literal()),
        TokenKind::True => ValueLiteral::Bool(lexer.next().unwrap().into_literal()),
        TokenKind::False => ValueLiteral::Bool(lexer.next().unwrap().into_literal()),
        TokenKind::Null => ValueLiteral::Null(lexer.next().unwrap().into_literal()),
        _ => return None,
    };
    return Some(value);
}

fn parse_type_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<TypeDefine<'input, 'allocator>> {
    if let Some(struct_define) = parse_struct_define(lexer, errors, allocator) {
        return Some(TypeDefine::Struct(struct_define));
    }
    if let Some(enum_define) = parse_enum_define(lexer, errors, allocator) {
        return Some(TypeDefine::Enum(enum_define));
    }
    None
}

fn parse_struct_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<StructDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    if lexer.current().get_kind() != TokenKind::Type {
        lexer.back_to_anchor(anchor);
        return None;
    }
    let keyword = lexer.next().unwrap();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => lexer.next().unwrap().into_literal(),
        _ => {
            let error = recover_until(
                ParseErrorKind::NotFoundStructName,
                lexer,
                &[TokenKind::LineFeed, TokenKind::Comma],
                Expected::StructName,
                Scope::StructDefine,
                allocator,
            );
            errors.push(error);

            return None;
        }
    };

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        let error = recover_until(
            ParseErrorKind::NotFoundStructBlock,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceLeft],
            Expected::StructElementBlockOrTypeName,
            Scope::StructDefine,
            allocator,
        );
        errors.push(error);

        if lexer.current().get_kind() != TokenKind::BraceLeft {
            // cannot recover as StructDefine
            return None;
        }
    }
    lexer.next();

    let defines = parse_defines(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBrace,
            lexer,
            &[],
            Expected::BraceRight,
            Scope::StructDefine,
            allocator,
        );
        errors.push(error);
    }
    lexer.next();

    Some(StructDefine {
        documents,
        keyword_span: keyword.span,
        name,
        defines,
        span: anchor.elapsed(lexer),
    })
}

fn parse_enum_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<EnumDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    if lexer.current().get_kind() != TokenKind::Enum {
        lexer.back_to_anchor(anchor);
        return None;
    }
    let keyword = lexer.next().unwrap();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => lexer.next().unwrap().into_literal(),
        _ => {
            let error = recover_until(
                ParseErrorKind::NotFoundEnumName,
                lexer,
                &[TokenKind::LineFeed, TokenKind::Comma],
                Expected::EnumName,
                Scope::EnumDefine,
                allocator,
            );
            errors.push(error);

            return None;
        }
    };

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        let error = recover_until(
            ParseErrorKind::NotFoundEnumBlock,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceLeft],
            Expected::EnumElementBlock,
            Scope::EnumDefine,
            allocator,
        );
        errors.push(error);

        if lexer.current().get_kind() != TokenKind::BraceLeft {
            // cannot recover as EnumDefine
            return None;
        }
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut elements = Vec::new_in(allocator);

    loop {
        if let TokenKind::BraceRight | TokenKind::None = lexer.current().get_kind() {
            break;
        }

        let element_anchor = lexer.cast_anchor();

        let documents = parse_documents(lexer, allocator);

        let literal = match lexer.current().get_kind() {
            TokenKind::StringLiteral => lexer.next().unwrap().into_literal(),
            _ => {
                let error = recover_until(
                    ParseErrorKind::InvalidEnumElement,
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::EnumElement,
                    Scope::EnumDefine,
                    allocator,
                );
                errors.push(error);

                if let TokenKind::LineFeed | TokenKind::Comma = lexer.current().get_kind() {
                    lexer.next();
                }

                continue;
            }
        };

        let literal_text = literal.value;
        let escaped = match literal_text.chars().next().unwrap() {
            '"' => {
                escape_literal(Spanned::new(
                    &literal_text[1..(literal_text.len() - 1)],
                    literal.span.clone(),
                ))
                .value
            }
            '\'' => literal_text[1..(literal_text.len() - 1)].into(),
            _ => unreachable!(),
        };

        elements.push(EnumElement {
            documents,
            literal,
            literal_value: escaped,
            span: element_anchor.elapsed(lexer),
        });

        match lexer.current().get_kind() {
            TokenKind::Comma | TokenKind::LineFeed => {
                lexer.next();
                lexer.skip_line_feed();
            }
            TokenKind::BraceRight | TokenKind::None => {
                continue;
            }
            _ => {
                let error = recover_until(
                    ParseErrorKind::InvalidEnumElementSeparator,
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::EnumElementSeparator,
                    Scope::EnumDefine,
                    allocator,
                );
                errors.push(error);

                continue;
            }
        }
    }

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBrace,
            lexer,
            &[],
            Expected::BraceRight,
            Scope::EnumDefine,
            allocator,
        );
        errors.push(error);
    }
    lexer.next();

    Some(EnumDefine {
        documents,
        keyword_span: keyword.span,
        name,
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_interface<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<Interface<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    let properties = parse_properties(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::Interface {
        lexer.back_to_anchor(anchor);
        return None;
    }
    let keyword_span = lexer.next().unwrap().span;

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => lexer.next().unwrap().into_literal(),
        _ => {
            let error = recover_until(
                ParseErrorKind::InvalidInterfaceFormat,
                lexer,
                &[TokenKind::LineFeed],
                Expected::InterfaceName,
                Scope::Interface,
                allocator,
            );
            errors.push(error);
            return None;
        }
    };

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        let error = recover_until(
            ParseErrorKind::InvalidInterfaceFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::BraceLeft,
            Scope::Interface,
            allocator,
        );
        errors.push(error);
        return None;
    }
    lexer.next();

    let mut functions = Vec::new_in(allocator);
    loop {
        let Some(function) = parse_function(lexer, errors, allocator) else {
            match lexer.current().get_kind() {
                TokenKind::BraceRight | TokenKind::None => break,
                TokenKind::LineFeed => {
                    lexer.skip_line_feed();
                    continue;
                }
                _ => {
                    let error = recover_until(
                        ParseErrorKind::NonLineFeed,
                        lexer,
                        &[TokenKind::LineFeed],
                        Expected::LineFeed,
                        Scope::Interface,
                        allocator,
                    );
                    errors.push(error);
                    lexer.skip_line_feed();
                    continue;
                }
            }
        };
        functions.push(function);
    }

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBrace,
            lexer,
            &[TokenKind::BraceRight],
            Expected::BraceRight,
            Scope::Interface,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(Interface {
        documents,
        properties,
        name,
        keyword_span,
        functions,
        span: anchor.elapsed(lexer),
    })
}

fn parse_function<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<Function<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let documents = parse_documents(lexer, allocator);

    let properties = parse_properties(lexer, errors, allocator);

    if lexer.current().get_kind() != TokenKind::Function {
        lexer.back_to_anchor(anchor);
        return None;
    }
    let keyword_span = lexer.next().unwrap().span;

    let Some(name) = parse_literal(lexer) else {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::FunctionName,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return None;
    };

    if lexer.current().get_kind() != TokenKind::ParenthesisLeft {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::ParenthesisLeft,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return Some(Function {
            documents,
            properties,
            keyword_span,
            name,
            arguments: Vec::new_in(allocator),
            return_type: None,
            return_block: None,
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut arguments = Vec::new_in(allocator);
    loop {
        let Some(argument) = parse_function_argument(lexer, errors, allocator) else {
            break;
        };
        arguments.push(argument);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();
    }

    if lexer.current().get_kind() != TokenKind::ParenthesisRight {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::ParenthesisRight],
            Expected::ParenthesisRight,
            Scope::Function,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::ParenthesisRight {
        lexer.next();
    } else {
        // failed to recover
        return Some(Function {
            documents,
            properties,
            keyword_span,
            name,
            arguments,
            return_type: None,
            return_block: None,
            span: anchor.elapsed(lexer),
        });
    }

    let return_type = parse_return_type(lexer, errors, allocator);

    let return_block = parse_return_block(lexer, errors, allocator);

    Some(Function {
        documents,
        properties,
        keyword_span,
        name,
        arguments,
        return_type,
        return_block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_return_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ReturnType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Arrow {
        return None;
    }
    lexer.next();

    let Some(type_info) = parse_or_type(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::BraceLeft],
            Expected::Type,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return None;
    };

    Some(ReturnType {
        type_info,
        span: anchor.elapsed(lexer),
    })
}

fn parse_return_block<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ReturnBlock<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let return_expr_anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Return {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::BraceRight],
            Expected::Return,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return None;
    }
    let keyword_span = lexer.next().unwrap().span;

    let Some(value) = parse_json_value(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::BraceRight],
            Expected::Value,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return None;
    };

    let return_expr_span = return_expr_anchor.elapsed(lexer);

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::NonClosedBrace,
            lexer,
            &[TokenKind::LineFeed, TokenKind::BraceRight],
            Expected::BraceRight,
            Scope::Function,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(ReturnBlock {
        return_expression: ReturnExpression {
            keyword_span,
            value,
            span: return_expr_span,
        },
        span: anchor.elapsed(lexer),
    })
}

fn parse_function_argument<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<FunctionArgument<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let properties = parse_properties(lexer, errors, allocator);

    let Some(name) = parse_literal(lexer) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    let Some(ty) = parse_element_type(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::InvalidFunctionFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::ParenthesisRight],
            Expected::Type,
            Scope::Function,
            allocator,
        );
        errors.push(error);
        return None;
    };

    let mut default_value = None;

    if lexer.current().get_kind() == TokenKind::Equal {
        lexer.next();

        default_value = parse_json_value(lexer, errors, allocator);
    }

    Some(FunctionArgument {
        properties,
        name,
        ty,
        default_value,
        span: anchor.elapsed(lexer),
    })
}

fn parse_json_value<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<JsonValue<'input, 'allocator>> {
    if let Some(value) = parse_value_literal(lexer) {
        return Some(JsonValue::Value(value));
    }
    if let Some(array) = parse_json_array(lexer, errors, allocator) {
        return Some(JsonValue::Array(array));
    }
    if let Some(object) = parse_json_object(lexer, errors, allocator) {
        return Some(JsonValue::Object(object));
    }
    None
}

fn parse_json_array<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<JsonArray<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BracketLeft {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut elements = Vec::new_in(allocator);
    loop {
        let Some(element) = parse_json_value(lexer, errors, allocator) else {
            break;
        };
        elements.push(element);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();
    }

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BracketRight {
        let error = recover_until(
            ParseErrorKind::InvalidJsonArrayFormat,
            lexer,
            &[TokenKind::BracketRight],
            Expected::BracketRight,
            Scope::Json,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BracketRight {
        lexer.next();
    }

    Some(JsonArray {
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_json_object<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<JsonObject<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    let mut elements = Vec::new_in(allocator);
    loop {
        let Some(element) = parse_json_object_element(lexer, errors, allocator) else {
            break;
        };
        elements.push(element);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();

        lexer.skip_line_feed();
    }

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BraceRight {
        let error = recover_until(
            ParseErrorKind::InvalidJsonObjectFormat,
            lexer,
            &[TokenKind::BraceRight],
            Expected::BraceRight,
            Scope::Json,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(JsonObject {
        elements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_json_object_element<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<JsonObjectElement<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let Some(name) = parse_literal(lexer) else {
        return None;
    };

    if lexer.current().get_kind() != TokenKind::Equal {
        let error = recover_until(
            ParseErrorKind::InvalidJsonObjectFormat,
            lexer,
            &[TokenKind::Comma, TokenKind::BraceRight],
            Expected::Equal,
            Scope::Json,
            allocator,
        );
        errors.push(error);
        return None;
    }
    lexer.next();

    let Some(value) = parse_json_value(lexer, errors, allocator) else {
        let error = recover_until(
            ParseErrorKind::InvalidJsonObjectFormat,
            lexer,
            &[TokenKind::Comma, TokenKind::BraceRight],
            Expected::Value,
            Scope::Json,
            allocator,
        );
        errors.push(error);
        return None;
    };

    Some(JsonObjectElement {
        name,
        value,
        span: anchor.elapsed(lexer),
    })
}

fn parse_properties<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Properties<'input, 'allocator> {
    let anchor = lexer.cast_anchor();

    let mut elements = Vec::new_in(allocator);

    loop {
        let Some(property) = parse_property(lexer, errors, allocator) else {
            break;
        };
        elements.push(property);

        lexer.skip_line_feed();
    }

    Properties {
        elements,
        span: anchor.elapsed(lexer),
    }
}

fn parse_property<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<Property<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Hash {
        return None;
    }
    lexer.next();

    if lexer.current().get_kind() != TokenKind::BracketLeft {
        let error = recover_until(
            ParseErrorKind::InvalidPropertyFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::BracketLeft,
            Scope::Property,
            allocator,
        );
        errors.push(error);
        return None;
    }
    lexer.next();

    let Some(name) = parse_literal(lexer) else {
        let error = recover_until(
            ParseErrorKind::InvalidPropertyFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::PropertyName,
            Scope::Property,
            allocator,
        );
        errors.push(error);
        return None;
    };

    if lexer.current().get_kind() != TokenKind::Equal {
        let error = recover_until(
            ParseErrorKind::InvalidPropertyFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::Equal,
            Scope::Property,
            allocator,
        );
        errors.push(error);
        return Some(Property {
            name,
            values: Vec::new_in(allocator),
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let mut values = Vec::new_in(allocator);
    loop {
        let Some(value) = parse_value_literal(lexer) else {
            break;
        };
        values.push(value);
    }

    if lexer.current().get_kind() != TokenKind::BracketRight {
        let error = recover_until(
            ParseErrorKind::InvalidPropertyFormat,
            lexer,
            &[TokenKind::LineFeed],
            Expected::BracketRight,
            Scope::Property,
            allocator,
        );
        errors.push(error);
    }

    if lexer.current().get_kind() == TokenKind::BracketRight {
        lexer.next();
    }

    Some(Property {
        name,
        values,
        span: anchor.elapsed(lexer),
    })
}

fn parse_literal<'input>(lexer: &mut Lexer<'input>) -> Option<EscapedLiteral<'input>> {
    let token = lexer.current();

    match token.get_kind() {
        TokenKind::Literal => Some(lexer.next().unwrap().into_literal().map(|text| text.into())),
        TokenKind::StringLiteral => Some(match token.unwrap().text.chars().next().unwrap() {
            '"' => escape_literal(
                lexer
                    .next()
                    .unwrap()
                    .into_literal()
                    .map(|text| &text[1..(text.len() - 1)]),
            ),
            '\'' => {
                let token = lexer.next().unwrap();
                EscapedLiteral::new(token.text[1..token.text.len() - 1].into(), token.span)
            }
            _ => unreachable!(),
        }),
        _ => None,
    }
}
