use allocator_api2::vec::Vec;
use bumpalo::Bump;
use either::Either;

use crate::{
    ast::{
        ArrayType, BaseType, BinaryLiteral, DefaultValue, Define, Defines, ElementDefine,
        ElementInlineType, ElementType, EnumDefine, FloatLiteral, IntoLiteral, NodeLiteral, OrType,
        StructDefine, TypeDefine, ValueLiteral,
    },
    error::{recover_until, Expected, ParseError, ParseErrorKind, Scope},
    lexer::{GetKind, Lexer, TokenKind},
};

pub(crate) fn parse_defines<'input, 'allocator>(
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
    None
}

fn parse_element_define<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ElementDefine<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    let first_node_literal = match parse_node_literal(lexer) {
        Some(literal) => literal,
        None => return None,
    };

    let mut node = Vec::new_in(allocator);
    node.push(first_node_literal);

    loop {
        if lexer.current().get_kind() != TokenKind::Period {
            break;
        }
        lexer.next();

        let node_literal = match parse_node_literal(lexer) {
            Some(literal) => literal,
            None => {
                let error = recover_until(
                    ParseErrorKind::NotFoundNodeLiteralAfterPeriod,
                    lexer,
                    &[
                        TokenKind::LineFeed,
                        TokenKind::Comma,
                        TokenKind::Colon,
                        TokenKind::Equal,
                    ],
                    Expected::NodeLiteral,
                    Scope::ElementDefine,
                    allocator,
                );
                errors.push(error);

                if let TokenKind::Colon | TokenKind::Equal = lexer.current().get_kind() {
                    break;
                } else {
                    return None;
                }
            }
        };
        node.push(node_literal);
    }

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
        TokenKind::Literal => Some(NodeLiteral::Literal(lexer.next().unwrap().into_literal())),
        TokenKind::Asterisk => Some(NodeLiteral::Asterisk(lexer.next().unwrap().into_literal())),
        _ => None,
    }
}

fn parse_element_inline_type<'input, 'allocator>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError<'input, 'allocator>, &'allocator Bump>,
    allocator: &'allocator Bump,
) -> Option<ElementInlineType<'input, 'allocator>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Colon {
        return None;
    }
    lexer.next();

    lexer.skip_line_feed();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        if let TokenKind::Literal | TokenKind::BracketLeft = lexer.current().get_kind() {
            // This should be a ElementType, so return to parent layer and try to parse as ElementType.
            lexer.back_to_anchor(anchor);
            return None;
        }

        let error = recover_until(
            ParseErrorKind::InvalidElementTypeFormat,
            lexer,
            &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceLeft],
            Expected::StructElementBlockOrTypeName,
            Scope::ElementDefine,
            allocator,
        );
        errors.push(error);

        if lexer.current().get_kind() != TokenKind::BraceLeft {
            // cannot recover as ElementInlineType
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

    let first_type = parse_base_type(lexer)
        .map(|base_type| Either::Left(base_type))
        .or_else(|| {
            parse_array_type(lexer, errors, allocator).map(|array_type| Either::Right(array_type))
        });

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

        let ty = parse_base_type(lexer)
            .map(|base_type| Either::Left(base_type))
            .or_else(|| {
                parse_array_type(lexer, errors, allocator)
                    .map(|array_type| Either::Right(array_type))
            });

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

fn parse_base_type<'input, 'allocator>(lexer: &mut Lexer<'input>) -> Option<BaseType<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Literal {
        return None;
    }

    let name = lexer.next().unwrap().into_literal();

    let optional = match lexer.current().get_kind() {
        TokenKind::QuestionMark => Some(lexer.next().unwrap().span),
        _ => None,
    };

    Some(BaseType {
        name,
        optional,
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
            let binary_literal = match &lexer.current().unwrap().text[0..2] {
                "0x" => BinaryLiteral::Hex(lexer.next().unwrap().into_literal()),
                "0o" => BinaryLiteral::Oct(lexer.next().unwrap().into_literal()),
                "0b" => BinaryLiteral::Bin(lexer.next().unwrap().into_literal()),
                _ => unreachable!(),
            };

            ValueLiteral::Binary(binary_literal)
        }
        TokenKind::StringLiteral => ValueLiteral::String(lexer.next().unwrap().into_literal()),
        TokenKind::Null => ValueLiteral::Null(lexer.next().unwrap().into_literal()),
        _ => {
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
        }
    };

    Some(DefaultValue {
        value,
        span: anchor.elapsed(lexer),
    })
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

    if lexer.current().get_kind() != TokenKind::Type {
        return None;
    }
    lexer.next();

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

    if lexer.current().get_kind() != TokenKind::Enum {
        return None;
    }
    lexer.next();

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

        let element = match lexer.current().get_kind() {
            TokenKind::Literal => lexer.next().unwrap().into_literal(),
            _ => {
                let error = recover_until(
                    ParseErrorKind::NotFoundEnumElement,
                    lexer,
                    &[TokenKind::LineFeed, TokenKind::Comma, TokenKind::BraceRight],
                    Expected::EnumElement,
                    Scope::EnumDefine,
                    allocator,
                );
                errors.push(error);

                continue;
            }
        };

        elements.push(element);

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
        name,
        elements,
        span: anchor.elapsed(lexer),
    })
}
