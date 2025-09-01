use std::collections::HashMap;

use extension_fn::extension_fn;
use tyml_formatter::{FormatterToken, FormatterTokenKind, SpaceFormat};

use crate::{
    ast::{Define, Defines, Interface, JsonValue, TypeDefine, AST},
    lexer::{Lexer, TokenKind},
};

#[extension_fn(<'input> Lexer<'input>)]
pub fn into_formatter_token(self, ast: &Defines) -> Vec<FormatterToken<'input>> {
    let comma_positions = collect_comma_position(ast);

    self.into_iter()
        .map(|token| {
            let mut formatter_token = match token.kind {
                TokenKind::Colon => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Equal => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Comma => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::LineFeedAndSplit {
                        split: ",",
                        is_extra: false,
                        need_whitespace: true,
                    },
                    right_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: true,
                    },
                },
                TokenKind::BraceLeft => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeIn,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: true,
                    },
                },
                TokenKind::BraceRight => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeOut,
                    left_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: true,
                    },
                    right_space: SpaceFormat::None,
                },
                TokenKind::BracketLeft => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeIn,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: false,
                    },
                },
                TokenKind::BracketRight => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeOut,
                    left_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: false,
                    },
                    right_space: SpaceFormat::None,
                },
                TokenKind::ParenthesisLeft => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeIn,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: false,
                    },
                },
                TokenKind::ParenthesisRight => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::TreeOut,
                    left_space: SpaceFormat::SpaceOrLineFeed {
                        need_whitespace: false,
                    },
                    right_space: SpaceFormat::None,
                },
                TokenKind::VerticalLine => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Type => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Enum => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::And => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Or => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::AtValue => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::AtLength => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::AtU8Size => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::AtRegex => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::LineFeed => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::LineFeed,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::None,
                },
                TokenKind::Comment => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Comment,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::None,
                },
                TokenKind::Document => FormatterToken {
                    text: token.text.replace("\n", "").replace("\r", "").into(),
                    kind: FormatterTokenKind::Document,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::LineFeed,
                },
                TokenKind::Function => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Arrow => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Interface => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Throws => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Authed => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                TokenKind::Return => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::Space,
                    right_space: SpaceFormat::Space,
                },
                _ => FormatterToken {
                    text: token.text.into(),
                    kind: FormatterTokenKind::Normal,
                    left_space: SpaceFormat::None,
                    right_space: SpaceFormat::None,
                },
            };
            if let Some((comma_kind, is_extra, need_whitespace)) =
                comma_positions.get(&token.span.end)
            {
                match comma_kind {
                    CommaKind::Necessary => {
                        formatter_token.right_space = SpaceFormat::LineFeedAndSplit {
                            split: ",",
                            is_extra: *is_extra,
                            need_whitespace: *need_whitespace,
                        };
                    }
                    CommaKind::Unnecessary => {
                        formatter_token.right_space = SpaceFormat::LineFeedOrSplit {
                            split: ",",
                            is_extra: *is_extra,
                            need_whitespace: *need_whitespace,
                        };
                    }
                    CommaKind::LineFeedOnly => {
                        formatter_token.right_space = SpaceFormat::LineFeed;
                    }
                }
            }

            formatter_token
        })
        .collect()
}

enum CommaKind {
    Necessary,
    Unnecessary,
    LineFeedOnly,
}

fn collect_comma_position(ast: &Defines) -> HashMap<usize, (CommaKind, bool, bool)> {
    let mut positions = HashMap::new();

    collect_comma_position_on_defines(ast, &mut positions);

    positions
}

fn collect_comma_position_on_defines(
    ast: &Defines,
    positions: &mut HashMap<usize, (CommaKind, bool, bool)>,
) {
    for (index, element) in ast.defines.iter().enumerate() {
        match element {
            Define::Element(element_define) => {
                positions.insert(
                    element_define.span.end,
                    (CommaKind::Unnecessary, index == ast.defines.len() - 1, true),
                );

                if let Some(inline_type) = &element_define.inline_type {
                    collect_comma_position_on_defines(&inline_type.defines, positions);
                }
            }
            Define::Type(type_define) => match type_define {
                TypeDefine::Struct(struct_define) => {
                    collect_comma_position_on_defines(&struct_define.defines, positions);
                }
                TypeDefine::Enum(enum_define) => {
                    for (index, element) in enum_define.elements.iter().enumerate() {
                        positions.insert(
                            element.span.end,
                            (
                                CommaKind::Unnecessary,
                                index == enum_define.elements.len() - 1,
                                true,
                            ),
                        );
                    }
                }
            },
            Define::Interface(interface) => {
                collect_comma_position_on_interface(interface, positions);
            }
        }
    }
}

fn collect_comma_position_on_interface(
    ast: &Interface,
    positions: &mut HashMap<usize, (CommaKind, bool, bool)>,
) {
    for function in ast.functions.iter() {
        for (index, argument) in function.arguments.iter().enumerate() {
            let is_extra = index == function.arguments.len() - 1;
            positions.insert(
                argument.span.end,
                (CommaKind::Necessary, is_extra, !is_extra),
            );

            if let Some(default_value) = &argument.default_value {
                collect_comma_position_on_json(default_value, positions);
            }
        }

        if let Some(return_block) = &function.return_block {
            collect_comma_position_on_json(&return_block.return_expression.value, positions);
        }

        if !function.properties.elements.is_empty() {
            positions.insert(
                function.properties.elements.last().unwrap().span.end,
                (CommaKind::LineFeedOnly, false, false),
            );
        }

        positions.insert(function.span.end, (CommaKind::LineFeedOnly, false, false));
    }
}

fn collect_comma_position_on_json(
    ast: &JsonValue,
    positions: &mut HashMap<usize, (CommaKind, bool, bool)>,
) {
    match ast {
        JsonValue::Value(_) => {}
        JsonValue::Array(json_array) => {
            for (index, element) in json_array.elements.iter().enumerate() {
                positions.insert(
                    element.span().end,
                    (
                        CommaKind::Necessary,
                        index == json_array.elements.len() - 1,
                        true,
                    ),
                );

                collect_comma_position_on_json(element, positions);
            }
        }
        JsonValue::Object(json_object) => {
            for (index, element) in json_object.elements.iter().enumerate() {
                positions.insert(
                    element.span.end,
                    (
                        CommaKind::Necessary,
                        index == json_object.elements.len() - 1,
                        true,
                    ),
                );

                collect_comma_position_on_json(&element.value, positions);
            }
        }
    }
}
