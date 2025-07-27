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
        .map(|token| match token.kind {
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
                // maybe, unused
                text: token.text.into(),
                kind: FormatterTokenKind::Normal,
                left_space: SpaceFormat::LineFeedOrSplit {
                    split: ",",
                    is_extra: false,
                },
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BraceLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BraceRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::SpaceOrLineFeed,
                right_space: SpaceFormat::None,
            },
            TokenKind::BracketLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::Space,
                right_space: SpaceFormat::SpaceOrLineFeed,
            },
            TokenKind::BracketRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::SpaceOrLineFeed,
                right_space: SpaceFormat::None,
            },
            TokenKind::ParenthesisLeft => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeIn,
                left_space: SpaceFormat::None,
                right_space: SpaceFormat::None,
            },
            TokenKind::ParenthesisRight => FormatterToken {
                text: token.text.into(),
                kind: FormatterTokenKind::TreeOut,
                left_space: SpaceFormat::None,
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
            _ => {
                if let Some(&is_extra) = comma_positions.get(&token.span.end) {
                    FormatterToken {
                        text: token.text.into(),
                        kind: FormatterTokenKind::Normal,
                        left_space: SpaceFormat::None,
                        right_space: SpaceFormat::LineFeedOrSplit {
                            split: ",",
                            is_extra,
                        },
                    }
                } else {
                    FormatterToken {
                        text: token.text.into(),
                        kind: FormatterTokenKind::Normal,
                        left_space: SpaceFormat::None,
                        right_space: SpaceFormat::None,
                    }
                }
            }
        })
        .collect()
}

fn collect_comma_position(ast: &Defines) -> HashMap<usize, bool> {
    let mut positions = HashMap::new();

    collect_comma_position_on_defines(ast, &mut positions);

    positions
}

fn collect_comma_position_on_defines(ast: &Defines, positions: &mut HashMap<usize, bool>) {
    for (index, element) in ast.defines.iter().enumerate() {
        match element {
            Define::Element(element_define) => {
                positions.insert(element_define.span.end, index == ast.defines.len() - 1);

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
                        positions.insert(element.span.end, index == enum_define.elements.len() - 1);
                    }
                }
            },
            Define::Interface(interface) => {
                collect_comma_position_on_interface(interface, positions);
            }
        }
    }
}

fn collect_comma_position_on_interface(ast: &Interface, positions: &mut HashMap<usize, bool>) {
    for function in ast.functions.iter() {
        for (index, argument) in function.arguments.iter().enumerate() {
            positions.insert(argument.span.end, index == function.arguments.len() - 1);

            if let Some(default_value) = &argument.default_value {
                collect_comma_position_on_json(default_value, positions);
            }
        }

        if let Some(return_block) = &function.return_block {
            collect_comma_position_on_json(&return_block.return_expression.value, positions);
        }
    }
}

fn collect_comma_position_on_json(ast: &JsonValue, positions: &mut HashMap<usize, bool>) {
    match ast {
        JsonValue::Value(_) => {}
        JsonValue::Array(json_array) => {
            for (index, element) in json_array.elements.iter().enumerate() {
                positions.insert(element.span().end, index == json_array.elements.len() - 1);

                collect_comma_position_on_json(element, positions);
            }
        }
        JsonValue::Object(json_object) => {
            for (index, element) in json_object.elements.iter().enumerate() {
                positions.insert(element.span.end, index == json_object.elements.len() - 1);

                collect_comma_position_on_json(&element.value, positions);
            }
        }
    }
}
