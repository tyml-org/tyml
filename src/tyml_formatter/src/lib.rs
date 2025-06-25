use std::{borrow::Cow, iter::Peekable};

pub struct GeneralFormatter<'input> {
    tree: Vec<FormatterTokenTree<'input>>,
}

impl<'input> GeneralFormatter<'input> {
    pub fn new(tokens: impl Iterator<Item = FormatterToken<'input>>) -> Self {
        let tree = Self::parse_elements(&mut tokens.peekable());

        Self { tree }
    }

    fn parse_tree(
        tokens: &mut Peekable<impl Iterator<Item = FormatterToken<'input>>>,
    ) -> Option<FormatterTokenTree<'input>> {
        let Some(current_token) = tokens.peek() else {
            return None;
        };

        match current_token.kind {
            FormatterTokenKind::TreeIn => {
                let tree_in = tokens.next().unwrap();

                let elements = Self::parse_elements(tokens);

                let tree_out = match tokens.next() {
                    Some(token) => match token.kind {
                        FormatterTokenKind::TreeOut => Some(token),
                        _ => None,
                    },
                    _ => None,
                };

                let Some(tree_out) = tree_out else {
                    return None;
                };

                Some(FormatterTokenTree::Node {
                    tree_in,
                    elements,
                    tree_out,
                })
            }
            FormatterTokenKind::TreeOut => return None,
            _ => Some(FormatterTokenTree::Leaf {
                token: tokens.next().unwrap(),
            }),
        }
    }

    fn parse_elements(
        tokens: &mut Peekable<impl Iterator<Item = FormatterToken<'input>>>,
    ) -> Vec<FormatterTokenTree<'input>> {
        let mut elements = Vec::new();
        loop {
            let Some(element) = Self::parse_tree(tokens) else {
                break;
            };
            elements.push(element);
        }
        elements
    }

    pub fn format(&mut self) {
        for tree in self.tree.iter_mut() {
            Self::format_tree(tree, 0, false);
        }
    }

    const MAX_COLUMN: usize = 100;

    fn format_tree(tree: &mut FormatterTokenTree<'input>, indent: usize, should_lf: bool) {
        match tree {
            FormatterTokenTree::Leaf { token: _ } => unreachable!(),
            FormatterTokenTree::Node {
                tree_in,
                elements,
                tree_out,
            } => {
                // remove existed whitespace
                for element in elements.iter_mut() {
                    if let FormatterTokenTree::Leaf { token } = element {
                        if token.kind == FormatterTokenKind::Whitespace {
                            *element = FormatterTokenTree::None;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }

                Self::insert_space(tree_in.right_space, elements, 0, should_lf);

                for i in 1..usize::MAX {
                    if i >= elements.len() {
                        break;
                    }

                    
                }
            }
            FormatterTokenTree::None => {}
        }
    }

    fn insert_space(
        format: SpaceFormat,
        elements: &mut Vec<FormatterTokenTree<'input>>,
        index: usize,
        should_lf: bool,
    ) {
        match format {
            SpaceFormat::Space => {
                elements.insert(
                    index,
                    FormatterTokenTree::Leaf {
                        token: FormatterToken {
                            text: " ".into(),
                            kind: FormatterTokenKind::Whitespace,
                            right_space: SpaceFormat::None,
                        },
                    },
                );
            }
            SpaceFormat::LineFeed => {
                // remove existed lf
                for element in &mut elements[index..] {
                    if let FormatterTokenTree::Leaf { token } = element {
                        if token.kind == FormatterTokenKind::LineFeed {
                            *element = FormatterTokenTree::None;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                elements.insert(
                    index,
                    FormatterTokenTree::Leaf {
                        token: FormatterToken {
                            text: "\n".into(),
                            kind: FormatterTokenKind::LineFeed,
                            right_space: SpaceFormat::None,
                        },
                    },
                );
            }
            SpaceFormat::SpaceOrLineFeed => {
                // remove existed lf
                for element in &mut elements[index..] {
                    if let FormatterTokenTree::Leaf { token } = element {
                        if token.kind == FormatterTokenKind::LineFeed {
                            *element = FormatterTokenTree::None;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if should_lf {
                    elements.insert(
                        index,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: "\n".into(),
                                kind: FormatterTokenKind::LineFeed,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );
                } else {
                    elements.insert(
                        index,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: " ".into(),
                                kind: FormatterTokenKind::Whitespace,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );
                }
            }
            SpaceFormat::None => {}
        }
    }
}

enum FormatterTokenTree<'input> {
    Leaf {
        token: FormatterToken<'input>,
    },
    Node {
        tree_in: FormatterToken<'input>,
        elements: Vec<Self>,
        tree_out: FormatterToken<'input>,
    },
    None,
}

impl FormatterTokenTree<'_> {
    fn count_chars(&self) -> usize {
        match self {
            FormatterTokenTree::Leaf { token } => token.text.chars().count(),
            FormatterTokenTree::Node {
                tree_in,
                elements,
                tree_out,
            } => {
                tree_in.text.chars().count()
                    + elements
                        .iter()
                        .map(|tree| tree.count_chars())
                        .sum::<usize>()
                    + tree_out.text.chars().count()
            }
            FormatterTokenTree::None => 0,
        }
    }

    fn right_space(&self) -> SpaceFormat {
        match self {
            FormatterTokenTree::Leaf { token } => token.right_space,
            FormatterTokenTree::Node {
                tree_in: _,
                elements: _,
                tree_out,
            } => tree_out.right_space,
            FormatterTokenTree::None => SpaceFormat::None,
        }
    }
}

pub struct FormatterToken<'input> {
    pub text: Cow<'input, str>,
    pub kind: FormatterTokenKind,
    pub right_space: SpaceFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatterTokenKind {
    Normal,
    Whitespace,
    LineFeed,
    Split,
    TreeIn,
    TreeOut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceFormat {
    Space,
    LineFeed,
    SpaceOrLineFeed,
    None,
}
