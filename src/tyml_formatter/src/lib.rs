use std::{borrow::Cow, iter::Peekable};

#[derive(Debug)]
pub struct GeneralFormatter<'input> {
    tree: Vec<FormatterTokenTree<'input>>,
    max_column: usize,
}

impl<'input> GeneralFormatter<'input> {
    pub fn new(tokens: impl Iterator<Item = FormatterToken<'input>>, max_column: usize) -> Self {
        // Overwrite and merge with the next space info of token
        //
        //  tokens[0]       tokens[1]
        //  │       │       │       │
        // left   right    left   right
        //  │       │       │       │
        //  ▼       ▼       ▼       ▼
        //None    None    Space    None
        //          ▲       │
        //          └───────┘
        //          overwrite
        //
        let mut tokens = tokens.collect::<Vec<_>>();
        for i in 0..tokens.len() {
            let left_format = tokens[i].left_space;
            if i != 0 {
                let right_format = &mut tokens[i - 1].right_space;

                // overwrite
                if *right_format == SpaceFormat::None {
                    *right_format = left_format;
                }
            }
        }

        let tree = Self::parse_elements(&mut tokens.into_iter().peekable());

        Self { tree, max_column }
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
        let mut i = 0;
        loop {
            if i >= self.tree.len() {
                break;
            }

            let space_format = self.tree[i].right_space();

            let step_add = Self::insert_space(space_format, &mut self.tree, i + 1, 0, true);

            i += 1 + step_add;
        }

        for element in self.tree.iter_mut() {
            if let tree @ FormatterTokenTree::Node {
                tree_in: _,
                elements: _,
                tree_out: _,
            } = element
            {
                Self::format_tree(tree, 1, self.max_column);
            }
        }
    }

    fn format_tree(tree: &mut FormatterTokenTree<'input>, indent: usize, max_column: usize) {
        let should_lf = tree.count_chars() > max_column;

        match tree {
            FormatterTokenTree::Leaf { token: _ } => unreachable!(),
            FormatterTokenTree::Node {
                tree_in,
                elements,
                tree_out: _,
            } => {
                Self::insert_space(tree_in.right_space, elements, 0, indent, should_lf);

                let mut i = 0;
                loop {
                    if i >= elements.len() {
                        break;
                    }

                    let space_format = elements[i].right_space();

                    let step_add =
                        Self::insert_space(space_format, elements, i + 1, indent, should_lf);

                    i += 1 + step_add;
                }

                // ajust last whitespace indent
                for element in elements.iter_mut().rev() {
                    if let FormatterTokenTree::Leaf { token } = element {
                        if token.kind == FormatterTokenKind::Whitespace
                            && token.text.as_ref().chars().count() % 4 == 0
                        {
                            token.text = "    "
                                .repeat(indent.checked_sub(1).unwrap_or_default())
                                .into();
                        }
                    }

                    if let FormatterTokenTree::None = element {
                        continue;
                    } else {
                        break;
                    }
                }

                for element in elements.iter_mut() {
                    if let tree @ FormatterTokenTree::Node {
                        tree_in: _,
                        elements: _,
                        tree_out: _,
                    } = element
                    {
                        Self::format_tree(tree, indent + 1, max_column);
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
        indent: usize,
        should_lf: bool,
    ) -> usize {
        match format {
            SpaceFormat::Space => {
                elements.insert(
                    index,
                    FormatterTokenTree::Leaf {
                        token: FormatterToken {
                            text: " ".into(),
                            kind: FormatterTokenKind::Whitespace,
                            left_space: SpaceFormat::None,
                            right_space: SpaceFormat::None,
                        },
                    },
                );

                1
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
                            left_space: SpaceFormat::None,
                            right_space: SpaceFormat::None,
                        },
                    },
                );

                elements.insert(
                    index + 1,
                    FormatterTokenTree::Leaf {
                        token: FormatterToken {
                            text: "    ".repeat(indent).into(),
                            kind: FormatterTokenKind::Whitespace,
                            left_space: SpaceFormat::None,
                            right_space: SpaceFormat::None,
                        },
                    },
                );

                2
            }
            SpaceFormat::SpaceOrLineFeed => {
                // remove existed lf
                // better logic?
                for element in &mut elements[index.checked_sub(1).unwrap_or_default()..] {
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
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );

                    elements.insert(
                        index + 1,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: "    ".repeat(indent).into(),
                                kind: FormatterTokenKind::Whitespace,
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );

                    2
                } else {
                    elements.insert(
                        index,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: " ".into(),
                                kind: FormatterTokenKind::Whitespace,
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );

                    1
                }
            }
            SpaceFormat::LineFeedOrSplit(split) => {
                // remove existed lf or split
                for element in &mut elements[index..] {
                    if let FormatterTokenTree::Leaf { token } = element {
                        if token.kind == FormatterTokenKind::LineFeed
                            || token.text.as_ref() == split
                        {
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
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );

                    elements.insert(
                        index + 1,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: "    ".repeat(indent).into(),
                                kind: FormatterTokenKind::Whitespace,
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );
                } else {
                    elements.insert(
                        index,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: split.into(),
                                kind: FormatterTokenKind::Normal,
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );

                    elements.insert(
                        index + 1,
                        FormatterTokenTree::Leaf {
                            token: FormatterToken {
                                text: " ".into(),
                                kind: FormatterTokenKind::Whitespace,
                                left_space: SpaceFormat::None,
                                right_space: SpaceFormat::None,
                            },
                        },
                    );
                }

                2
            }
            SpaceFormat::None => 0,
        }
    }

    pub fn generate_code(&self) -> String {
        let mut code = String::new();

        for tree in self.tree.iter() {
            Self::generate_code_recursive(tree, &mut code);
        }

        code
    }

    fn generate_code_recursive(tree: &FormatterTokenTree<'input>, code: &mut String) {
        match tree {
            FormatterTokenTree::Leaf { token } => {
                *code += token.text.as_ref();
            }
            FormatterTokenTree::Node {
                tree_in,
                elements,
                tree_out,
            } => {
                *code += tree_in.text.as_ref();

                for element in elements {
                    Self::generate_code_recursive(element, code);
                }

                *code += tree_out.text.as_ref();
            }
            FormatterTokenTree::None => {}
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FormatterToken<'input> {
    pub text: Cow<'input, str>,
    pub kind: FormatterTokenKind,
    pub left_space: SpaceFormat,
    pub right_space: SpaceFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatterTokenKind {
    Normal,
    Whitespace,
    LineFeed,
    Comment,
    Document,
    TreeIn,
    TreeOut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceFormat {
    Space,
    LineFeed,
    SpaceOrLineFeed,
    LineFeedOrSplit(&'static str),
    None,
}
