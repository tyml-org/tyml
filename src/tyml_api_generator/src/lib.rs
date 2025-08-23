use std::path::PathBuf;

pub mod general;
pub mod name;
pub mod server;

pub struct GeneratorSettings {
    pub package_name: String,
    pub package_path: PathBuf,
}

#[cfg(test)]
mod test {
    use tyml::{Tyml, tyml_type::types::TypeTree};

    use crate::{general::rust::generate_type_tree_for_rust, name::NameContext};

    #[test]
    fn type_gen() {
        let source = r#"
/// User docs
/// yay
type User {
    /// this is id!
    id: int
    name: string | Name
}

type Name {
    name: string
    display_name: string
}

user: User
        "#;

        let tyml = Tyml::parse(source.to_string());

        let mut type_def = String::new();
        let mut name_context = NameContext::new();

        if let TypeTree::Node {
            node,
            any_node: _,
            node_key_span: _,
            any_node_key_span: _,
            documents: _,
            span: _,
        } = tyml.type_tree()
        {
            for (name, tree) in node.iter() {
                let type_name = generate_type_tree_for_rust(
                    tree,
                    &mut type_def,
                    &mut name_context,
                    tyml.named_type_map(),
                );
                println!("{}: {}", name, type_name);
            }
        }

        println!("{}", type_def);
    }
}
