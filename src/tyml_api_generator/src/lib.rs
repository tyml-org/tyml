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

    use crate::{
        general::rust::generate_type_tree_for_rust,
        name::NameContext,
        server::rust_axum::{function_gen::generate_trait, lib_gen::generate_server},
    };

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

    #[test]
    fn trait_gen() {
        let source = r#"
type User {
    id: int
    name: string | Name
}

type Name {
    name: string
    display_name: string
}

interface API {
    function get_user(id: int) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_trait(&tyml));
    }

    #[test]
    fn rust_server_gen() {
        let source = r#"
interface API {
    function get_user(@body: int) -> User throws string
}
        "#;

        let tyml = Tyml::parse(source.to_string());

        println!("{}", generate_server(&tyml));
    }
}
