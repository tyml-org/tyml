#[cfg(test)]
mod tests {
    use bnf_rules::bnf_rules_macro::bnf_rules;

    // This is an LR(1) parser generator, used for maintain quality.
    // If the specified grammar is ambiguous, compilation is aborted with conflict.
    // Usage : https://github.com/bea4dev/bnf_rules
    bnf_rules! {
        source             ::= defines

        defines            ::= [ lf ] { define ( lf | "," [ lf ] ) }
        define             ::= documents ( element_define | type_define | interface )

        documents          ::= { r"(###|///)[^\n\r]*(\n|\r|\r\n)" }
        //comments         ::= r"//[^\n\r]*(\n|\r|\r\n)" | "/\*.*\*/"  /* ignored in lexer */

        element_define     ::= ( literal | "*" ) [ lf ] type_or_value
        type_or_value      ::= element_type [ default_value ] | default_value | inline_type_define

        element_type       ::= ":" or_type
        or_type            ::= base_type { "|" [ lf ] base_type }
        base_type          ::= ( named_type | array_type ) [ "?" ] attribute_or
        array_type         ::= "[" [ lf ] or_type [ lf ] "]"
        named_type         ::= normal_literal

        attribute_or       ::= attribute_and { "or" attribute_and }
        attribute_and      ::= type_attribute { "and" type_attribute }

        type_attribute     ::= int_attribute | regex_attribute | "(" attribute_or ")"
        int_attribute      ::= ( "@value" | "@length" | "@u8size" ) [ r"\d+" ] ( "..<" | "..=" | ".." ) [ r"\d+" ]
        regex_attribute    ::= "@regex" string_literal

        inline_type_define ::= ":" "{" defines "}"

        default_value      ::= "=" value_literal

        value_literal      ::= string_literal | numeric_literal | "true" | "false" | "null"

        string_literal     ::= r#""([^"\\]|\\.)*""# | r"'([^'\\]|\\.)*'"

        numeric_literal    ::= float_numeric | binary_numeric

        float_numeric      ::= r"[+-]?[\d_]+(\.[\d_]+)?([eE][+-][\d_]+)?" | "inf" | "nan"

        binary_numeric     ::= r"0x[a-f|A-F|0-9|_]+" | r"0o[0-7|_]+" | r"0b[01_]+"

        type_define        ::= struct_define | enum_define

        struct_define      ::= "type" normal_literal [ lf ] "{" defines "}"

        enum_define        ::= "enum" normal_literal [ lf ] "{" enum_elements "}"
        enum_elements      ::= [ lf ] { documents string_literal ( lf | "," [ lf ] ) }

        interface          ::= properties "interface" literal "{" { function lf } "}"

        properties         ::= { property [ lf ] }
        property           ::= "#" "[" literal "=" { value_literal } "]"

        function           ::= properties "function" normal_literal function_arguments [ return_type ]
                               [ "{" [ lf ] "return" json_value [ lf ] "}" ]
        function_arguments ::= "(" [ lf ] { properties literal element_type [ "=" json_value ] "," [ lf ] } ")"
        return_type        ::= "->" or_type

        json_value         ::= value_literal
                               | "[" [ lf ] { json_value "," [ lf ] } "]"
                               | "{" [ lf ] { literal "=" json_value "," [ lf ] } "}"

        literal            ::= normal_literal | string_literal
        normal_literal     ::= r"(\w|-)+"

        lf                 ::= r"(\n|\r|\r\n)+"
    }
}
