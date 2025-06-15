#[cfg(test)]
mod tests {
    use bnf_rules::bnf_rules_macro::bnf_rules;

    // This is an LR(1) parser generator, used for maintain quality.
    // If the specified grammar is ambiguous, compilation is aborted with conflict.
    // Usage : https://github.com/bea4dev/bnf_rules
    bnf_rules! {
        source             ::= defines

        defines            ::= [ lf ] { define ( lf | "," [ lf ] ) }
        define             ::= documents ( element_define | type_define )

        documents          ::= { r"(###|///)[^\n\r]*(\n|\r|\r\n)" }
        //comments         ::= r"//[^\n\r]*(\n|\r|\r\n)" | "/\*.*\*/"  /* ignored in lexer */

        element_define     ::= ( literal | "*" ) [ lf ] type_or_value
        type_or_value      ::= element_type [ default_value ] | default_value | inline_type_define

        element_type       ::= ":" or_type
        or_type            ::= base_type { "|" [ lf ] base_type }
        base_type          ::= ( named_type | array_type ) [ "?" ]
        array_type         ::= "[" [ lf ] or_type [ lf ] "]"
        named_type         ::= literal

        inline_type_define ::= ":" "{" defines "}"

        default_value      ::= "=" ( string_literal | numeric_literal | "null" )

        string_literal     ::= r#""([^"\\]|\\.)*""# | r"'([^'\\]|\\.)*'"

        numeric_literal    ::= float_numeric | binary_numeric

        float_numeric      ::= r"[+-]?[\d_]+(\.[\d_]+)?([eE][+-][\d_]+)?" | "inf" | "nan"

        binary_numeric     ::= r"0x[a-f|A-F|0-9|_]+" | r"0o[0-7|_]+" | r"0b[01_]+"

        type_define        ::= struct_define | enum_define

        struct_define      ::= "type" literal [ lf ] "{" defines "}"

        enum_define        ::= "enum" literal [ lf ] "{" enum_elements "}"
        enum_elements      ::= [ lf ] { documents string_literal ( lf | "," [ lf ] ) }

        literal            ::= r"(\w|-)+" | string_literal

        lf                 ::= r"(\n|\r|\r\n)+"
    }
}
