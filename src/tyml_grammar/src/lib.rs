#[cfg(test)]
mod tests {
    use bnf_rules::bnf_rules_macro::bnf_rules;

    // This is an LR(1) parser generator, used for maintain quality.
    // If the specified grammar is ambiguous, compilation is aborted with conflict.
    // Usage : https://github.com/bea4dev/bnf_rules
    bnf_rules! {
        #[generate_code = false]

        source          ::= [ "!tyml" ] defines

        defines         ::= [ define ] { ( lf | "," [ lf ] ) define }
        define          ::= element_define | type_define

        element_define  ::= node_literal { "." node_literal } [ element_type ] [ default_value ]
        node_literal    ::= literal | "*"

        element_type    ::= ":" [ lf ] literal [ "?" ]

        default_value   ::= "=" [ lf ] ( string_literal | numeric_literal | "null" )

        string_literal  ::= r#""([^"\\]|\\.)*""# | r"'([^'\\]|\\.)*'"

        numeric_literal ::= float_numeric | binary_numeric

        float_numeric   ::= r"[+-]?[\d_]+(\.[\d_]+)?([eE][+-][\d_]+)?" | "inf" | "nan"

        binary_numeric  ::= r"0x[a-f|A-F|0-9|_]+" | r"0o[0-7|_]+" | r"0b[01_]+"

        type_define     ::= struct_define | enum_define

        struct_define   ::= "type" literal [ lf ] "{" defines "}"

        enum_define     ::= "enum" literal [ lf ] "{" enum_elements "}"
        enum_elements   ::= [ literal ] { ( lf | "," [ lf ] ) literal }

        literal         ::= r"\w+"

        lf              ::= lf_code { lf_code }
        lf_code         ::= "\n" | "\r"
    }
}
