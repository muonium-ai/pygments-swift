import Foundation

/// Pragmatic ABAP lexer (smoke-test level).
public final class AbapLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "report", "data", "types", "write", "select", "endselect", "from", "where",
            "if", "elseif", "else", "endif", "loop", "endloop", "do", "enddo",
            "form", "endform", "module", "endmodule", "function", "endfunction",
            "class", "endclass", "method", "endmethod", "interface", "endinterface",
            "try", "catch", "endtry", "raise", "return", "call", "perform"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Full-line comments: '*' in column 1.
                .rule(Rule("^\\*[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Single")))),
                // Inline comments start with a double-quote.
                .rule(Rule("\"[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("'(?:''|[^'])*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule(":=|=|<>|<=|>=|<|>|[+\\-*/]", action: .token(.operator))),

                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),
                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
