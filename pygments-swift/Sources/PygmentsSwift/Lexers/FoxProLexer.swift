import Foundation

/// Pragmatic FoxPro / xBase lexer (smoke-test level).
public final class FoxProLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "procedure", "function", "return", "local", "public", "private", "parameters",
            "if", "else", "endif", "do", "case", "endcase", "for", "next", "while", "endwhile",
            "select", "from", "where", "into", "endproc", "endfunc", "with", "endwith"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments: '*' in column 1, or '&&' inline.
                .rule(Rule("^\\*[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Single")))),
                .rule(Rule("&&[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),
                .rule(Rule("\\b(?:\\.t\\.|\\.f\\.)\\b", options: [.caseInsensitive], action: .token(.keyword.child("Constant")))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule(":=|==|!=|<=|>=|=|[-+*/%<>]", action: .token(.operator))),
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
