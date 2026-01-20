import Foundation

/// Pragmatic QML lexer (smoke-test level).
public final class QmlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "import", "as",
            "property", "signal", "function",
            "var", "let", "const",
            "if", "else", "for", "while", "return", "break", "continue",
            "true", "false", "null",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // QML type ids and identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[{}\\[\\]();,.:]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|&&|\\|\\|)", action: .token(.operator))),
                .rule(Rule("[=<>+\\-*/%!&|^~?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
