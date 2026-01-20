import Foundation

/// Pragmatic Jsonnet lexer (smoke-test level).
public final class JsonnetLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "local", "function", "if", "then", "else", "for", "in",
            "import", "importstr", "true", "false", "null", "self", "super",
            "error", "assert",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_$][A-Za-z0-9_$]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d+)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]\\{\\},;:.]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|&&|\\|\\||::|->|\\+\\+|--)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%<>=!&|^~?]+", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
