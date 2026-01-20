import Foundation

/// Pragmatic JSON5 lexer (smoke-test level).
///
/// Supports JSON with JSON5 extras like comments and single-quoted strings.
public final class Json5Lexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "true", "false", "null", "Infinity", "NaN",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_$][A-Za-z0-9_$]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d+)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords / constants
                .rule(Rule(keywords, action: .token(.keyword.child("Constant")))),

                // Punctuation
                .rule(Rule("[\\{\\}\\[\\],:]", action: .token(.punctuation))),

                // Identifiers (unquoted object keys)
                .rule(Rule(ident, action: .token(.name))),

                // Operators (very minimal)
                .rule(Rule("[+\\-*/%<>=!&|^~?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
