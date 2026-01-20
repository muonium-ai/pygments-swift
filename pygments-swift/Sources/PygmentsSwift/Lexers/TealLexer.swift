import Foundation

/// Pragmatic Teal lexer (smoke-test level).
///
/// Teal is a typed dialect of Lua.
public final class TealLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            // Lua keywords
            "and", "break", "do", "else", "elseif", "end", "false", "for", "function",
            "if", "in", "local", "nil", "not", "or", "repeat", "return", "then",
            "true", "until", "while",
            // Teal additions (common)
            "record", "enum", "type", "global",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--\\[\\[.*?\\]\\]", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),
                .rule(Rule("\\[\\[.*?\\]\\]", options: [.dotMatchesLineSeparators], action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule("[()\\[\\]{}.,;]", action: .token(.punctuation))),
                .rule(Rule("(::|:=|==|!=|<=|>=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%^#=<>~:]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
