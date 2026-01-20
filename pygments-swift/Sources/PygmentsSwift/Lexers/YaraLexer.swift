import Foundation

/// Pragmatic YARA lexer (smoke-test level).
public final class YaraLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "rule", "import", "include", "private", "global",
            "meta", "strings", "condition",
            "and", "or", "not", "of", "them", "any", "all", "for", "in", "at",
            "nocase", "wide", "ascii", "fullword", "xor", "base64", "base64wide",
            "contains", "matches",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#
        let stringId = #"\$[A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings / regexes / hex strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("/(?:\\\\/|[^/\\n])+/[a-zA-Z]*", action: .token(.string.child("Regex")))),
                .rule(Rule("\\{[0-9A-Fa-f\\s?\\-]+\\}", action: .token(.string.child("Hex")))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(stringId, action: .token(.name.child("Variable")))),
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]\\{\\},;:]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%<>=!&|^~?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
