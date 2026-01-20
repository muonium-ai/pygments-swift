import Foundation

/// Pragmatic Cap'n Proto schema lexer (smoke-test level).
public final class CapnProtoLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "using", "import",
            "struct", "interface", "enum", "union",
            "const", "annotation",
            "extends",
            "true", "false",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Annotations / field ordinals
                .rule(Rule("@\\d+", action: .token(.number))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d+)", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers / types
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[{}\\[\\]();,.:=]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
