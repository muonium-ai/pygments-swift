import Foundation

/// Pragmatic Smithy IDL lexer (smoke-test level).
public final class SmithyLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "namespace", "use",
            "service", "operation", "resource",
            "structure", "union", "enum",
            "list", "map",
            "string", "integer", "boolean", "byte", "short", "long", "float", "double",
            "timestamp", "blob", "document",
            "apply",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Traits: @trait(...)
                .rule(Rule("(@)(" + ident + ")", action: .byGroups([
                    .name.child("Decorator"),
                    .name.child("Decorator"),
                ]))),

                // Strings
                .rule(Rule("\"\"\".*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("[{}()\\[\\],.:=]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
