import Foundation

/// Pragmatic Turtle (RDF) lexer (smoke-test level).
public final class TurtleLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "@prefix", "@base", "a",
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // IRIs
                .rule(Rule("<[^>\\n]*>", action: .token(.name.child("Namespace")))),

                // Prefixed names
                .rule(Rule("[A-Za-z_][A-Za-z0-9_-]*:[A-Za-z0-9_.-]*", action: .token(.name.child("Namespace")))),

                // Strings
                .rule(Rule("\"\"\".*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Punctuation
                .rule(Rule("[\\[\\]();.,]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
