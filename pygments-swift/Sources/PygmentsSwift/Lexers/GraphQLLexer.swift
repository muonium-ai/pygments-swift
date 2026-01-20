import Foundation

/// Minimal GraphQL lexer.
public final class GraphQLLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Block string
                Rule("\"\"\"[\\s\\S]*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:query|mutation|subscription|fragment|on|true|false|null)\\b", action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),
                Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("[!():=\\[\\]{},|]", action: .token(.punctuation)),
                Rule("\\.", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
