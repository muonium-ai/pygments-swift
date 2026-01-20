import Foundation

/// Minimal LaTeX lexer.
public final class LatexLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("%[^\\n]*", action: .token(.comment.child("Single"))),

                // Math (very rough)
                Rule("\\$[^\\$]*\\$", action: .token(.string)),
                Rule("\\\\\\[[\\s\\S]*?\\\\\\]", options: [.dotMatchesLineSeparators], action: .token(.string)),

                // Commands
                Rule("\\\\(?:begin|end)\\s*\\{[A-Za-z0-9*_-]+\\}", action: .token(.keyword)),
                Rule("\\\\[A-Za-z@]+", action: .token(.keyword)),

                Rule("\\{\\}|[{}\\[\\]()]", action: .token(.punctuation)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
