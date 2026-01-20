import Foundation

/// Minimal Pascal lexer (Delphi-ish).
public final class PascalLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                // Comments
                Rule("\\{[\\s\\S]*?\\}", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("\\(\\*[\\s\\S]*?\\*\\)", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),

                // Strings (Pascal uses single quotes; doubled '' escape)
                Rule("'(?:[^']|'')*'", action: .token(.string)),

                // Numbers
                Rule("\\b(?:\\d+\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?\\b", action: .token(.number)),
                Rule("\\$[0-9A-Fa-f]+", action: .token(.number)),

                // Keywords
                Rule("(?i)\\b(?:program|unit|interface|implementation|uses|type|var|const|begin|end|procedure|function|class|record|object|if|then|else|case|of|for|to|downto|while|do|repeat|until|with|try|except|finally|raise|in|is)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),
                Rule(":=|\\.\\.|<=|>=|<>|[+\\-*/=<>&^@]", action: .token(.operator)),

                Rule("[:;,.()\\[\\]]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
