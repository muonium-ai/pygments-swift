import Foundation

/// Minimal reStructuredText lexer.
public final class ReStructuredTextLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Directives, e.g. ".. note::"
                Rule("^\\.\\.\\s+([A-Za-z0-9_-]+)::", options: [.anchorsMatchLines], action: .token(.keyword)),

                // Comments (common form)
                Rule("^\\.\\.\\s+.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),

                // Inline literals / emphasis
                Rule("``[^`]+``", action: .token(.string)),
                Rule("\\*\\*[^*]+\\*\\*", action: .token(.string)),
                Rule("\\*[^*]+\\*", action: .token(.string)),

                // Links/roles
                Rule("`[^`]+`_?", action: .token(.name)),

                // Section underlines (====, ----, etc.)
                Rule("^[=\\-~`^\"#*+]+$", options: [.anchorsMatchLines], action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
