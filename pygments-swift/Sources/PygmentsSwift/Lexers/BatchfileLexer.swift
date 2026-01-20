import Foundation

/// Minimal Windows Batch (.bat/.cmd) lexer.
public final class BatchfileLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                // Labels
                Rule("^:.*$", options: [.anchorsMatchLines], action: .token(.name.child("Label"))),

                // Comments
                Rule("^(?:@?rem)\\b[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),
                Rule("^::[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Variables
                Rule("%[^%]+%", action: .token(.name.child("Variable"))),
                Rule("![^!]+!", action: .token(.name.child("Variable"))),

                // Numbers
                Rule("\\b\\d+\\b", action: .token(.number)),

                // Keywords (case-insensitive via inline flag)
                Rule("(?i)\\b(?:echo|set|if|else|for|in|do|goto|call|exit|shift|start|endlocal|setlocal)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[+\\-*/%<>=]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
