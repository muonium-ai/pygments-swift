import Foundation

/// Minimal Nushell lexer.
public final class NushellLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\$" + ident, action: .token(.name.child("Variable"))),

                Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number)),

                Rule("\\b(?:def|export|let|mut|if|else|match|for|in|while|break|continue|return|do)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|\\|\\||&&|\\||=>|[=+\\-*/%<>&|!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
