import Foundation

/// Minimal AWK lexer.
public final class AwkLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        let operators = #"==|!=|<=|>=|&&|\|\||\+\+|--|[+\-*/%<>=!?:]"#
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Strings and regex literals (simplified)
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("/(?:[^/\\\\]|\\\\.)+/", action: .token(.string.child("Regex"))),

                // Numbers
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                // Keywords / builtins
                Rule("\\b(?:BEGIN|END|BEGINFILE|ENDFILE|if|else|while|do|for|break|continue|next|nextfile|function|return|print|printf|delete|in)\\b", action: .token(.keyword)),

                // Variables like $1
                Rule("\\$\\d+", action: .token(.name.child("Variable"))),

                Rule(ident, action: .token(.name)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),
                Rule(operators, action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
