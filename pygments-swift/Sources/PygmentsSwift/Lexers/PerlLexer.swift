import Foundation

/// Minimal Perl lexer.
public final class PerlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                // Variables
                Rule("[$@%]\\{[^\\}]+\\}", action: .token(.name.child("Variable"))),
                Rule("[$@%][A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                // Keywords
                Rule("\\b(?:my|our|sub|use|package|require|if|elsif|else|while|for|foreach|until|return|print|say|last|next|redo|goto)\\b", action: .token(.keyword)),

                // Identifiers
                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|=>|=~|!~|\\+\\+|--|&&|\\|\\||::|->|[=+\\-*/%<>!?:]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
