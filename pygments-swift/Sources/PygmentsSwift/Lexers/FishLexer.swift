import Foundation

/// Minimal Fish shell lexer.
public final class FishLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable"))),
                Rule("\\$" + ident, action: .token(.name.child("Variable"))),

                Rule("\\b\\d+\\b", action: .token(.number)),

                Rule("\\b(?:function|end|set|if|else|for|in|while|switch|case|break|continue|return|and|or|not|begin)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|&&|\\|\\||\\||;|&|=|[+\\-*/<>]", action: .token(.operator)),
                Rule("[()\\[\\]{}]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
