import Foundation

/// Minimal Apache config lexer.
public final class ApacheConfLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("</?[A-Za-z][A-Za-z0-9]*\\b", action: .token(.keyword)),
                Rule(">", action: .token(.punctuation)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable"))),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                // Directives at line start
                Rule("^([A-Za-z][A-Za-z0-9]*)\\b", options: [.anchorsMatchLines], action: .token(.name.child("Attribute"))),

                Rule("[=]", action: .token(.operator)),
                Rule("[{}();]", action: .token(.punctuation)),

                Rule("[^\\s#]+", action: .token(.text)),
            ]
        ]
    }
}
