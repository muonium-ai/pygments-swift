import Foundation

/// Minimal Nginx config lexer.
public final class NginxLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("\\b(?:on|off|true|false)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("\\b(?:\\d+(?:\\.\\d+)?)(?:[kKmMgG])?\\b", action: .token(.number)),

                Rule("[{};()]", action: .token(.punctuation)),

                // Directives
                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Attribute"))),

                Rule("[^\\s#;{}()]+", action: .token(.text)),
            ]
        ]
    }
}
