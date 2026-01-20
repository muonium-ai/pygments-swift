import Foundation

/// Minimal SQL lexer.
public final class SqlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("--[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("'(?:''|[^'])*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[(),.;]", action: .token(.punctuation)),
                Rule("[+*/<>=~!]+", action: .token(.operator)),

                Rule(
                    "\\b(?:select|from|where|insert|into|values|update|set|delete|create|table|drop|alter|join|left|right|inner|outer|on|group|by|order|having|limit|offset|as|distinct|and|or|null|is|not|in|exists|case|when|then|else|end)\\b",
                    options: [.caseInsensitive],
                    action: .token(.keyword)
                ),

                Rule("[A-Za-z_][A-Za-z0-9_$$]*", action: .token(.name))
            ]
        ]
    }
}
