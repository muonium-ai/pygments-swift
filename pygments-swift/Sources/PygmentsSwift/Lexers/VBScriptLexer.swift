import Foundation

/// Minimal VBScript lexer.
public final class VBScriptLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                // Comments
                Rule("'[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("(?i)\\brem\\b[^\\n]*", action: .token(.comment.child("Single"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Numbers
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)", action: .token(.number)),

                // Keywords (case-insensitive)
                Rule("(?i)\\b(?:dim|redim|preserve|set|let|const|if|then|else|elseif|end|for|each|to|step|next|while|wend|do|loop|until|function|sub|select|case|class|property|public|private|call|exit|on|error|resume)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),
                Rule("[()\\[\\]{}.,:]", action: .token(.punctuation)),
                Rule("==|<>|<=|>=|\\bmod\\b|\\band\\b|\\bor\\b|\\bnot\\b|[+\\-*/^=<>&]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
