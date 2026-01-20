import Foundation

/// Minimal Nim lexer.
public final class NimLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("##[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"\"\"[\\s\\S]*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?(?:[eE][-+]?\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:proc|func|var|let|const|if|elif|else|for|while|return|type|object|import|from|as|when|template|macro|iterator|yield|try|except|finally|raise)\\b", action: .token(.keyword)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|!=|<=|>=|\\+\\+|--|&&|\\|\\||->|:=|=|[+\\-*/%<>!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
