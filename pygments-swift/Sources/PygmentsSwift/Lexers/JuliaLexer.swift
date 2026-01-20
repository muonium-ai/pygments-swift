import Foundation

/// Minimal Julia lexer.
public final class JuliaLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#=([\\s\\S]*?)=#", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"\"\"([\\s\\S]*?)\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?(?:[eE][-+]?\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:function|end|if|else|elseif|for|while|begin|let|local|global|const|struct|mutable|module|using|import|export|return|break|continue|try|catch|finally|do|quote|macro)\\b", action: .token(.keyword)),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),
                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("::|->|==|!=|<=|>=|\\.\\.|\\+\\+|--|&&|\\|\\||[=+\\-*/%<>!?:^\\\\]", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
