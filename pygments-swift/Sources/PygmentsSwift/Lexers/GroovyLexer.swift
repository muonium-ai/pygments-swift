import Foundation

/// Minimal Groovy lexer.
public final class GroovyLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"\"\"([\\s\\S]*?)\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("'''([\\s\\S]*?)'''", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:def|class|interface|trait|enum|package|import|as|extends|implements|new|return|if|else|for|while|switch|case|break|continue|try|catch|finally|throw|in|true|false|null)\\b", action: .token(.keyword)),

                // GString interpolation
                Rule("\\$\\{[^\\}]+\\}", action: .token(.name.child("Variable"))),
                Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|!=|<=|>=|\\+\\+|--|&&|\\|\\||->|=~|\\?\\.|\\*\\.|[=+\\-*/%<>!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
