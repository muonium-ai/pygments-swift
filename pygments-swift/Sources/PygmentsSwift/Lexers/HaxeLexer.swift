import Foundation

/// Minimal Haxe lexer.
public final class HaxeLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string.child("Char"))),

                Rule("@:[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:package|import|using|class|interface|extends|implements|enum|typedef|abstract|extern|macro|inline|static|public|private|override|function|var|final|dynamic|if|else|while|do|for|in|switch|case|default|break|continue|return|try|catch|throw|new|this|super|true|false|null)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|=>|->|\\+\\+|--|&&|\\|\\||<<|>>|[=+\\-*/%<>&^|!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
