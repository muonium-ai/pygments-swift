import Foundation

/// Minimal Dart lexer.
public final class DartLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:abstract|as|assert|async|await|break|case|catch|class|const|continue|default|deferred|do|dynamic|else|enum|export|extends|extension|external|factory|false|final|finally|for|function|get|if|implements|import|in|interface|is|late|library|mixin|new|null|on|operator|part|rethrow|return|set|static|super|switch|sync|this|throw|true|try|typedef|var|void|while|with|yield)\\b", action: .token(.keyword)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|!=|<=|>=|=>|\\+\\+|--|&&|\\|\\||\\?\\?|<<|>>|[=+\\-*/%<>!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
