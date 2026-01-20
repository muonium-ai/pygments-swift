import Foundation

/// Minimal D lexer.
public final class DLexer: RegexLexer {
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
                Rule("`[^`]*`", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:module|import|static|shared|const|immutable|inout|enum|struct|class|interface|union|template|mixin|alias|typedef|pragma|extern|export|private|protected|public|package|final|override|abstract|synchronized|deprecated|lazy|auto|scope|pure|nothrow|@safe|@trusted|@system|if|else|while|for|foreach|do|switch|case|default|break|continue|return|try|catch|finally|throw|new|delete|this|super|true|false|null)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|=>|\\+\\+|--|&&|\\|\\||<<|>>|\\.\\.|[=+\\-*/%<>&^|!?:~.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
