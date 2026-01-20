import Foundation

/// Minimal V (Vlang) lexer.
public final class VLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("`[^`]*`", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string.child("Char"))),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:module|import|pub|mut|fn|struct|interface|enum|type|const|return|if|else|for|in|match|or|break|continue|defer|unsafe|true|false|none)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|:=|\\+\\+|--|&&|\\|\\||<<|>>|\\.\\.|[=+\\-*/%<>&^|!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
