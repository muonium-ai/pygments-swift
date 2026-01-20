import Foundation

/// Minimal Crystal lexer.
public final class CrystalLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*[!?]?"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string.child("Char"))),

                Rule(":\\w+", action: .token(.string.child("Symbol"))),
                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:class|module|struct|enum|lib|alias|type|def|macro|end|if|then|else|elsif|unless|case|when|while|until|for|in|do|begin|rescue|ensure|return|break|next|redo|retry|yield|include|extend|require|self|nil|true|false|private|protected|public)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|\\*\\*|&&|\\|\\||\\+\\+|--|=>|\\.\\.|[=+\\-*/%<>&^|!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
