import Foundation

/// Minimal Elm lexer.
public final class ElmLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_']*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("--[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("\\{-.*?\\-\\}", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string.child("Char"))),

                Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number)),

                Rule("\\b(?:module|import|exposing|as|type|alias|port|let|in|case|of|if|then|else|infix|infixl|infixr)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("->|<-|==|/=|<=|>=|\\|>|<\\||\\+\\+|::|[=+\\-*/<>|:&.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
