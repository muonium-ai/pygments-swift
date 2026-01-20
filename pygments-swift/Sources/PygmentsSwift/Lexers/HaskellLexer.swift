import Foundation

/// Minimal Haskell lexer.
public final class HaskellLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("\\{-[\\s\\S]*?-\\}", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("--[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string)),

                Rule("\\b(?:\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:module|import|qualified|as|hiding|where|let|in|data|type|newtype|class|instance|deriving|if|then|else|case|of|do|forall)\\b", action: .token(.keyword)),

                Rule("[A-Z][A-Za-z0-9_']*", action: .token(.name.child("Class"))),
                Rule("[a-z_][A-Za-z0-9_']*", action: .token(.name)),

                Rule("::|->|=>|<-|=|\\\\|\\|", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
