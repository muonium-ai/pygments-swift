import Foundation

/// Minimal Erlang lexer.
public final class ErlangLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("%[^\\n]*", action: .token(.comment.child("Single"))),

                // Attributes
                Rule("^(-)(module|export|import|define)(?=\\()", options: [.anchorsMatchLines], action: .byGroups([.punctuation, .keyword])),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:-?\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:fun|receive|after|case|of|end|if|when|try|catch|throw|exit)\\b", action: .token(.keyword)),

                // Variables start uppercase or underscore
                Rule("(?:_|[A-Z])[A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                // Atoms / function names
                Rule("[a-z][A-Za-z0-9_]*", action: .token(.name)),

                Rule("->|==|/=|=<|>=|:=|=:=|=/=|[=+\\-*/<>:]+", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
