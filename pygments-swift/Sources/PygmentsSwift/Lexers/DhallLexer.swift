import Foundation

/// Pragmatic lexer for Dhall (`.dhall`).
public final class DhallLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("--[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("\\{-[\\s\\S]*?-\\}", action: .token(.comment)),

                Rule("\\b(let|in|if|then|else|with|merge|forall|assert|as)\\b", action: .token(.keyword)),
                Rule("\\b(Bool|Natural|Integer|Double|Text|List|Optional)\\b", action: .token(.keyword)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("-?\\d+(?:\\.\\d+)?", action: .token(.number)),

                Rule("\\->|\\\\\\\\|==|!=|<=|>=|=|:|\\+|\\-|\\*|/|<|>", action: .token(.operator)),
                Rule("[\\{\\}\\[\\]\\(\\),.;]", action: .token(.punctuation)),

                Rule("[A-Za-z_][A-Za-z0-9_\\-]*", action: .token(.name)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
