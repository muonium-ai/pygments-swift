import Foundation

/// Minimal .gitignore lexer.
public final class GitIgnoreLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("!", action: .token(.operator)),
                Rule("\\*\\*|\\*|\\?", action: .token(.operator)),
                Rule("/", action: .token(.punctuation)),

                Rule("\\[[^\\]]+\\]", action: .token(.string)),
                Rule("[^\\s#]+", action: .token(.string)),
            ]
        ]
    }
}
