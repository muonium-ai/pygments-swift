import Foundation

/// Minimal Scheme lexer.
public final class SchemeLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = #"[A-Za-z_+\-*/?<>=!][A-Za-z0-9_+\-*/?<>=!]*"#
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#\\|[\\s\\S]*?\\|#", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("#t|#f", action: .token(.keyword)),

                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("\\b(?:define|lambda|let|let\\*|letrec|if|cond|case|begin|set!|and|or|not|quote|quasiquote|unquote)\\b", action: .token(.keyword)),

                Rule("[()]", action: .token(.punctuation)),
                Rule(ident, action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
