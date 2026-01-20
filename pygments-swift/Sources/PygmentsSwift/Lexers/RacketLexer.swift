import Foundation

/// Minimal Racket lexer.
public final class RacketLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = #"[A-Za-z_+\-*/?<>=!][A-Za-z0-9_+\-*/?<>=!]*"#
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // #lang line
                Rule("^#lang\\b[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Preproc"))),

                // Comments
                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#\\|[\\s\\S]*?\\|#", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Numbers
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                // Keywords (very small set)
                Rule("\\b(?:define|lambda|let|let\\*|letrec|if|cond|case|begin|set!|require|provide|module)\\b", action: .token(.keyword)),

                Rule("[()]", action: .token(.punctuation)),
                Rule(ident, action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
