import Foundation

/// Minimal Clojure lexer.
public final class ClojureLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:-?\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                // Keywords (Clojure forms)
                Rule("\\b(?:def|defn|defmacro|let|if|do|fn|ns|require|use|cond|case|loop|recur|when|when-not|->|->>|doseq|dotimes)\\b", action: .token(.keyword)),

                // :keyword
                Rule(":[A-Za-z_\\-][A-Za-z0-9_\\-]*", action: .token(.name.child("Tag"))),

                // symbols
                Rule("[A-Za-z_\\-+*/!?=><][A-Za-z0-9_\\-+*/!?=><]*", action: .token(.name)),

                Rule("[()\\[\\]{}]", action: .token(.punctuation)),
                Rule("'|`|~@?|@|\\^", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
