import Foundation

/// Minimal Makefile lexer.
public final class MakefileLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                // Recipe lines start with a literal tab
                Rule("^\\t.*$", options: [.anchorsMatchLines], action: .token(.string)),

                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Target: deps
                Rule(
                    "^([A-Za-z0-9_./-]+)(\\s*)(:)([^\\n]*)$",
                    options: [.anchorsMatchLines],
                    action: .byGroups([.name.child("Function"), .whitespace, .punctuation, .text])
                ),

                // VAR = value
                Rule(
                    "^([A-Za-z0-9_]+)(\\s*)([:?+]?=)",
                    options: [.anchorsMatchLines],
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator])
                ),

                // $(VAR)
                Rule("\\$\\([^\\)]+\\)", action: .token(.name.child("Variable"))),

                Rule("[():=]", action: .token(.punctuation)),
                Rule("[^\\s#]+", action: .token(.text)),
            ]
        ]
    }
}
