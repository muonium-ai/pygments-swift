import Foundation

/// Minimal unified diff lexer.
public final class DiffLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("^diff\\b.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Heading"))),
                Rule("^index\\b.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Heading"))),
                Rule("^@@.*?@@.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Subheading"))),

                Rule("^---.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Heading"))),
                Rule("^\\+\\+\\+.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Heading"))),

                Rule("^\\+.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Inserted"))),
                Rule("^-.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Deleted"))),

                Rule("^\\s.*$", options: [.anchorsMatchLines], action: .token(.text)),
                Rule("^[^\\n]*$", options: [.anchorsMatchLines], action: .token(.text)),
            ]
        ]
    }
}
