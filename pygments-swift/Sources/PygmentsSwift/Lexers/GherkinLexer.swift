import Foundation

/// Pragmatic lexer for Gherkin (`.feature`) files.
public final class GherkinLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("@[A-Za-z0-9_-]+", action: .token(.name.child("Decorator"))),

                Rule(
                    "(^|\\n)([\\t ]*)(Feature|Background|Scenario Outline|Scenario|Examples|Given|When|Then|And|But)(\\b)",
                    action: .byGroups([.whitespace, .whitespace, .keyword, .whitespace])
                ),

                Rule("\"[^\"\\n]*\"", action: .token(.string)),
                Rule("\"", action: .token(.punctuation)),
                Rule("\\b\\d+\\b", action: .token(.number)),
                Rule(":", action: .token(.punctuation)),

                Rule("[^\\n\"]+", action: .token(.text)),
            ]
        ]
    }
}
