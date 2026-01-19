import Foundation

/// Minimal INI-style lexer (example).
///
/// This is intentionally small: it's mainly a proving-ground for the RegexLexer engine.
public final class IniLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule("[;#][^\\n]*", action: .token(.comment.child("Single"))),

                // Section: [section]
                Rule("(\\[)([^\\]\\n]+)(\\])", action: .byGroups([.punctuation, .name.child("Label"), .punctuation])),

                // key = value (enter value state after the '=')
                Rule(
                    "([A-Za-z_][A-Za-z0-9_-]*)(\\s*)(=)(\\s*)",
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator, .whitespace]),
                    newState: .ops([.push("value")])
                ),
            ],

            "value": [
                // Consume value until newline, then pop back to root.
                Rule("([^\\n]*)(\\n)", action: .byGroups([.string, .whitespace]), newState: .ops([.pop]))
            ]
        ]
    }
}
