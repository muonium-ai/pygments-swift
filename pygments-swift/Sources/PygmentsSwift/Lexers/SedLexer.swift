import Foundation

/// Minimal sed lexer.
public final class SedLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Addresses (very simplified)
                Rule("(\\d+)(,)(\\d+)([A-Za-z])", action: .byGroups([.number, .punctuation, .number, .keyword])),
                Rule("\\$|\\d+", action: .token(.number)),
                Rule("/(?:[^/\\\\]|\\\\.)+/", action: .token(.string.child("Regex"))),

                // s/// and y/// (very simplified)
                Rule("\\b(?:s|y)(?=/)", action: .token(.keyword)),

                // Common commands
                Rule("\\b(?:a|b|c|d|D|g|G|h|H|i|l|n|N|p|P|q|r|s|t|w|x|y)\\b", action: .token(.keyword)),

                Rule("[;{}]", action: .token(.punctuation)),

                // Strings in append/insert/change lines
                Rule("\\\\\\n", action: .token(.string.child("Escape"))),
                Rule("\\\\.", action: .token(.string.child("Escape"))),

                Rule("[^\\n]+", action: .token(.text)),
            ]
        ]
    }
}
