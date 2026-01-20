import Foundation

/// Minimal CSS lexer.
public final class CssLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("@[_A-Za-z-]+", action: .token(.keyword)),

                // Selector chunk before '{'
                Rule("[^\\{\\}\\n]+(?=\\{)", action: .token(.name)),
                Rule("\\{", action: .token(.punctuation), newState: .ops([.push("block")]) ),

                Rule("\\}", action: .token(.punctuation)),
                Rule(".", action: .token(.text)),
            ],

            "block": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                // property:
                Rule("([A-Za-z-]+)(\\s*)(:)", action: .byGroups([.name.child("Attribute"), .whitespace, .punctuation])),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'[^']*'", action: .token(.string)),

                // Colors
                Rule("#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})\\b", action: .token(.number)),

                // Numbers with units
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:%|[a-zA-Z]+)?", action: .token(.number)),

                Rule("!important\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("[;,()\\[\\]]", action: .token(.punctuation)),
                Rule("\\}", action: .token(.punctuation), newState: .ops([.pop]) ),

                Rule("[_A-Za-z-][_A-Za-z0-9-]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ],
        ]
    }
}
