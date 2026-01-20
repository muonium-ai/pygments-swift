import Foundation

/// Minimal HTML lexer.
public final class HtmlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("<!DOCTYPE[^>]*>", options: [.caseInsensitive], action: .token(.keyword)),

                // Close tags
                Rule("(<\\s*/\\s*)([A-Za-z][A-Za-z0-9:-]*)(\\s*>)", action: .byGroups([.punctuation, .name.child("Tag"), .punctuation])),

                // Open tag start
                Rule("(<\\s*)([A-Za-z][A-Za-z0-9:-]*)", action: .byGroups([.punctuation, .name.child("Tag")]), newState: .ops([.push("tag")]) ),

                // Entities
                Rule("&[A-Za-z][A-Za-z0-9]*;", action: .token(.name.child("Entity"))),

                // Text
                Rule("[^<&\\n]+", action: .token(.text)),
                Rule(".", action: .token(.text)),
            ],

            "tag": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // attr=
                Rule(
                    "([A-Za-z_:][A-Za-z0-9:._-]*)(\\s*)(=)",
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator])
                ),

                // Strings
                Rule("\"[^\"]*\"", action: .token(.string)),
                Rule("'[^']*'", action: .token(.string)),

                // End of tag
                Rule("\\s*/?>", action: .token(.punctuation), newState: .ops([.pop]) ),

                // Bare attribute values
                Rule("[^\\s>]+", action: .token(.string)),
            ],
        ]
    }
}
