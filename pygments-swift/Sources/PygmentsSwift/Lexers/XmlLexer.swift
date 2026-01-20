import Foundation

/// Minimal XML lexer.
public final class XmlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("<!\\[CDATA\\[.*?\\]\\]>", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("<\\?.*?\\?>", options: [.dotMatchesLineSeparators], action: .token(.comment)),
                Rule("<!DOCTYPE[^>]*>", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("(<\\s*/\\s*)([A-Za-z_][A-Za-z0-9:._-]*)(\\s*>)", action: .byGroups([.punctuation, .name.child("Tag"), .punctuation])),
                Rule("(<\\s*)([A-Za-z_][A-Za-z0-9:._-]*)", action: .byGroups([.punctuation, .name.child("Tag")]), newState: .ops([.push("tag")]) ),

                Rule("&[A-Za-z_][A-Za-z0-9._-]*;", action: .token(.name.child("Entity"))),
                Rule("[^<&\\n]+", action: .token(.text)),
                Rule(".", action: .token(.text)),
            ],

            "tag": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule(
                    "([A-Za-z_:][A-Za-z0-9:._-]*)(\\s*)(=)",
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator])
                ),
                Rule("\"[^\"]*\"", action: .token(.string)),
                Rule("'[^']*'", action: .token(.string)),

                Rule("\\s*/?>", action: .token(.punctuation), newState: .ops([.pop]) ),
                Rule("[^\\s>]+", action: .token(.string)),
            ],
        ]
    }
}
