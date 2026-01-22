import Foundation

/// Pragmatic DTD lexer (smoke-test level).
public final class DtdLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Declarations
                .rule(Rule("<!\\s*(ELEMENT|ATTLIST|ENTITY|NOTATION|DOCTYPE)\\b", options: [.caseInsensitive], action: .token(.keyword))),
                .rule(Rule(">", action: .token(.punctuation))),
                .rule(Rule("<!\\[CDATA\\[.*?\\]\\]>", options: [.dotMatchesLineSeparators, .caseInsensitive], action: .token(.string))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Names
                .rule(Rule("%?[-A-Za-z_:][-A-Za-z0-9_:.]*", action: .token(.name))),

                // Punctuation
                .rule(Rule("[()\\[\\]|,*+?]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
