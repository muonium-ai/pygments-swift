import Foundation

/// Pragmatic Liquid template lexer (smoke-test level).
public final class LiquidLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Liquid comments
                .rule(Rule("\\{%-?\\s*comment\\b.*?endcomment\\s*-?%\\}", options: [.dotMatchesLineSeparators, .caseInsensitive], action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\{#.*?#\\}", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Tags / output
                .rule(Rule("\\{%-?.*?-?%\\}", options: [.dotMatchesLineSeparators], action: .token(.other))),
                .rule(Rule("\\{\\{.*?\\}\\}", options: [.dotMatchesLineSeparators], action: .token(.string))),

                // HTML-ish comments
                .rule(Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Basic tags/attributes
                .rule(Rule("</?[A-Za-z][A-Za-z0-9:_-]*", action: .token(.name.child("Tag")))),
                .rule(Rule("/?>", action: .token(.name.child("Tag")))),
                .rule(Rule("[A-Za-z_:][A-Za-z0-9:._-]*", action: .token(.name.child("Attribute")))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
