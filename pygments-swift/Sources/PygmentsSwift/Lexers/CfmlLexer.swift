import Foundation

/// Pragmatic CFML (Coldfusion markup) lexer (smoke-test level).
public final class CfmlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // CFML comments
                .rule(Rule("<!---.*?--->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),
                // HTML comments
                .rule(Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // CFML interpolation / expressions
                .rule(Rule("#.*?#", options: [.dotMatchesLineSeparators], action: .token(.string))),

                // CF tags
                .rule(Rule("</?cf[A-Za-z0-9:_-]*", action: .token(.name.child("Tag")))),
                .rule(Rule("/?>", action: .token(.name.child("Tag")))),

                // Attribute-ish
                .rule(Rule("[A-Za-z_:][A-Za-z0-9:._-]*", action: .token(.name.child("Attribute")))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
