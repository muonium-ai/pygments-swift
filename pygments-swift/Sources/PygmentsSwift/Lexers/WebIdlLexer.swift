import Foundation

/// Pragmatic WebIDL lexer (smoke-test level).
public final class WebIdlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "interface", "dictionary", "enum", "typedef", "callback", "namespace",
            "partial", "readonly", "attribute", "const", "static", "constructor",
            "inherit", "includes", "implements", "mixin",
            "iterable", "maplike", "setlike",
            "getter", "setter", "deleter", "stringifier",
            "optional", "required",
            // common types / modifiers
            "unsigned", "long", "short", "double", "float", "boolean", "void", "any",
            "object", "DOMString", "USVString",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Extended attributes
                .rule(Rule("\\[[^\\]]+\\]", action: .token(.name.child("Attribute")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\d+", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]\\{\\},;<>]", action: .token(.punctuation))),
                .rule(Rule("[=:+\\-*/]", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
