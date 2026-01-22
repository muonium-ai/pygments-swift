import Foundation

/// Pragmatic Angular2 template lexer (smoke-test level).
///
/// Highlights HTML-ish tags plus Angular template expressions.
public final class Angular2Lexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        // Common Angular template keywords used inside expressions.
        let exprKeywords = RegexHelpers.words([
            "let", "as", "true", "false", "null", "undefined", "if", "else", "for", "of"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // HTML comments
                .rule(Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Angular template comments (rare but seen in some toolchains)
                .rule(Rule("\\{#.*?#\\}", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Interpolations
                .rule(Rule("\\{\\{.*?\\}\\}", options: [.dotMatchesLineSeparators], action: .token(.string))),

                // Tags
                .rule(Rule("</?[A-Za-z][A-Za-z0-9:_-]*", action: .token(.name.child("Tag")))),
                .rule(Rule("/?>", action: .token(.name.child("Tag")))),

                // Angular binding-ish attribute starters
                .rule(Rule("\\[\\([A-Za-z0-9:_-]+\\)\\]|\\([A-Za-z0-9:_-]+\\)|\\[[A-Za-z0-9:_-]+\\]|\\*[A-Za-z0-9:_-]+", action: .token(.name.child("Attribute")))),

                // Regular attribute names
                .rule(Rule("[A-Za-z_:][A-Za-z0-9:._-]*", action: .token(.name.child("Attribute")))),
                .rule(Rule("=", action: .token(.operator))),

                // Attribute values
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Some expression keywords if they appear outside {{ }} (e.g. microsyntax)
                .rule(Rule(exprKeywords, action: .token(.keyword))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
