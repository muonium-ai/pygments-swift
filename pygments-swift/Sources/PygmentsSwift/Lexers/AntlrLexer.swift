import Foundation

/// Pragmatic ANTLR lexer (smoke-test level).
public final class AntlrLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "grammar", "lexer", "parser", "tree", "options", "tokens", "channels", "fragment",
            "returns", "locals", "throws", "catch", "finally", "import", "scope",
            "protected", "public", "private"
        ], suffix: "\\b")

        let ident = "[A-Za-z_][A-Za-z0-9_]*"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Rule definitions at beginning of line
                .rule(Rule("^([a-z]" + ident + ")(\\s*)(:)", options: [.anchorsMatchLines], action: .byGroups([
                    .name.child("Function"), .whitespace, .punctuation,
                ]))),
                .rule(Rule("^([A-Z]" + ident + ")(\\s*)(:)", options: [.anchorsMatchLines], action: .byGroups([
                    .name.child("Class"), .whitespace, .punctuation,
                ]))),

                // Strings
                .rule(Rule("'(?:\\\\.|[^'\\\\])*'", action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Actions / predicates (not nested)
                .rule(Rule("\\{[^\\}]*\\}", action: .token(.other))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Operators / punctuation
                .rule(Rule("->|::|\\.\\.|[()\\[\\]{},;.:|?*+~!=<>-]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
