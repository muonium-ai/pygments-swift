import Foundation

/// Pragmatic CodeQL lexer (QL; smoke-test level).
public final class CodeQLLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = [
            "from", "where", "select", "import", "as", "and", "or", "not", "in",
            "exists", "forall", "if", "then", "else", "true", "false",
            "class", "predicate", "module", "this", "super",
        ]
        let keywordPattern = "\\b(?:" + keywords.joined(separator: "|") + ")\\b"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywordPattern, action: .token(.keyword))),

                // Operators / punctuation
                .rule(Rule("<=|>=|==|!=|=|<|>|\\+|\\-|\\*|/|%|\\.|::|:\\-|->", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;:]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

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
