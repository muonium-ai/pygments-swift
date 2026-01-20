import Foundation

/// Pragmatic PRQL lexer (smoke-test level).
public final class PrqlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "from", "select", "filter", "derive", "group", "aggregate", "sort", "take",
            "join", "inner", "left", "right",
            "let", "case", "when", "then", "else",
            "true", "false", "null",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#.*", action: .token(.comment.child("Single")))),
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"\"\".*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("(->|=>|==|!=|<=|>=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%<>!=|]", action: .token(.operator))),
                .rule(Rule("[(){}\\[\\],.:]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
