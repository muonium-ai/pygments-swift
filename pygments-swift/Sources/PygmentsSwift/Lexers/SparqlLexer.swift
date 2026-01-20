import Foundation

/// Pragmatic SPARQL lexer (smoke-test level).
public final class SparqlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "PREFIX", "BASE",
            "SELECT", "ASK", "CONSTRUCT", "DESCRIBE",
            "WHERE", "FROM", "NAMED",
            "FILTER", "OPTIONAL", "UNION", "GRAPH", "SERVICE", "MINUS",
            "BIND", "VALUES",
            "ORDER", "BY", "ASC", "DESC",
            "LIMIT", "OFFSET", "DISTINCT", "REDUCED",
            "a",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // IRIs
                .rule(Rule("<[^>\\n]*>", action: .token(.name.child("Namespace")))),

                // Prefixed names
                        .rule(Rule("[A-Za-z_][A-Za-z0-9_-]*:[A-Za-z0-9_.-]*", action: .token(.name.child("Namespace")))),

                // Variables
                .rule(Rule("[?$][A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable")))),

                // Strings
                .rule(Rule("\"\"\".*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords (SPARQL is case-insensitive, but we match common uppercase)
                .rule(Rule(keywords, action: .token(.keyword))),

                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[{}\\[\\]();.,]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|&&|\\|\\|)", action: .token(.operator))),
                .rule(Rule("[=<>+\\-*/!]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
