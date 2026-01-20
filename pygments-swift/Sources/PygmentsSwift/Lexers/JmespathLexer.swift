import Foundation

/// Pragmatic JMESPath lexer (smoke-test level).
public final class JmespathLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "true", "false", "null",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Strings / JSON literals
                .rule(Rule("`[^`]*`", action: .token(.literal.child("String")))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("(==|!=|<=|>=)", action: .token(.operator))),
                .rule(Rule("[\\.\\[\\]{}(),:]", action: .token(.punctuation))),
                .rule(Rule("[?!@&|*]", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
