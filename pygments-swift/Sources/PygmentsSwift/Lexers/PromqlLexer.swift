import Foundation

/// Pragmatic PromQL lexer (smoke-test level).
public final class PromqlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "by", "without", "on", "ignoring", "group_left", "group_right",
            "offset", "bool", "and", "or", "unless",
        ], suffix: "\\b")

        let metric = #"[A-Za-z_:][A-Za-z0-9_:]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#.*", action: .token(.comment.child("Single")))),

                // Strings (label matchers)
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Durations
                .rule(Rule("\\b\\d+(?:y|w|d|h|m|s|ms)\\b", action: .token(.number))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Metric / function names
                .rule(Rule(metric, action: .token(.name))),

                // Operators
                .rule(Rule("(==|!=|<=|>=|=~|!~)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%^]|=|<|>", action: .token(.operator))),

                // Punctuation
                .rule(Rule("[{}\\[\\](),.:]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
