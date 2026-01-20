import Foundation

/// Pragmatic ABNF lexer (smoke-test level).
public final class AbnfLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let coreKeywords = [
            "ALPHA", "BIT", "CHAR", "CR", "CRLF", "CTL", "DIGIT", "DQUOTE", "HEXDIG",
            "HTAB", "LF", "LWSP", "OCTET", "SP", "VCHAR", "WSP",
        ]

        let keywords = "\\b(?:" + coreKeywords.joined(separator: "|") + ")\\b"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),

                // Rule names / assignments
                .rule(Rule("(<-)|(=\\/)|=", action: .token(.operator))),

                // Core terminals
                .rule(Rule(keywords, action: .token(.keyword))),

                // Strings and numeric values
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("%[bBdDxX][0-9A-Fa-f.-]+", action: .token(.number))),

                // Repetition / grouping / alternation
                .rule(Rule("[\\[\\]{}()/*]", action: .token(.punctuation))),
                .rule(Rule("\\|", action: .token(.operator))),

                // Identifiers
                .rule(Rule("[A-Za-z][A-Za-z0-9-]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
