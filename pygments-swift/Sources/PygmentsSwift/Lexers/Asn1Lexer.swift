import Foundation

/// Pragmatic ASN.1 lexer (smoke-test level).
public final class Asn1Lexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "BEGIN", "END", "DEFINITIONS", "AUTOMATIC", "EXPLICIT", "IMPLICIT",
            "IMPORTS", "EXPORTS", "FROM",
            "SEQUENCE", "SET", "CHOICE", "OF",
            "INTEGER", "BOOLEAN", "NULL", "OCTET", "STRING", "BIT", "OBJECT", "IDENTIFIER",
            "ENUMERATED", "REAL", "UTCTime", "GeneralizedTime",
            "OPTIONAL", "DEFAULT",
        ], suffix: "\\b")

        let ident = #"[A-Za-z][A-Za-z0-9-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers / types
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("(::=)", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
