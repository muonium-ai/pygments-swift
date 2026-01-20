import Foundation

/// Pragmatic CDDL lexer (smoke-test level).
public final class CddlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let ident = #"[$A-Za-z_][A-Za-z0-9_\-\.]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments (RFC8610 uses ';' line comments)
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Operators
                .rule(Rule("(=>|=|\\/\\/|\\/|\\*|\\+|\\?|\\^)", action: .token(.operator))),

                // Punctuation
                .rule(Rule("[{}\\[\\](),:]", action: .token(.punctuation))),

                // Identifiers / type names
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
