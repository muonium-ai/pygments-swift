import Foundation

/// Pragmatic COBOL lexer (smoke-test level).
///
/// Highlights common COBOL tokens: comments, strings, keywords,
/// numbers, punctuation/operators, and identifiers.
public final class CobolLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "ACCEPT", "ADD", "CALL", "COMPUTE", "CONTINUE", "DATA", "DIVIDE", "DISPLAY",
            "ELSE", "END", "END-IF", "END-PERFORM", "END-EVALUATE", "EVALUATE", "GOBACK",
            "IDENTIFICATION", "IF", "MOVE", "MULTIPLY", "PERFORM", "PIC", "PICTURE",
            "PROCEDURE", "PROGRAM-ID", "RETURN", "RUN", "SECTION", "STOP", "SUBTRACT",
            "THEN", "UNTIL", "USING", "VALUE", "VARYING", "WHEN", "WORKING-STORAGE",
        ], suffix: "\\b")

        let ident = #"[A-Za-z][A-Za-z0-9-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Fixed-format full-line comment (very simplified)
                .rule(Rule(#"^\s*\*[^\n]*"#, options: [.anchorsMatchLines], action: .token(.comment.child("Single")))),
                // Inline comment marker
                .rule(Rule("\\*>[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Numbers
                .rule(Rule("\\d+(?:\\.\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(<=|>=|<>|=|\\+|\\-|\\*|/)", action: .token(.operator))),

                // Identifiers / words
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "dq": [
                .rule(Rule("\"\"", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^\"]+", action: .token(.string))),
            ],

            "sq": [
                .rule(Rule("''", action: .token(.string))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^']+", action: .token(.string))),
            ],
        ]
    }
}
