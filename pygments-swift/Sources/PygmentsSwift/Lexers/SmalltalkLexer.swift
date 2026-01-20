import Foundation

/// Pragmatic Smalltalk lexer (smoke-test level).
///
/// Highlights Smalltalk comments (double quotes), strings, symbols,
/// pseudo-variables, numbers, and basic punctuation/operators.
public final class SmalltalkLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let pseudo = RegexHelpers.words([
            "self", "super", "true", "false", "nil", "thisContext",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments (Smalltalk uses double quotes)
                .rule(Rule("\"", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),

                // Symbols
                .rule(Rule("#\\(", action: .token(.punctuation))),
                .rule(Rule("#[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Constant")))),

                // Character literal
                .rule(Rule("\\$.", action: .token(.string.child("Char")))),

                // Pseudo vars
                .rule(Rule(pseudo, action: .token(.keyword.child("Constant")))),

                // Numbers
                .rule(Rule("\\d+(?:r[0-9A-Fa-f]+)?(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Assignment / return
                .rule(Rule(":=|\\^", action: .token(.operator))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("[+\\-*/=<>~&|@%]+", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "comment": [
                .rule(Rule("\"", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^\"]+", action: .token(.comment.child("Multiline")))),
            ],

            "sq": [
                .rule(Rule("''", action: .token(.string))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^']+", action: .token(.string))),
            ],
        ]
    }
}
