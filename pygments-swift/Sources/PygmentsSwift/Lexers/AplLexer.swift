import Foundation

/// Pragmatic APL lexer (smoke-test level).
public final class AplLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments (⍝ is traditional; # is also used by some implementations)
                .rule(Rule("⍝[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"(?:\"\"|[^\"])*\"", action: .token(.string))),

                // Numbers (APL commonly uses ¯ for negative)
                .rule(Rule("(?:¯|-)?\\d+(?:\\.\\d+)?", action: .token(.number))),

                // Names
                .rule(Rule("[\\p{L}_][\\p{L}\\p{N}_]*", action: .token(.name))),

                // Operators / punctuation (catch-all for APL glyphs)
                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("[^\\s\\p{L}\\p{N}_\"]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
