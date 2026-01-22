import Foundation

/// Plain text lexer.
public final class TextLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        [
            "root": [
                .rule(Rule("\\s+", action: .token(.whitespace))),
                .rule(Rule("[^\\s]+", action: .token(.text))),
            ],
        ]
    }
}
