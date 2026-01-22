import Foundation

/// Pragmatic AppleScript lexer (smoke-test level).
public final class AppleScriptLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "tell", "end", "if", "then", "else", "repeat", "with", "to", "set", "return",
            "on", "script", "property", "try", "error", "of", "whose", "in", "as", "and", "or", "not"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("\\(\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{},.:]", action: .token(.punctuation))),
                .rule(Rule("[+\\-*/&=<>]", action: .token(.operator))),

                // Identifiers (AppleScript is more English-like; this is a minimal subset)
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("\\*\\)", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
