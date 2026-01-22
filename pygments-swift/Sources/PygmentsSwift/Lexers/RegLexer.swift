import Foundation

/// Pragmatic Windows Registry (.reg) lexer (smoke-test level).
public final class RegLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),

                // Header
                .rule(Rule("^Windows Registry Editor Version[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Preproc")))),

                // Section / key
                .rule(Rule("^\\[[^\\]\\n]+\\]", options: [.anchorsMatchLines], action: .token(.name.child("Namespace")))),

                // Value name
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Types / builtins
                .rule(Rule("\\b(?:dword|qword|hex|hex\\([0-9]+\\)|string|expand_sz|multi_sz)\\b", options: [.caseInsensitive], action: .token(.keyword.child("Type")))),

                // Operators / punctuation
                .rule(Rule("=|:|,", action: .token(.operator))),
                .rule(Rule("\\[|\\]", action: .token(.punctuation))),

                // Hex bytes / numbers
                .rule(Rule("\\b[0-9A-Fa-f]{2}\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b0x[0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
