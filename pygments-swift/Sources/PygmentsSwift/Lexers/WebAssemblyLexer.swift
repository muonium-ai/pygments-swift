import Foundation

/// Pragmatic WebAssembly text format lexer (WAT) (smoke-test level).
public final class WebAssemblyLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "module", "func", "param", "result", "local", "global",
            "memory", "data", "type", "import", "export", "start",
            "table", "elem", "mut",
        ], suffix: "\\b")

        let ident = #"\$[A-Za-z0-9_.$<>\-]*"#
        let instr = #"[A-Za-z][A-Za-z0-9_\-\.]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";;[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("\\(;.*?;\\)", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),

                // Identifiers ($name)
                .rule(Rule(ident, action: .token(.name.child("Variable")))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d+)(?:\\.\\d+)?", action: .token(.number))),

                // Keywords / instructions
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(instr, action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{}]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
