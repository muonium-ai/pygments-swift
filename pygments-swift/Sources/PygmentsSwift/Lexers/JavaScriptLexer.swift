import Foundation

/// Pragmatic JavaScript lexer (smoke-test level).
///
/// Highlights common JS tokens: comments, strings (including template literals),
/// keywords, numbers, identifiers.
public final class JavaScriptLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "await", "break", "case", "catch", "class", "const", "continue", "debugger",
            "default", "delete", "do", "else", "export", "extends", "finally", "for",
            "function", "if", "import", "in", "instanceof", "let", "new", "return",
            "super", "switch", "this", "throw", "try", "typeof", "var", "void",
            "while", "with", "yield",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false", "null", "undefined"], suffix: "\\b")

        let ident = #"[$_\p{XID_Start}][$_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("`", action: .token(.string), newState: .ops([.push("tmpl")]))),

                // Keywords / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers
                .rule(Rule("0[xX][0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0[oO][0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(===|!==|==|!=|<=|>=|<<|>>|\\*\\*)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
            "tmpl": [
                .rule(Rule("`", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("\\$\\{", action: .token(.string.child("Interpol")), newState: .ops([.push("interp")]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\`$]+"#, action: .token(.string))),
                .rule(Rule("\\$", action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
            "interp": [
                .rule(Rule("\\{", action: .token(.punctuation), newState: .ops([.pushCurrent]))),
                .rule(Rule("\\}", action: .token(.punctuation), newState: .ops([.pop]))),
                .include("root")
            ]
        ]
    }
}
