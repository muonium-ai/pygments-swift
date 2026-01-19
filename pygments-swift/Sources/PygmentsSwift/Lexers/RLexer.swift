import Foundation

/// Pragmatic R lexer (smoke-test level).
///
/// Highlights common R tokens: comments, strings, keywords, builtins, numbers,
/// operators, and identifiers.
public final class RLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "if", "else", "repeat", "while", "for", "in",
            "function", "return", "next", "break",
        ], suffix: "\\b")

        let constants = RegexHelpers.words([
            "NULL", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
            "TRUE", "FALSE", "Inf", "NaN",
        ], suffix: "\\b")

        let builtins = RegexHelpers.words([
            "c", "list", "matrix", "array", "data.frame",
            "library", "require", "source",
            "print", "cat", "message", "stop", "warning",
            "setwd", "getwd",
        ], suffix: "\\b")

        // R identifiers:
        // - can start with letter or '.', but '.' must not be followed by a digit.
        // - allow Unicode identifiers using XID properties.
        let ident = #"(?:[\p{XID_Start}]|\.(?!\d)[\p{XID_Start}_])[\p{XID_Continue}._]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings / symbols
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule("`", action: .token(.name), newState: .ops([.push("bt")]))),

                // Keywords / constants / builtins
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),
                .rule(Rule(builtins, action: .token(.name.child("Builtin")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?[iL]?", action: .token(.number))),
                .rule(Rule("\\.\\d+(?:[eE][+\\-]?\\d+)?[iL]?", action: .token(.number))),

                // Operators (including %op%)
                .rule(Rule("%[^%\\s]+%", action: .token(.operator))),
                .rule(Rule("(<-|<<-|->>|->|\\+=|\\-=|\\*=|\\/=|==|!=|<=|>=|&&|\\|\\||:::{0,1}|:)", action: .token(.operator))),
                .rule(Rule("[+\\-*/^$@~<>]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),

                // Punctuation
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            // Backticked symbol names
            "bt": [
                .rule(Rule("`", action: .token(.name), newState: .ops([.pop]))),
                .rule(Rule(#"[^`]+"#, action: .token(.name))),
            ],
        ]
    }
}
