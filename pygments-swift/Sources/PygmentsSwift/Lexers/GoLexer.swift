import Foundation

/// Pragmatic Go lexer (smoke-test level).
///
/// Highlights common Go tokens: comments, raw/interpreted strings, runes,
/// keywords, builtin types, numbers, and identifiers.
public final class GoLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "break", "case", "chan", "const", "continue", "default", "defer", "else",
            "fallthrough", "for", "func", "go", "goto", "if", "import", "interface",
            "map", "package", "range", "return", "select", "struct", "switch", "type",
            "var",
        ], suffix: "\\b")

        let builtinTypes = RegexHelpers.words([
            "bool", "byte", "complex64", "complex128", "error", "float32", "float64",
            "int", "int8", "int16", "int32", "int64", "rune", "string",
            "uint", "uint8", "uint16", "uint32", "uint64", "uintptr",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false", "iota", "nil"], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("`", action: .token(.string), newState: .ops([.push("raw")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Runes
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtinTypes, action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0[oO][0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|:=|<-|\\+\\+|--|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=|&\\^=)", action: .token(.operator))),
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

            "raw": [
                .rule(Rule("`", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^`]+", action: .token(.string))),
                .rule(Rule("`", action: .token(.string))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string.child("Char")))),
                .rule(Rule("\\\\", action: .token(.string.child("Char")))),
            ],
        ]
    }
}
