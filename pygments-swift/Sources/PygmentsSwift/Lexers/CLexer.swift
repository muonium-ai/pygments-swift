import Foundation

/// Pragmatic C lexer (smoke-test level).
///
/// Highlights common C tokens: comments, strings/chars, preprocessor, keywords,
/// types, numbers, and identifiers.
public final class CLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "auto", "break", "case", "const", "continue", "default", "do", "else",
            "enum", "extern", "for", "goto", "if", "inline", "register", "restrict",
            "return", "signed", "sizeof", "static", "struct", "switch", "typedef",
            "union", "unsigned", "volatile", "while",
            // C11
            "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic",
            "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "void", "char", "short", "int", "long", "float", "double",
            "size_t", "ptrdiff_t", "wchar_t",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["NULL"], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Preprocessor (line-based)
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings / chars
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F']+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01']+", action: .token(.number.child("Bin")))),
                .rule(Rule("\\d+(?:'\\d+)*(?:\\.\\d+(?:'\\d+)*)?(?:[eE][+\\-]?\\d+)?[uUlLfF]*", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|->|&&|\\|\\||\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=)", action: .token(.operator))),
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
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string.child("Char")))),
                .rule(Rule("\\\\", action: .token(.string.child("Char")))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
        ]
    }
}
