import Foundation

/// Pragmatic Ruby lexer (smoke-test level).
///
/// Highlights common Ruby tokens: comments, strings, symbols, regex literals
/// (very naive), keywords, numbers, and identifiers.
public final class RubyLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "BEGIN", "END", "alias", "and", "begin", "break", "case", "class", "def",
            "defined?", "do", "else", "elsif", "end", "ensure", "false", "for",
            "if", "in", "module", "next", "nil", "not", "or", "redo", "rescue",
            "retry", "return", "self", "super", "then", "true", "undef", "unless",
            "until", "when", "while", "yield",
        ], suffix: "\\b")

        let ident = #"[@$]?[_\p{XID_Start}][_\p{XID_Continue}]*[!?=]?"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),

                // Symbols
                .rule(Rule(":(" + ident + ")", action: .byGroups([.punctuation, .name.child("Constant")]))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Numbers
                .rule(Rule("0[xX][0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?", action: .token(.number))),

                // Regex literal (very naive; may mis-tokenize division)
                .rule(Rule("/(?:\\\\/|[^/\\n])+/[a-z]*", action: .token(.string.child("Regex")))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|\\+\\+|--|=>|::|\\.\\.)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),

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
        ]
    }
}
