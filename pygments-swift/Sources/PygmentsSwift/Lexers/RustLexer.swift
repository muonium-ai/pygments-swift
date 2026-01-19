import Foundation

/// Pragmatic Rust lexer (smoke-test level).
///
/// Highlights common Rust tokens: comments, attributes, strings (incl. raw-ish),
/// lifetimes, macros, keywords, types, numbers, and identifiers.
public final class RustLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else",
            "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop",
            "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self",
            "static", "struct", "super", "trait", "true", "type", "unsafe", "use",
            "where", "while",
            // newer-ish
            "yield",
        ], suffix: "\\b")

        let builtinTypes = RegexHelpers.words([
            "bool", "char", "str",
            "i8", "i16", "i32", "i64", "i128", "isize",
            "u8", "u16", "u32", "u64", "u128", "usize",
            "f32", "f64",
        ], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Attributes
                .rule(Rule("#!?\\[", action: .token(.name.child("Decorator")), newState: .ops([.push("attribute")]))),

                // Comments (order matters)
                .rule(Rule("///[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("//! [^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("r#*\"", action: .token(.string), newState: .ops([.push("rawstring")]))),
                .rule(Rule("b?\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Chars / lifetimes
                .rule(Rule("'" + ident, action: .token(.name.child("Label")))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtinTypes, action: .token(.keyword.child("Type")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F_]+(?:[iu](?:8|16|32|64|128|size))?", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+(?:[iu](?:8|16|32|64|128|size))?", action: .token(.number.child("Bin")))),
                .rule(Rule("0[oO][0-7_]+(?:[iu](?:8|16|32|64|128|size))?", action: .token(.number.child("Oct")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?(?:[iu](?:8|16|32|64|128|size)|f(?:32|64))?", action: .token(.number))),

                // Macro invocation: ident!
                .rule(Rule("(" + ident + ")(!)", action: .byGroups([.name, .punctuation]))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|->|=>|::|\\.\\.)", action: .token(.operator))),
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

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "rawstring": [
                // Pragmatic: close on any quote; does not enforce matching # count.
                .rule(Rule("\"#*", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"[^\"]+"#, action: .token(.string))),
                .rule(Rule("\"", action: .token(.string))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string.child("Char")))),
                .rule(Rule("\\\\", action: .token(.string.child("Char")))),
            ],

            "attribute": [
                .rule(Rule("\\]", action: .token(.name.child("Decorator")), newState: .ops([.pop]))),
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                .rule(Rule("#[!]?\\[", action: .token(.name.child("Decorator")))),
                .rule(Rule("[(){}:.,;]", action: .token(.punctuation))),
                .rule(Rule(ident, action: .token(.name.child("Decorator")))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),
                .rule(Rule(".", action: .token(.text))),
            ]
        ]
    }
}
