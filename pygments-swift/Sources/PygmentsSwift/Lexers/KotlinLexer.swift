import Foundation

/// Pragmatic Kotlin lexer (smoke-test level).
///
/// Highlights common Kotlin tokens: comments, strings (incl. triple-quoted),
/// keywords, types, annotations, numbers, and identifiers.
public final class KotlinLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "as", "break", "class", "continue", "do", "else", "false", "for", "fun",
            "if", "in", "interface", "is", "null", "object", "package", "return",
            "super", "this", "throw", "true", "try", "typealias", "val", "var",
            "when", "while",
            // modifiers / misc
            "abstract", "annotation", "companion", "const", "crossinline", "data",
            "dynamic", "enum", "expect", "external", "final", "infix", "inline",
            "inner", "internal", "lateinit", "noinline", "open", "operator", "out",
            "override", "private", "protected", "public", "reified", "sealed",
            "suspend", "tailrec", "vararg",
        ], suffix: "\\b")

        let builtinTypes = RegexHelpers.words([
            "Any", "Unit", "Nothing",
            "Boolean", "Byte", "Short", "Int", "Long", "Float", "Double", "Char",
            "String",
        ], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Annotations
                .rule(Rule("@" + ident, action: .token(.name.child("Decorator")))),

                // Strings
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.push("tdq")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtinTypes, action: .token(.keyword.child("Type")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F_]+[lL]?", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+[lL]?", action: .token(.number.child("Bin")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?[fFdDlL]?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|===|!==|\\+\\+|--|->|::|\\+=|-=|\\*=|/=|%=|&&|\\|\\|)", action: .token(.operator))),
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

            "tdq": [
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^\"]+", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string))),
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
