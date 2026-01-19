import Foundation

/// Pragmatic C# lexer (smoke-test level).
///
/// Highlights common C# tokens: comments, strings (including verbatim),
/// keywords, types, attributes, numbers, and identifiers.
public final class CSharpLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "abstract", "as", "base", "break", "case", "catch", "checked", "class",
            "const", "continue", "default", "delegate", "do", "else", "enum", "event",
            "explicit", "extern", "false", "finally", "fixed", "for", "foreach", "goto",
            "if", "implicit", "in", "interface", "internal", "is", "lock", "namespace",
            "new", "null", "operator", "out", "override", "params", "private", "protected",
            "public", "readonly", "ref", "return", "sealed", "sizeof", "stackalloc",
            "static", "struct", "switch", "this", "throw", "true", "try", "typeof",
            "unchecked", "unsafe", "using", "virtual", "void", "volatile", "while",
            // newer-ish
            "record", "init", "with", "var", "dynamic", "async", "await",
        ], suffix: "\\b")

        let builtinTypes = RegexHelpers.words([
            "bool", "byte", "sbyte", "short", "ushort", "int", "uint", "long", "ulong",
            "char", "float", "double", "decimal", "string", "object",
        ], suffix: "\\b")

        // C# identifiers can be prefixed with @ to escape keywords.
        let ident = #"@?[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Attributes
                .rule(Rule("\\[", action: .token(.punctuation), newState: .ops([.push("attribute")]))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("@\"", action: .token(.string), newState: .ops([.push("verbatim")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtinTypes, action: .token(.keyword.child("Type")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F_]+[uUlL]*", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?[mMdDfFuUlL]*", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[(){}\\[\\]:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|=>|\\?\\?|\\?\\.|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=)", action: .token(.operator))),
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

            "verbatim": [
                .rule(Rule("\"\"", action: .token(.string.child("Escape")))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"[^\"]+"#, action: .token(.string))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string.child("Char")))),
                .rule(Rule("\\\\", action: .token(.string.child("Char")))),
            ],

            "attribute": [
                .rule(Rule("\\]", action: .token(.punctuation), newState: .ops([.pop]))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule(ident, action: .token(.name.child("Decorator")))),
                .rule(Rule("[(){}:.,;]", action: .token(.punctuation))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule(".", action: .token(.text))),
            ]
        ]
    }
}
