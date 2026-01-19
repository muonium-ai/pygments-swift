import Foundation

/// Pragmatic Scala lexer (smoke-test level).
///
/// Highlights common Scala tokens: comments, strings (including triple-quoted),
/// annotations, keywords, numbers, operators, and identifiers.
public final class ScalaLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "abstract", "case", "catch", "class", "def", "do", "else", "enum", "extends",
            "false", "final", "finally", "for", "forSome", "given", "if", "implicit", "import",
            "lazy", "match", "new", "null", "object", "override", "package", "private",
            "protected", "return", "sealed", "super", "this", "throw", "trait", "true",
            "try", "type", "val", "var", "while", "with", "yield", "then",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "Any", "AnyRef", "Boolean", "Byte", "Char", "Double", "Float", "Int", "Long",
            "Nothing", "Null", "Short", "String", "Unit",
            "Option", "Either", "List", "Seq", "Vector", "Map", "Set",
        ], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Annotations
                .rule(Rule("@" + ident, action: .token(.name.child("Decorator")))),

                // Strings: triple quotes, then standard strings with optional interpolator prefix
                .rule(Rule("(?:[sfr]|raw)?\\\"\\\"\\\"", action: .token(.string), newState: .ops([.push("tqs")]))),
                .rule(Rule("(?:[sfr]|raw)?\\\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Keywords / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),

                // Numbers
                .rule(Rule("0[xX][0-9a-fA-F_]+[lL]?", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?[fFdD]?", action: .token(.number))),

                // Backticked identifiers
                .rule(Rule("`[^`]+`", action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("(=>|<-|::|==|!=|<=|>=|&&|\\|\\|)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),

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
                .rule(Rule("\\\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"$]+"#, action: .token(.string))),
                // basic interpolation marker
                .rule(Rule("\\$", action: .token(.string.child("Interpol")))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "tqs": [
                .rule(Rule("\\\"\\\"\\\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"[^"]+"#, action: .token(.string))),
                .rule(Rule("\\\"", action: .token(.string))),
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
