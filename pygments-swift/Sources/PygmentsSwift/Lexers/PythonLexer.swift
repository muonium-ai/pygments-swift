import Foundation

/// Pragmatic Python lexer.
///
/// This is a RegexLexer-based highlighter intended for common Python code.
/// It is not yet a full port of Pygments' PythonLexer (no full f-string / format
/// spec parser, etc.), but is good enough for basic syntax highlighting.
public final class PythonLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "False", "None", "True",
            "and", "as", "assert", "async", "await",
            "break", "class", "continue", "def", "del",
            "elif", "else", "except", "finally", "for", "from",
            "global", "if", "import", "in", "is", "lambda",
            "nonlocal", "not", "or", "pass", "raise", "return",
            "try", "while", "with", "yield",
        ], suffix: "\\b")

        let builtins = RegexHelpers.words([
            "abs", "all", "any", "bool", "bytearray", "bytes", "callable", "chr",
            "dict", "dir", "enumerate", "eval", "exec", "filter", "float", "format",
            "getattr", "hasattr", "hash", "help", "hex", "id", "int", "isinstance",
            "issubclass", "iter", "len", "list", "map", "max", "min", "next", "object",
            "oct", "open", "ord", "pow", "print", "range", "repr", "reversed", "round",
            "set", "slice", "sorted", "str", "sum", "tuple", "type", "zip"
        ], suffix: "\\b")

        // Identifier: Python 3 allows Unicode identifiers; we use XID properties.
        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        // String prefixes: r, u, b, f and combinations (common subset).
        let strPrefix = #"(?i:(?:r|u|b|f|fr|rf|br|rb)?)"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Decorators
                .rule(Rule("@" + ident, action: .token(.name.child("Decorator")))),

                // def / class names (must run before generic keyword matching)
                .rule(Rule("(def)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword, .whitespace, .name.child("Function")
                ]))),
                .rule(Rule("(class)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword, .whitespace, .name.child("Class")
                ]))),

                // Keywords / builtins
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtins, action: .token(.name.child("Builtin")))),

                // Numbers (simplified)
                .rule(Rule("0[bB][01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0[oO][0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("0[xX][0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("(?:\\d+_?)+\\.(?:\\d+_?)*(?:[eE][+\\-]?(?:\\d+_?)+)?", action: .token(.number.child("Float")))),
                .rule(Rule("(?:\\d+_?)+(?:[eE][+\\-]?(?:\\d+_?)+)", action: .token(.number.child("Float")))),
                .rule(Rule("(?:\\d+_?)+", action: .token(.number.child("Integer")))),

                // Strings: triple then single
                .rule(Rule(strPrefix + "'''", action: .token(.string), newState: .ops([.push("tsq")]))),
                .rule(Rule(strPrefix + "\"\"\"", action: .token(.string), newState: .ops([.push("tdq")]))),
                .rule(Rule(strPrefix + "'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule(strPrefix + "\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Operators / punctuation
                .rule(Rule("[()\\[\\]{}:.,;@]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\*\\*|//|:=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Fallback
                .rule(Rule(".", action: .token(.text))),
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
            "tsq": [
                .rule(Rule("'''", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
            "tdq": [
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
        ]
    }
}
