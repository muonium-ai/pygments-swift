import Foundation

/// Swift lexer based on Pygments' `SwiftLexer` (ported from `pygments.lexers.objective`).
public final class SwiftLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywordPattern = RegexHelpers.words([
            "as", "async", "await", "break", "case", "catch", "continue", "default", "defer",
            "do", "else", "fallthrough", "for", "guard", "if", "in", "is",
            "repeat", "return", "#selector", "switch", "throw", "try",
            "where", "while"
        ], suffix: "\\b")

        let reservedPattern = RegexHelpers.words([
            "associativity", "convenience", "dynamic", "didSet", "final",
            "get", "indirect", "infix", "inout", "lazy", "left", "mutating",
            "none", "nonmutating", "optional", "override", "postfix",
            "precedence", "prefix", "Protocol", "required", "rethrows",
            "right", "set", "throws", "Type", "unowned", "weak", "willSet",
            "@availability", "@autoclosure", "@noreturn",
            "@NSApplicationMain", "@NSCopying", "@NSManaged", "@objc",
            "@UIApplicationMain", "@IBAction", "@IBDesignable",
            "@IBInspectable", "@IBOutlet"
        ], suffix: "\\b")

        let declPattern = RegexHelpers.words([
            "actor", "associatedtype", "class", "deinit", "enum", "extension", "func", "import",
            "init", "internal", "let", "operator", "private", "protocol", "public",
            "static", "struct", "subscript", "typealias", "var"
        ], suffix: "\\b")

        let builtinTypesPattern = RegexHelpers.words([
            // Minimal subset to match our current tests; expand over time.
            "Any", "AnyObject", "Bool", "Character", "Double", "Float", "Int",
            "Int8", "Int16", "Int32", "Int64", "String", "UInt", "UInt8", "UInt16", "UInt32", "UInt64",
            "Void"
        ], suffix: "\\b")

        return [
            "root": [
                // Whitespace and Comments
                .rule(Rule("\\n", action: .token(.text))),
                .rule(Rule("\\s+", action: .token(.whitespace))),
                .rule(Rule("//", action: .token(.comment.child("Single")), newState: .ops([.push("comment-single")]))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment-multi")]))),
                .rule(Rule("#(if|elseif|else|endif|available)\\b", action: .token(.comment.child("Preproc")), newState: .ops([.push("preproc")]))),

                .include("keywords"),

                // Global Types (minimal subset)
                .rule(Rule(builtinTypesPattern, action: .token(.name.child("Builtin")))),

                // Implicit Block Variables
                .rule(Rule("\\$\\d+", action: .token(.name.child("Variable")))),

                // Numeric literals
                .rule(Rule("0b[01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0o[0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("0x[0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[0-9][0-9_]*(\\.[0-9_]+[eE][+\\-]?[0-9_]+|\\.[0-9_]*|[eE][+\\-]?[0-9_]+)", action: .token(.number.child("Float")))),
                .rule(Rule("[0-9][0-9_]*", action: .token(.number.child("Integer")))),

                // String literals
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.push("string-multi")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("string")]))),

                // Operators and punctuation
                .rule(Rule("[(){}\\[\\].,:;=@#`?]|->|[<&?](?=\\w)|(?<=\\w)[>!?]", action: .token(.punctuation))),
                .rule(Rule("[/=\\-+!*%<>&|^?~]+", action: .token(.operator))),

                // Identifier
                .rule(Rule("[a-zA-Z_]\\w*", action: .token(.name)))
            ],

            "keywords": [
                .rule(Rule(keywordPattern, action: .token(.keyword))),
                .rule(Rule("@availability\\([^)]+\\)", action: .token(.keyword.child("Reserved")))),
                .rule(Rule(reservedPattern, action: .token(.keyword.child("Reserved")))),
                .rule(Rule("(as|dynamicType|false|is|nil|self|Self|super|true|__COLUMN__|__FILE__|__FUNCTION__|__LINE__|_|#(?:file|line|column|function))\\b", action: .token(.keyword.child("Constant")))),
                .rule(Rule("import\\b", action: .token(.keyword.child("Declaration")), newState: .ops([.push("module")]))),
                .rule(Rule("(class|enum|extension|struct|protocol)(\\s+)([a-zA-Z_]\\w*)", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Class")]))),
                .rule(Rule("(func)(\\s+)([a-zA-Z_]\\w*)", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Function")]))),
                .rule(Rule("(var|let)(\\s+)([a-zA-Z_]\\w*)", action: .byGroups([.keyword.child("Declaration"), .whitespace, .name.child("Variable")]))),
                .rule(Rule(declPattern, action: .token(.keyword.child("Declaration"))))
            ],

            "comment": [
                .rule(Rule(":param: [a-zA-Z_]\\w*|:returns?:|(FIXME|MARK|TODO):", action: .token(.comment.child("Special"))))
            ],

            "comment-single": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .include("comment"),
                .rule(Rule("[^\\n]+", action: .token(.comment.child("Single"))))
            ],

            "comment-multi": [
                .include("comment"),
                .rule(Rule("[^*/]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.pushCurrent]))),
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[*/]+", action: .token(.comment.child("Multiline"))))
            ],

            "module": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .rule(Rule("[a-zA-Z_]\\w*", action: .token(.name.child("Class")))),
                .include("root")
            ],

            "preproc": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.pop]))),
                .include("keywords"),
                .rule(Rule("[A-Za-z]\\w*", action: .token(.comment.child("Preproc")))),
                .include("root")
            ],

            "string": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .include("string-common")
            ],

            "string-multi": [
                .rule(Rule("\"\"\"", action: .token(.string), newState: .ops([.pop]))),
                .include("string-common")
            ],

            "string-common": [
                .rule(Rule("\\\\\\(", action: .token(.string.child("Interpol")), newState: .ops([.push("string-intp")]))),
                .rule(Rule(#"\\['"\\nrt]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}"#, action: .token(.string.child("Escape")))),
                .rule(Rule("[^\\\\\"]+", action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string)))
            ],

            "string-intp": [
                .rule(Rule("\\(", action: .token(.string.child("Interpol")), newState: .ops([.pushCurrent]))),
                .rule(Rule("\\)", action: .token(.string.child("Interpol")), newState: .ops([.pop]))),
                .include("root")
            ]
        ]
    }
}
