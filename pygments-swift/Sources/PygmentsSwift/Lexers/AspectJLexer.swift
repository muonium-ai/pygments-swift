import Foundation

/// Pragmatic AspectJ lexer (smoke-test level).
///
/// AspectJ is largely Java with additional pointcut/advice keywords.
public final class AspectJLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            // Java-ish
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char",
            "class", "const", "continue", "default", "do", "double", "else", "enum",
            "extends", "final", "finally", "float", "for", "goto", "if", "implements",
            "import", "instanceof", "int", "interface", "long", "native", "new",
            "package", "private", "protected", "public", "return", "short", "static",
            "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
            "transient", "try", "void", "volatile", "while", "var", "record", "sealed", "permits",
            // AspectJ additions
            "aspect", "pointcut", "before", "after", "around", "declare", "precedence", "privileged",
            "within", "withincode", "execution", "call", "get", "set", "args", "target", "thisJoinPoint"
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false", "null"], suffix: "\\b")
        let ident = #"[_$\p{XID_Start}][_$\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Annotations
                .rule(Rule("@" + ident, action: .token(.name.child("Decorator")))),

                // class/interface/enum/aspect names (must run before generic keyword matching)
                .rule(Rule("(class|interface|enum|aspect)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword.child("Declaration"), .whitespace, .name.child("Class")
                ]))),

                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Strings / chars
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Numbers
                .rule(Rule("0[xX][0-9a-fA-F_]+[lL]?", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+[lL]?", action: .token(.number.child("Bin")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?[fFdD]?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|\\*\\*)", action: .token(.operator))),
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
                .rule(Rule("[^\\\\\"\n]+", action: .token(.string))),
                .rule(Rule("\\\\.", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("\\n", action: .token(.string))),
            ],
            "sq": [
                .rule(Rule("[^\\\\'\n]+", action: .token(.string.child("Char")))),
                .rule(Rule("\\\\.", action: .token(.string.child("Char")))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule("\\n", action: .token(.string.child("Char")))),
            ],
        ]
    }
}
