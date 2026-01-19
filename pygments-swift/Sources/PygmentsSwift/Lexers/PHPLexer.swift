import Foundation

/// Pragmatic PHP lexer (smoke-test level).
///
/// Highlights common PHP tokens: comments, strings, variables, keywords,
/// numbers, operators, and namespaces.
public final class PHPLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "__halt_compiler",
            "abstract", "and", "array", "as", "break",
            "callable", "case", "catch", "class", "clone", "const", "continue",
            "declare", "default", "die", "do", "echo", "else", "elseif", "empty",
            "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile",
            "eval", "exit", "extends", "final", "finally", "fn", "for", "foreach",
            "function", "global", "goto", "if", "implements", "include", "include_once",
            "instanceof", "insteadof", "interface", "isset", "list",
            "match", "namespace", "new", "or", "print", "private", "protected", "public",
            "readonly", "require", "require_once", "return", "static", "switch",
            "throw", "trait", "try", "unset", "use", "var", "while", "xor", "yield", "yield from",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false", "null"], suffix: "\\b")

        let magicConstants = RegexHelpers.words([
            "__CLASS__", "__DIR__", "__FILE__", "__FUNCTION__", "__LINE__",
            "__METHOD__", "__NAMESPACE__", "__TRAIT__",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "bool", "boolean", "int", "integer", "float", "double", "string",
            "array", "object", "callable", "iterable", "mixed", "void", "never",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_\p{XID_Start}][A-Za-z0-9_\p{XID_Continue}]*"#
        let qname = #"\\?\#(ident)(?:\\\#(ident))*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // PHP open/close tags
                .rule(Rule("<\\?(?:php|=)?", action: .token(.comment.child("Preproc")))),
                .rule(Rule("\\?>", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("`", action: .token(.string.child("Backtick")), newState: .ops([.push("bq")]))),

                // Declarations (simple)
                .rule(Rule("(class|interface|trait)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword, .whitespace, .name.child("Class"),
                ]))),
                .rule(Rule("(function)(\\s+)(?:&\\s*)?(" + ident + ")", action: .byGroups([
                    .keyword, .whitespace, .name.child("Function"),
                ]))),
                .rule(Rule("(namespace|use)(\\s+)(" + qname + ")", action: .byGroups([
                    .keyword, .whitespace, .name.child("Namespace"),
                ]))),

                // Keywords / constants / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),
                .rule(Rule(magicConstants, action: .token(.name.child("Constant")))),

                // Variables
                .rule(Rule("\\$this\\b", action: .token(.name.child("Builtin").child("Pseudo")))),
                .rule(Rule("\\$(?:" + ident + ")", action: .token(.name.child("Variable")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01_]+", action: .token(.number.child("Bin")))),
                .rule(Rule("0[0-7_]+", action: .token(.number.child("Oct")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?", action: .token(.number))),

                // Operators / punctuation
                .rule(Rule("(===|!==|==|!=|<=|>=|<<|>>|\\*\\*|\\.\\.|=>|->|::|\\?\\?|\\?\\?=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),

                // Names / identifiers (including namespaces)
                .rule(Rule(qname, action: .token(.name))),
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
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

                // Interpolation (very simplified)
                .rule(Rule("\\$\\{[^}]*\\}", action: .token(.name.child("Variable")))),
                .rule(Rule("\\$(?:" + ident + ")", action: .token(.name.child("Variable")))),

                .rule(Rule(#"[^\\\\\"$]+"#, action: .token(.string))),
                .rule(Rule("\\$", action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "bq": [
                .rule(Rule("`", action: .token(.string.child("Backtick")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\`]+"#, action: .token(.string.child("Backtick")))),
                .rule(Rule("\\\\", action: .token(.string.child("Backtick")))),
            ],
        ]
    }
}
