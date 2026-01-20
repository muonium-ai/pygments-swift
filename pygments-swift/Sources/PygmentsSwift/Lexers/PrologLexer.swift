import Foundation

/// Pragmatic Prolog lexer (smoke-test level).
///
/// Highlights common Prolog tokens: comments, atoms/strings, variables,
/// numbers, punctuation/operators, and a few directive keywords.
public final class PrologLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let directives = RegexHelpers.words([
            "dynamic", "discontiguous", "multifile", "module", "use_module",
            "initialization", "consult", "include",
        ], suffix: "\\b")

        let builtins = RegexHelpers.words([
            "true", "fail", "repeat", "once", "call", "catch", "throw",
        ], suffix: "\\b")

        let atom = #"[a-z][A-Za-z0-9_]*"#
        let variable = #"[_A-Z][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("%[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings / quoted atoms
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Directive/builtin keywords (as atoms)
                .rule(Rule(directives, action: .token(.keyword))),
                .rule(Rule(builtins, action: .token(.keyword.child("Constant")))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Operators / punctuation
                .rule(Rule("(:-|\\?-|-->|->)", action: .token(.operator))),
                .rule(Rule("[()\\[\\]{}|,.;]", action: .token(.punctuation))),
                .rule(Rule("(==|\\\\=|=\\.\\.|=:=|=\\=|=<|>=|<|>|=)" , action: .token(.operator))),
                .rule(Rule("[+\\-*/^]", action: .token(.operator))),

                // Variables and atoms
                .rule(Rule(variable, action: .token(.name.child("Variable")))),
                .rule(Rule(atom, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],

            "sq": [
                .rule(Rule("''", action: .token(.string))),
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule("[^']+", action: .token(.string))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
        ]
    }
}
