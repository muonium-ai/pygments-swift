import Foundation

/// Pragmatic Gnuplot lexer (smoke-test level).
public final class GnuplotLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "plot", "splot", "set", "unset", "show", "load", "save", "fit",
            "if", "else", "do", "while", "for", "in", "using", "with", "title"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("==|!=|<=|>=|[-+*/%&|^~<>!?]=?", action: .token(.operator))),

                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),
                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
