import Foundation

/// Pragmatic AutoIt lexer (smoke-test level).
public final class AutoItLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "if", "then", "else", "endif", "while", "wend", "for", "next", "to", "step",
            "do", "until", "func", "endfunc", "return", "exit", "continue", "select", "case", "endselect",
            "local", "global", "dim", "const", "byref", "with", "endwith"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("(?i)#cs\\b", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),
                .rule(Rule("(?i)#comments-start\\b", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Preprocessor-ish directives
                .rule(Rule("#[A-Za-z_][A-Za-z0-9_]*", action: .token(.comment.child("Preproc")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Variables
                .rule(Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable")))),

                // Operators / punctuation
                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("==|!=|<=|>=|<>|=|[+\\-*/&<>]", action: .token(.operator))),

                // Identifiers
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),
                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("(?i)#ce\\b", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("(?i)#comments-end\\b", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^#]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("#", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
