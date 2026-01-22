import Foundation

/// Pragmatic BASIC (QBasic-ish) lexer (smoke-test level).
public final class BasicLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "dim", "redim", "as", "integer", "long", "double", "single", "string", "boolean",
            "print", "input", "let", "const", "sub", "end", "function", "return",
            "if", "then", "else", "elseif", "endif", "for", "to", "step", "next",
            "while", "wend", "do", "loop", "until", "select", "case", "endselect"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("'(?:[^\\n]*)", action: .token(.comment.child("Single")))),
                .rule(Rule("\\bREM\\b[^\\n]*", options: [.caseInsensitive], action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"(?:\"\"|[^\"])*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("==|<>|<=|>=|=|[-+*/%<>]", action: .token(.operator))),
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*\\$?", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
