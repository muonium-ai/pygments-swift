import Foundation

/// Pragmatic Chapel lexer (smoke-test level).
public final class ChapelLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = [
            "proc", "iter", "class", "record", "module", "use", "import", "config",
            "var", "const", "param", "type", "ref",
            "if", "then", "else", "select", "when",
            "for", "forall", "while", "do",
            "return", "yield", "break", "continue",
            "try", "catch", "throw", "throws",
            "in", "by", "with",
            "true", "false", "nil",
        ]
        let keywordPattern = "\\b(?:" + keywords.joined(separator: "|") + ")\\b"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string.child("Char")))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywordPattern, action: .token(.keyword))),

                // Operators / punctuation
                .rule(Rule("\\.{2,3}|==|!=|<=|>=|&&|\\|\\||<<|>>|[-+*/%]=?|[=<>]", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;:]", action: .token(.punctuation))),

                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),
                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
