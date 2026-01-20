import Foundation

/// Pragmatic ActionScript lexer (smoke-test level).
public final class ActionScriptLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = [
            "as", "break", "case", "catch", "class", "const", "continue", "default",
            "delete", "do", "dynamic", "else", "extends", "final", "finally", "for",
            "function", "get", "if", "implements", "import", "in", "include", "instanceof",
            "interface", "internal", "is", "namespace", "native", "new", "override", "package",
            "private", "protected", "public", "return", "set", "static", "super", "switch",
            "this", "throw", "try", "typeof", "use", "var", "void", "while", "with",
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
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywordPattern, action: .token(.keyword))),

                // Operators / punctuation
                .rule(Rule("[-+*/%]=?|==?=?|!=?=?|<=|>=|&&|\\|\\||[?:~^&|]", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule("[A-Za-z_$][A-Za-z0-9_$]*", action: .token(.name))),

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
