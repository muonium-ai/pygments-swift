import Foundation

/// Pragmatic AutoHotkey lexer (smoke-test level).
public final class AutoHotkeyLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        // AutoHotkey v1-ish basics.
        let keywords = [
            "if", "else", "return", "break", "continue", "loop", "while", "for", "in",
            "global", "local", "static", "byref", "class", "extends", "new", "try", "catch", "finally", "throw",
        ]
        let keywordPattern = "\\b(?:" + keywords.joined(separator: "|") + ")\\b"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Hotkeys / labels
                .rule(Rule("^[^\\S\\n]*[^\\s:]+::", options: [.anchorsMatchLines], action: .token(.name.child("Label")))),
                .rule(Rule("^[^\\S\\n]*[A-Za-z_#@$][A-Za-z0-9_#@$]*:\\s*$", options: [.anchorsMatchLines], action: .token(.name.child("Label")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywordPattern, action: .token(.keyword))),

                // Built-in vars/functions (very partial)
                .rule(Rule("\\bA_[A-Za-z0-9_]+\\b", action: .token(.name.child("Builtin")))),

                // Operators / punctuation
                .rule(Rule(":=|\\+=|-=|\\*=|/=|==|!=|<=|>=|&&|\\|\\||[-=+*/%<>]", action: .token(.operator))),
                .rule(Rule("[()\\[\\]{},.]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule("[A-Za-z_#@$][A-Za-z0-9_#@$]*", action: .token(.name))),

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
