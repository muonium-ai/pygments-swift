import Foundation

/// Pragmatic Kconfig lexer (smoke-test level).
public final class KconfigLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "config", "menuconfig", "menu", "endmenu", "choice", "endchoice",
            "bool", "boolean", "tristate", "int", "hex", "string",
            "depends", "on", "select", "imply", "default", "prompt",
            "help", "if", "endif", "source", "range", "option",
            "mainmenu", "comment",
            "y", "n", "m",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Symbols
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("\\(|\\)|\\[|\\]|\\{|\\}|,|:|;", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|&&|\\|\\|)", action: .token(.operator))),
                .rule(Rule("[=!<>+\\-*/%]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
