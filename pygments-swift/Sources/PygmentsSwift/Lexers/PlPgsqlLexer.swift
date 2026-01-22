import Foundation

/// Pragmatic PL/pgSQL lexer (smoke-test level).
public final class PlPgsqlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let plKeywords = RegexHelpers.words([
            // SQL-ish
            "select", "from", "where", "insert", "into", "values", "update", "set", "delete",
            "create", "alter", "drop", "table", "view", "function", "procedure", "do",
            // PL/pgSQL
            "declare", "begin", "end", "loop", "while", "for", "in", "if", "then", "elsif", "else", "end if",
            "raise", "notice", "exception", "perform", "execute", "return", "returns", "language"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Dollar-quoted blocks (non-nested)
                .rule(Rule("\\$\\$.*?\\$\\$", options: [.dotMatchesLineSeparators], action: .token(.string))),

                // Strings
                .rule(Rule("'(?:''|[^'])*'", action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Variables
                .rule(Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable")))),

                // Numbers
                .rule(Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number))),

                .rule(Rule(plKeywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("\\b(?:true|false|null)\\b", options: [.caseInsensitive], action: .token(.keyword.child("Constant")))),

                .rule(Rule("[(),.;]", action: .token(.punctuation))),
                .rule(Rule(":=|[+*/<>=~!]+", action: .token(.operator))),
                .rule(Rule("[A-Za-z_][A-Za-z0-9_$$]*", action: .token(.name))),

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
