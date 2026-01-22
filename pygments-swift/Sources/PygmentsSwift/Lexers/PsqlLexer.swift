import Foundation

/// Pragmatic PostgreSQL console (psql) lexer (smoke-test level).
public final class PsqlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let sqlKeywords = RegexHelpers.words([
            "select", "from", "where", "insert", "into", "values", "update", "set", "delete",
            "create", "alter", "drop", "table", "view", "function", "procedure", "do",
            "begin", "end", "declare", "language", "returns", "return", "as", "distinct",
            "join", "left", "right", "inner", "outer", "on", "group", "by", "order",
            "having", "limit", "offset", "and", "or", "not", "null", "is", "in", "exists"
        ], suffix: "\\b")

        return [
            "root": [
                // Prompts (dbname=>, dbname=#, etc.)
                .rule(Rule("^([A-Za-z0-9_\\-]+(?:\\([^\\)]+\\))?[#=>-]*>)(\\s+)", options: [.anchorsMatchLines], action: .byGroups([
                    .generic.child("Prompt"), .whitespace,
                ]))),

                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // psql backslash commands
                .rule(Rule("\\\\[A-Za-z_\\-]+", action: .token(.keyword.child("Pseudo")))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("'(?:''|[^'])*'", action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Variables
                .rule(Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable")))),

                // Numbers
                .rule(Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number))),

                .rule(Rule(sqlKeywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[(),.;]", action: .token(.punctuation))),
                .rule(Rule("[+*/<>=~!]+", action: .token(.operator))),
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
