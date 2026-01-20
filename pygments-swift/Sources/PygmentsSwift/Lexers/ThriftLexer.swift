import Foundation

/// Pragmatic Apache Thrift lexer (smoke-test level).
public final class ThriftLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "namespace", "include", "cpp_include",
            "typedef", "const",
            "enum", "struct", "union", "exception",
            "service", "extends", "oneway",
            "required", "optional",
            "throws",
            "void", "bool", "byte", "i16", "i32", "i64", "double", "string", "binary",
            "map", "list", "set",
            "true", "false",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Field ids like 1:
                .rule(Rule("\\b\\d+\\s*:", action: .token(.number))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[{}\\[\\]();,<>:=]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
