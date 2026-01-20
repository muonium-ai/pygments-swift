import Foundation

/// Pragmatic YANG lexer (smoke-test level).
public final class YangLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "module", "submodule", "namespace", "prefix", "yang-version",
            "organization", "contact", "description", "reference",
            "revision", "revision-date",
            "import", "include", "belongs-to",
            "typedef", "type", "identity", "feature",
            "container", "leaf", "leaf-list", "list", "choice", "case",
            "grouping", "uses",
            "rpc", "input", "output", "notification",
            "augment", "deviation",
            "when", "must", "config", "mandatory", "default", "units",
            "key", "unique", "ordered-by", "status", "presence",
            "min-elements", "max-elements", "if-feature",
            "path", "length", "pattern", "range", "enum", "bit", "position", "value",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\d+", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[\\{\\}();:]", action: .token(.punctuation))),
                .rule(Rule("[+\\-*/<>=!&|^~?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
