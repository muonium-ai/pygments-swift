import Foundation

/// Pragmatic Caddyfile lexer (smoke-test level).
public final class CaddyfileLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "import", "handle", "handle_path", "route",
            "respond", "rewrite", "redir", "try_files",
            "file_server", "reverse_proxy", "encode", "log", "tls",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Site addresses / placeholders
                .rule(Rule("\\{[^\\}\\n]+\\}", action: .token(.name.child("Variable")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Numbers / ports
                .rule(Rule(":\\d+", action: .token(.number))),

                // Braces / punctuation
                .rule(Rule("[{}(),]", action: .token(.punctuation))),

                // Identifiers and paths
                .rule(Rule("/[^\\s{}#]+", action: .token(.string.child("Path")))),
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
