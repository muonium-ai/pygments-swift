import Foundation

/// Pragmatic AsciiDoc lexer (smoke-test level).
public final class AsciiDocLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Block comments (//// ... ////)
                .rule(Rule("////\\n.*?\\n////", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Line comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),

                // Titles / section headers
                .rule(Rule("^(={1,6})\\s+.*$", options: [.anchorsMatchLines], action: .token(.generic.child("Heading")))),

                // Attribute entries
                .rule(Rule("^:[^:\\n]+:.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Preproc")))),

                // Links
                .rule(Rule("https?://[^\\s\\])]+", action: .token(.name.child("Namespace")))),

                // Inline code
                .rule(Rule("`[^`\\n]+`", action: .token(.string.child("Backtick")))),

                // Bold / italic-ish markup
                .rule(Rule("\\*[^\\*\\n]+\\*", action: .token(.generic.child("Strong")))),
                .rule(Rule("_[^_\\n]+_", action: .token(.generic.child("Emph")))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
