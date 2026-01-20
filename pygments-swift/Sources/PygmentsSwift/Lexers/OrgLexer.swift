import Foundation

/// Pragmatic Org-mode lexer (smoke-test level).
public final class OrgLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "TODO", "DONE", "CANCELLED",
            "BEGIN_SRC", "END_SRC", "BEGIN_EXAMPLE", "END_EXAMPLE",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Headings: * [TODO] Heading
                .rule(Rule("^(\\*+)(\\s+)(?:(TODO|DONE|CANCELLED)(\\s+))?(.*)$", options: [.anchorsMatchLines], action: .byGroups([
                    .generic.child("Heading"),
                    .whitespace,
                    .keyword,
                    .whitespace,
                    .generic.child("Heading"),
                ]))),

                // Special directives: #+...
                .rule(Rule("^#\\+.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("#(?!\\+)[^\\n]*", action: .token(.comment.child("Single")))),

                // Inline code / verbatim
                .rule(Rule("~[^~\\n]+~", action: .token(.string.child("Backtick")))),
                .rule(Rule("=[^=\\n]+=", action: .token(.string.child("Backtick")))),

                // Keywords (inside directives and headlines)
                .rule(Rule(keywords, action: .token(.keyword))),

                // Links
                .rule(Rule("\\[\\[[^\\]]+\\]\\[[^\\]]+\\]\\]", action: .token(.name.child("Namespace")))),

                .rule(Rule(ident, action: .token(.name))),
                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
