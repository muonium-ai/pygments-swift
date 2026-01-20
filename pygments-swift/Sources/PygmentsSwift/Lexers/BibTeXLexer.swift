import Foundation

/// Pragmatic BibTeX lexer (smoke-test level).
public final class BibTeXLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let entryType = #"[A-Za-z][A-Za-z0-9_-]*"#
        let ident = #"[A-Za-z_][A-Za-z0-9_:-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("%[^\\n]*", action: .token(.comment.child("Single")))),

                // Entry start: @type{ or @type(
                .rule(Rule("(@)(" + entryType + ")(\\s*)([({])", action: .byGroups([
                    .punctuation,
                    .keyword,
                    .whitespace,
                    .punctuation,
                ]))),

                // Field names
                .rule(Rule("\\b(" + ident + ")(\\s*)(=)", action: .byGroups([
                    .name.child("Attribute"),
                    .whitespace,
                    .operator,
                ]))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("\\{[^{}\\n]*\\}", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Identifiers (macros)
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation
                .rule(Rule("[{},()#]", action: .token(.punctuation))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
