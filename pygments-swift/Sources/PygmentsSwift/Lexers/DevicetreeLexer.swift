import Foundation

/// Pragmatic Devicetree source lexer (DTS) (smoke-test level).
public final class DevicetreeLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "/dts-v1/", "/memreserve/", "delete-node", "delete-property",
        ], suffix: "")

        let ident = #"[A-Za-z_][A-Za-z0-9_,+\-\.]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Angle-bracketed cell lists
                .rule(Rule("<[^>]*>", action: .token(.number))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Labels: foo:
                .rule(Rule("(" + ident + ")(:)", action: .byGroups([
                    .name.child("Label"),
                    .punctuation,
                ]))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Operators / punctuation
                .rule(Rule("[{}\\[\\]();=,<>]", action: .token(.punctuation))),
                .rule(Rule("[&@]", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
