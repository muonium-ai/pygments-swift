import Foundation

/// Pragmatic Typst lexer (smoke-test level).
public final class TypstLexer: RegexLexer {
    public override func getTokensUnprocessed(_ text: String, stack: [String] = ["root"]) -> [Token] {
        // Start in a beginning-of-line state so we can correctly detect headings.
        // The core RegexLexer engine anchors ^/$ to the current search range,
        // so we implement BOL-sensitive rules via explicit states.
        if stack == ["root"] {
            return super.getTokensUnprocessed(text, stack: ["root", "bol"])
        }
        return super.getTokensUnprocessed(text, stack: stack)
    }

    public override var tokenDefs: [String: [TokenRuleDef]] {
        let ident = #"[A-Za-z_][A-Za-z0-9_\-]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace), newState: .ops([.push("bol")]))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Code mode prefix (#)
                .rule(Rule("#", action: .token(.punctuation))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)", action: .token(.number))),

                // Emphasis-ish markup
                .rule(Rule("\\*[^\\*\\n]+\\*", action: .token(.generic.child("Strong")))),
                .rule(Rule("_[^_\\n]+_", action: .token(.generic.child("Emph")))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation
                .rule(Rule("[(){}\\[\\],.:;]", action: .token(.punctuation))),
                .rule(Rule("[+\\-*/=<>]", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],

            // Beginning-of-line rules.
            "bol": [
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),
                .rule(Rule("=+\\s+[^\\n]*", action: .token(.generic.child("Heading")), newState: .ops([.pop]))),
                .default(.ops([.pop])),
            ],
        ]
    }
}
