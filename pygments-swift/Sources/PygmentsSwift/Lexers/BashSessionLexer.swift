import Foundation

/// Pragmatic Bash session lexer (smoke-test level).
///
/// Highlights prompts and shell-like comments.
public final class BashSessionLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        [
            "root": [
                // Prompt + command
                .rule(Rule("^([^\\n]*?[$#])(\\s+)(.*)$", options: [.anchorsMatchLines], action: .byGroups([
                    .generic.child("Prompt"), .whitespace, .text,
                ]))),

                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments inside the command text
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
