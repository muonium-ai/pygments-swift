import Foundation

/// Pragmatic Procfile lexer (smoke-test level).
///
/// Procfile format is roughly: `<process_type>: <command>`
public final class ProcfileLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Process type at beginning of line.
                .rule(Rule("^([A-Za-z0-9_][A-Za-z0-9_-]*)(\\s*)(:)", options: [.anchorsMatchLines], action: .byGroups([
                    .name.child("Function"), .whitespace, .punctuation,
                ]))),

                // Anything after the ':' is the command.
                .rule(Rule("[^\\n]+", action: .token(.text))),
            ],
        ]
    }
}
