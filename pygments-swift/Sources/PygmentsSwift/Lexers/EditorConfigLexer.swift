import Foundation

/// Minimal .editorconfig lexer.
public final class EditorConfigLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("[;#][^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\\[[^\\]]+\\]", action: .token(.keyword)),

                // key = value
                Rule("^([A-Za-z0-9_.-]+)(\\s*)(=)", options: [.anchorsMatchLines], action: .byGroups([
                    .name.child("Attribute"),
                    .whitespace,
                    .operator,
                ])),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("[^\\n]+", action: .token(.string)),
            ]
        ]
    }
}
