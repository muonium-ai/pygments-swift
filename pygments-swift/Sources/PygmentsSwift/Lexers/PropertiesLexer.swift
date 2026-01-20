import Foundation

/// Minimal Java .properties lexer.
public final class PropertiesLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("[#!][^\\n]*", action: .token(.comment.child("Single"))),

                // key[:=]value
                Rule("^([^\\s:=]+)(\\s*)([:=])", options: [.anchorsMatchLines], action: .byGroups([
                    .name.child("Attribute"),
                    .whitespace,
                    .operator,
                ])),

                Rule("\\\\u[0-9A-Fa-f]{4}", action: .token(.string)),
                Rule("\\\\.", action: .token(.string)),

                Rule("[^\\n]+", action: .token(.string)),
            ]
        ]
    }
}
