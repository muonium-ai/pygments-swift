import Foundation

/// Minimal TOML lexer.
public final class TomlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Tables: [table] or [[table]]
                Rule(
                    "^(\\[\\[?)([^\\]\\n]+)(\\]\\]?)(?=\\s|$)",
                    options: [.anchorsMatchLines],
                    action: .byGroups([.punctuation, .name.child("Label"), .punctuation])
                ),

                // Key =
                Rule(
                    "^([A-Za-z0-9_-]+)(\\s*)(=)",
                    options: [.anchorsMatchLines],
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator])
                ),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'])*'", action: .token(.string)),

                // Booleans
                Rule("\\b(?:true|false)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                // Datetimes (very loose)
                Rule("\\d{4}-\\d{2}-\\d{2}(?:[Tt ]\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?(?:Z|[+-]\\d{2}:\\d{2})?)?", action: .token(.number)),

                // Numbers
                Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("[\\[\\]\\{\\}(),.]", action: .token(.punctuation)),
                Rule("[^\\s#]+", action: .token(.text)),
            ]
        ]
    }
}
