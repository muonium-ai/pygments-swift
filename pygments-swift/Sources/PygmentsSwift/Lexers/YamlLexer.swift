import Foundation

/// Minimal YAML lexer.
public final class YamlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Document markers
                Rule("^(---|\\.\\.\\.)(?=\\s|$)", options: [.anchorsMatchLines], action: .token(.punctuation)),

                // Key: value
                Rule(
                    "^(\\s*)([A-Za-z_][A-Za-z0-9_-]*)(\\s*)(:)(?=\\s|$)",
                    options: [.anchorsMatchLines],
                    action: .byGroups([.whitespace, .name.child("Attribute"), .whitespace, .punctuation])
                ),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^']|'')*'", action: .token(.string)),

                // Booleans / null
                Rule("\\b(?:true|false|null|~)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                // Numbers
                Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                // Punctuation / indicators
                Rule("[:\\[\\]\\{\\},&*?!|>-]", action: .token(.punctuation)),

                // Plain scalars
                Rule("[^\\s#]+", action: .token(.text)),
            ]
        ]
    }
}
