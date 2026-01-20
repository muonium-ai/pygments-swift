import Foundation

/// Pragmatic lexer for Hjson (`.hjson`).
public final class HjsonLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*[\\s\\S]*?\\*/", action: .token(.comment)),

                Rule("[\\{\\}\\[\\],:]", action: .token(.punctuation)),

                Rule("\\b(true|false|null)\\b", action: .token(.keyword)),
                Rule("-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", action: .token(.number)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                // Unquoted keys: key:
                Rule("([A-Za-z_][A-Za-z0-9_-]*)(\\s*)(:)", action: .byGroups([.name.child("Tag"), .whitespace, .punctuation])),

                Rule("[^\\n]+", action: .token(.text)),
            ]
        ]
    }
}
