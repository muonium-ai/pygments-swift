import Foundation

/// Pragmatic lexer for KDL (`.kdl`).
public final class KdlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*[\\s\\S]*?\\*/", action: .token(.comment)),

                Rule("[\\{\\}\\(\\)\\[\\];]", action: .token(.punctuation)),
                Rule("=", action: .token(.operator)),

                Rule("\\b(true|false|null)\\b", action: .token(.keyword)),
                Rule("-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", action: .token(.number)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("([A-Za-z_][A-Za-z0-9_-]*)(\\s*)(=)", action: .byGroups([.name.child("Attribute"), .whitespace, .operator])),
                Rule("[A-Za-z_][A-Za-z0-9_-]*", action: .token(.name)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
