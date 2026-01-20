import Foundation

/// Pragmatic lexer for Vue single-file components (`.vue`).
///
/// This is intentionally lightweight: tags/attrs/strings/comments are highlighted,
/// but embedded script/style contents are not deeply parsed yet.
public final class VueLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("<!--[\\s\\S]*?-->", action: .token(.comment)),

                Rule("(</?)([A-Za-z][A-Za-z0-9:_-]*)", action: .byGroups([.punctuation, .name.child("Tag")])),
                Rule("/?>", action: .token(.punctuation)),

                Rule("\\b([A-Za-z_:][A-Za-z0-9:._-]*)(\\s*)(=)", action: .byGroups([.name.child("Attribute"), .whitespace, .operator])),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\"\"\"", action: .token(.string)),
                Rule("\\{\\{|\\}\\}", action: .token(.punctuation)),

                Rule("[^<\\n]+", action: .token(.text)),
                Rule("<", action: .token(.punctuation)),
            ]
        ]
    }
}
