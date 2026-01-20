import Foundation

/// Minimal Graphviz DOT lexer.
public final class GraphvizLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:strict|graph|digraph|subgraph|node|edge)\\b", action: .token(.keyword)),

                Rule("->|--", action: .token(.operator)),
                Rule("=", action: .token(.operator)),

                Rule("[{}\\[\\](),;]", action: .token(.punctuation)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
