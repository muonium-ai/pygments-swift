import Foundation

/// Minimal Mermaid diagram lexer.
public final class MermaidLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("%%[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\\b(?:graph|flowchart|sequenceDiagram|classDiagram|stateDiagram|stateDiagram-v2|erDiagram|journey|gantt|pie)\\b", action: .token(.keyword)),

                Rule("-->|---|==>|<--|<==|\\.\\.>|-\\.->", action: .token(.operator)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("\\[[^\\]]+\\]", action: .token(.string)),
                Rule("\\([^\\)]+\\)", action: .token(.string)),

                Rule("[{}|:;]", action: .token(.punctuation)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
