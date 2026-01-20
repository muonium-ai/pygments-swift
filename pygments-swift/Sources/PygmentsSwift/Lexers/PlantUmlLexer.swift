import Foundation

/// Minimal PlantUML lexer.
public final class PlantUmlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("^@(?:startuml|enduml)\\b", options: [.anchorsMatchLines], action: .token(.keyword)),

                Rule("/'[\\s\\S]*?'/'", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("'[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:class|interface|enum|abstract|actor|participant|usecase|note|as|extends|implements|package|namespace|title|skinparam)\\b", action: .token(.keyword)),

                Rule("<\\|--|--\\|>|<\\|\\.\\.|\\.\\.\\||-\\[|\\]-|<->|->|<-|-->", action: .token(.operator)),

                Rule("[{}()\\[\\],:]", action: .token(.punctuation)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
