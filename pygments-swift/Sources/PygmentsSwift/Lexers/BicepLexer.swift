import Foundation

/// Pragmatic lexer for Bicep (`.bicep`).
public final class BicepLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*[\\s\\S]*?\\*/", action: .token(.comment)),

                Rule("\\b(param|var|resource|module|output|import|targetScope|existing)\\b", action: .token(.keyword)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("-?\\d+(?:\\.\\d+)?", action: .token(.number)),

                Rule("==|!=|<=|>=|=>|=|\\?|\\.\\.|\\+|\\-|\\*|/|%|<|>", action: .token(.operator)),
                Rule("[\\{\\}\\[\\]\\(\\),:]", action: .token(.punctuation)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
