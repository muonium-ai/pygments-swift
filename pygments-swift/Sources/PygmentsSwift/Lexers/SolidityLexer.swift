import Foundation

/// Minimal Solidity lexer.
public final class SolidityLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("0x[0-9A-Fa-f]+", action: .token(.number)),
                Rule("\\b(?:\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule(
                    "\\b(?:pragma|solidity|contract|interface|library|function|returns|event|modifier|mapping|struct|enum|public|private|internal|external|view|pure|payable|memory|storage|calldata|uint|int|address|bool|string|bytes|emit|require|revert|return|if|else|for|while|break|continue|new)\\b",
                    action: .token(.keyword)
                ),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|!=|<=|>=|&&|\\|\\||->|=>|\\+\\+|--|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
