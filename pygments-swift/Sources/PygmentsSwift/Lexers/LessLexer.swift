import Foundation

/// Minimal Less lexer.
public final class LessLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("@[A-Za-z_][A-Za-z0-9_-]*", action: .token(.name.child("Variable"))),

                Rule("'[^']*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:true|false)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:%|px|em|rem|vh|vw|s|ms)?", action: .token(.number)),
                Rule("#[0-9A-Fa-f]{3,8}\\b", action: .token(.number)),

                Rule("[{}();:,\\[\\]]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[+\\-*/%<>!=]", action: .token(.operator)),

                Rule("[A-Za-z_-][A-Za-z0-9_-]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
