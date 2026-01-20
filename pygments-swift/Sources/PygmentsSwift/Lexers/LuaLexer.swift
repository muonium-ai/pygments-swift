import Foundation

/// Minimal Lua lexer.
public final class LuaLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule("--\\[\\[.*?\\]\\]", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("--[^\\n]*", action: .token(.comment.child("Single"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),
                Rule("\\[\\[.*?\\]\\]", options: [.dotMatchesLineSeparators], action: .token(.string)),

                // Numbers
                Rule("0x[0-9A-Fa-f]+", action: .token(.number)),
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                // Keywords
                Rule("\\b(?:and|break|do|else|elseif|end|false|for|function|if|in|local|nil|not|or|repeat|return|then|true|until|while)\\b", action: .token(.keyword)),

                // Identifiers
                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),
                Rule("[+\\-*/%^#=<>~:]+", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
