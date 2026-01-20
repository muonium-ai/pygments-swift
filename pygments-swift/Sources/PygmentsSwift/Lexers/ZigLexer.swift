import Foundation

/// Minimal Zig lexer.
public final class ZigLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//![^\\n]*", action: .token(.comment.child("Single"))),
                Rule("///[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0b[01_]+|0o[0-7_]+|0x[0-9A-Fa-f_]+|\\d[0-9_]*(?:\\.[0-9_]+)?)\\b", action: .token(.number)),

                Rule("\\b(?:fn|var|const|if|else|while|for|return|struct|enum|union|pub|comptime|usingnamespace|defer|errdefer|try|catch|switch|orelse|break|continue)\\b", action: .token(.keyword)),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),
                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|!=|<=|>=|<<|>>|&&|\\|\\||\\?\\?|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
