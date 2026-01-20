import Foundation

/// Minimal Nix lexer.
public final class NixLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_'\\\\-]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("''.*?''", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number)),

                Rule("\\b(?:let|in|with|rec|inherit|if|then|else|assert|or)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|->|\\+\\+|//|[=+\\-*/%<>&|!?@:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
