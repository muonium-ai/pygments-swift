import Foundation

/// Minimal Common Lisp lexer.
public final class CommonLispLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = #"[A-Za-z_+\-*/?<>=!&$%][A-Za-z0-9_+\-*/?<>=!&$%]*"#
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#\\|[\\s\\S]*?\\|#", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Keywords (common forms)
                Rule("\\b(?:defun|defmacro|defparameter|defvar|let|let\\*|lambda|if|cond|case|loop|progn|setq|quote|function|labels|flet)\\b", action: .token(.keyword)),

                // Packages and keywords like :foo
                Rule(":\\w+", action: .token(.name.child("Tag"))),

                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("[()]", action: .token(.punctuation)),
                Rule(ident, action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
