import Foundation

/// Minimal Emacs Lisp lexer.
public final class EmacsLispLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = #"[A-Za-z_+\-*/?<>=!&$%][A-Za-z0-9_+\-*/?<>=!&$%]*"#
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:defun|defmacro|defvar|defconst|let|let\\*|lambda|if|cond|when|unless|progn|setq|quote|interactive|require|provide)\\b", action: .token(.keyword)),

                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("[()]", action: .token(.punctuation)),
                Rule(ident, action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
