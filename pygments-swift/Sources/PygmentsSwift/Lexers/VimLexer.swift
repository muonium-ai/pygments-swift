import Foundation

/// Minimal Vim script lexer.
public final class VimLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                // Comments (Vim uses " for comments)
                Rule("^\\s*\".*$", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),

                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("'[^']*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:let|set|if|elseif|else|endif|for|endfor|while|endwhile|function|endfunction|return|call|echo|finish|try|catch|finally|throw)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("[bgwtlsav]:[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),
                Rule("[A-Za-z_][A-Za-z0-9_:#]*", action: .token(.name)),

                Rule("==|!=|<=|>=|&&|\\|\\||[=+\\-*/<>!?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{},]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
