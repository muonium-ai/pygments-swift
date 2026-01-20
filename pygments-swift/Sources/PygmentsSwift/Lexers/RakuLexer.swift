import Foundation

/// Minimal Raku (Perl 6) lexer.
public final class RakuLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\r?\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("[$@%&]" + ident, action: .token(.name.child("Variable"))),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:my|our|has|state|sub|method|multi|class|role|module|grammar|use|need|constant|enum|if|else|elsif|for|while|given|when|default|return|next|last|redo|try|catch|CATCH|throw|die|true|false|Nil)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),

                Rule("==|!=|<=|>=|~~|::|\\.\\.|&&|\\|\\||=>|[=+\\-*/%<>&^|!?:~.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
