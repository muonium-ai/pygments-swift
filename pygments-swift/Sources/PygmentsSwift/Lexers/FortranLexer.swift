import Foundation

/// Minimal Fortran lexer.
public final class FortranLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("![^\\n]*", action: .token(.comment.child("Single"))),

                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:program|end|implicit|none|integer|real|logical|character|parameter|contains|function|subroutine|module|use|call|return|if|then|else|endif|do|enddo|print|stop)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("==|/=|<=|>=|\\.and\\.|\\.or\\.|[=+\\-*/%<>]", options: [.caseInsensitive], action: .token(.operator)),
                Rule("[()\\[\\]{},;:]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
