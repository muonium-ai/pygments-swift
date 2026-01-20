import Foundation

/// Minimal F# lexer.
public final class FSharpLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("\\(\\*.*?\\*\\)", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("@\"(?:[^\"]|\"\")*\"", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:namespace|module|open|type|member|let|rec|in|fun|match|with|if|then|else|for|while|do|done|yield|return|use|try|finally|exception|new)\\b", action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[A-Za-z_][A-Za-z0-9_']*", action: .token(.name)),

                Rule("::|->|<-|:=|==|!=|<=|>=|\\|\\||&&|[=+\\-*/%<>!&|^~?:]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
