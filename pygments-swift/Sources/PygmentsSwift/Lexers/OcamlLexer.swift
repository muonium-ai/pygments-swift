import Foundation

/// Minimal OCaml lexer.
public final class OcamlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("\\(\\*.*?\\*\\)", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("'(?:[^'\\\\]|\\\\.)'", action: .token(.string.child("Char"))),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:let|rec|in|fun|function|match|with|type|module|struct|sig|end|open|if|then|else|begin|and|or|try|exception|raise|when)\\b", action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[A-Z][A-Za-z0-9_']*", action: .token(.name.child("Class"))),
                Rule("[a-z_][A-Za-z0-9_']*", action: .token(.name)),

                Rule("::|->|<-|:=|==|!=|<=|>=|\\|\\||&&|[=+\\-*/%<>!&|^~?:]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
