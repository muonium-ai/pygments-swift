import Foundation

/// Minimal Haml lexer.
public final class HamlLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("\\n", action: .token(.whitespace)),
                Rule("[\\t ]+", action: .token(.whitespace)),

                Rule("^-#.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),
                Rule("^/.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),

                Rule("^%[A-Za-z][A-Za-z0-9:_-]*", options: [.anchorsMatchLines], action: .token(.name.child("Tag"))),
                Rule("#[A-Za-z_][A-Za-z0-9_-]*", action: .token(.name.child("Attribute"))),
                Rule("\\.[A-Za-z_][A-Za-z0-9_-]*", action: .token(.name.child("Attribute"))),

                Rule("^(?:-|=)\\s*", options: [.anchorsMatchLines], action: .token(.keyword)),

                Rule("'[^']*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[(){}:,]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
