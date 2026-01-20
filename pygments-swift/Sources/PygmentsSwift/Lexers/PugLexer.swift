import Foundation

/// Minimal Pug (Jade) lexer.
public final class PugLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("\\n", action: .token(.whitespace)),
                Rule("[\\t ]+", action: .token(.whitespace)),

                Rule("^//-?.*$", options: [.anchorsMatchLines], action: .token(.comment.child("Single"))),

                Rule("\\b(?:mixin|include|extends|block|append|prepend|if|else|each|while|case|when|default)\\b", action: .token(.keyword)),

                Rule("^([A-Za-z][A-Za-z0-9_-]*)\\b", options: [.anchorsMatchLines], action: .token(.name.child("Tag"))),

                Rule("#[{][^}]+[}]", action: .token(.name.child("Variable"))),

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
