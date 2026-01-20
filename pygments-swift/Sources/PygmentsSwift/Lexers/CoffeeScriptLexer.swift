import Foundation

/// Minimal CoffeeScript lexer.
public final class CoffeeScriptLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("###(?:[\\s\\S]*?)###", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("'''[\\s\\S]*?'''", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("\"\"\"[\\s\\S]*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:true|false|null|undefined|yes|no|on|off)\\b", options: [.caseInsensitive], action: .token(.keyword)),
                Rule("\\b(?:if|then|else|unless|for|while|until|loop|switch|when|try|catch|finally|throw|return|break|continue|class|extends|new|in|of|is|isnt|not|and|or)\\b", action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),
                Rule("[A-Za-z_\\$][A-Za-z0-9_\\$]*", action: .token(.name)),

                Rule("==|!=|<=|>=|&&|\\|\\||->|=>|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{},;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
