import Foundation

/// Minimal Tcl lexer.
public final class TclLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        let ident = "[A-Za-z_][A-Za-z0-9_]*"
        return [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments (Tcl treats '#' as comment mostly at command start; this is a simplification)
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Strings and brace-quoted blocks
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("\\{", action: .token(.punctuation), newState: .ops([.push("brace")])),

                // Variables
                Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable"))),
                Rule("\\$" + ident, action: .token(.name.child("Variable"))),

                // Numbers
                Rule("0x[0-9A-Fa-f]+", action: .token(.number)),
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                // Keywords / common commands
                Rule("\\b(?:proc|if|then|else|elseif|for|foreach|while|switch|set|unset|incr|return|break|continue|namespace|variable)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),
                Rule("[()\\[\\].,;]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[+\\-*/%<>=]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
            ,
            "brace": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Nested braces
                Rule("\\{", action: .token(.punctuation), newState: .ops([.push("brace")])),
                Rule("\\}", action: .token(.punctuation), newState: .ops([.pop])),

                // Variables
                Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable"))),
                Rule("\\$" + ident, action: .token(.name.child("Variable"))),

                // Numbers
                Rule("0x[0-9A-Fa-f]+", action: .token(.number)),
                Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("\\b(?:proc|if|then|else|elseif|for|foreach|while|switch|set|unset|incr|return|break|continue|namespace|variable)\\b", action: .token(.keyword)),

                Rule(ident, action: .token(.name)),
                Rule("[()\\[\\].,;]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[+\\-*/%<>=]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
