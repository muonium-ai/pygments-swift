import Foundation

/// Minimal PowerShell lexer.
public final class PowerShellLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("<#.*?#>", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.|\\$[A-Za-z_][A-Za-z0-9_]*)*\"", action: .token(.string)),
                Rule("'(?:[^'])*'", action: .token(.string)),

                // Variables
                Rule("\\$\\{[^\\}]+\\}", action: .token(.name.child("Variable"))),
                Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("\\b(?:-?\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:function|param|begin|process|end|if|elseif|else|foreach|for|while|do|until|switch|break|continue|return|try|catch|finally|throw|trap|in|class|enum|filter)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                // Cmdlets like Get-Process
                Rule("[A-Za-z_][A-Za-z0-9_]*-[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Function"))),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("-(?:eq|ne|gt|ge|lt|le|like|notlike|match|notmatch|contains|notcontains|in|notin)\\b", options: [.caseInsensitive], action: .token(.operator)),
                Rule("==|!=|<=|>=|\\+\\+|--|&&|\\|\\||[=+\\-*/%<>!?:|]", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
