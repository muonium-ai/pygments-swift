import Foundation

/// Minimal Terraform/HCL lexer.
public final class TerraformLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Heredoc
                Rule("<<-?([A-Za-z_][A-Za-z0-9_]*)\\n[\\s\\S]*?\\n\\1", options: [.dotMatchesLineSeparators], action: .token(.string)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:true|false|null)\\b", options: [.caseInsensitive], action: .token(.keyword)),
                Rule("\\b(?:resource|variable|output|provider|module|data|locals|terraform)\\b", action: .token(.keyword)),

                Rule("\\$\\{[^\\}]+\\}", action: .token(.name.child("Variable"))),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number)),

                Rule("[A-Za-z_][A-Za-z0-9_\\-]*", action: .token(.name)),
                Rule("[{}\\[\\](),.=]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|&&|\\|\\||[+\\-*/<>!?::]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
