import Foundation

/// Minimal CMake lexer.
public final class CMakeLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Bracket strings: [[ ... ]]
                Rule("\\[\\[[\\s\\S]*?\\]\\]", options: [.dotMatchesLineSeparators], action: .token(.string)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                // Variables: ${VAR}
                Rule("\\$\\{[^\\}]+\\}", action: .token(.name.child("Variable"))),

                Rule("\\b(?:true|false|on|off|yes|no)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                // Commands at line start
                Rule("^([A-Za-z_][A-Za-z0-9_]*)\\s*(?=\\()", options: [.anchorsMatchLines], action: .token(.name.child("Function"))),

                Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[()\\[\\]{},]", action: .token(.punctuation)),
                Rule("[=+\\-*/<>]", action: .token(.operator)),

                Rule("[A-Za-z_][A-Za-z0-9_\\-]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
