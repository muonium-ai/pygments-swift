import Foundation

/// Pragmatic lexer for `.env` / dotenv files.
public final class DotenvLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Comments
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // export KEY=...
                Rule("\\bexport\\b", action: .token(.keyword)),

                // KEY =
                Rule(
                    "([A-Za-z_][A-Za-z0-9_]*)(\\s*)(=)(\\s*)",
                    action: .byGroups([.name.child("Attribute"), .whitespace, .operator, .whitespace])
                ),

                // Quoted values
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                // Unquoted value (until newline)
                Rule("[^\\n#]+", action: .token(.string)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
