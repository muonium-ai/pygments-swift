import Foundation

/// Minimal CSV lexer.
public final class CsvLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("\\r?\\n", action: .token(.whitespace)),
                Rule(",", action: .token(.punctuation)),

                Rule("\"(?:[^\"]|\"\")*\"", action: .token(.string)),

                Rule("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number)),

                Rule("[^,\\r\\n]+", action: .token(.text)),
            ]
        ]
    }
}
