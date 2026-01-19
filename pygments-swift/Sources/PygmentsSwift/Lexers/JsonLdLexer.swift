import Foundation

/// JSON-LD lexer based on Pygments' `JsonLdLexer`.
///
/// This behaves like `JsonLexer`, but retokenizes JSON-LD keywords like "@context"
/// (including surrounding quotes) from `Name.Tag` to `Name.Decorator`.
public final class JsonLdLexer: LexerBase {
    private static let jsonLdKeywords: Set<String> = [
        "\"@base\"",
        "\"@container\"",
        "\"@context\"",
        "\"@direction\"",
        "\"@graph\"",
        "\"@id\"",
        "\"@import\"",
        "\"@included\"",
        "\"@index\"",
        "\"@json\"",
        "\"@language\"",
        "\"@list\"",
        "\"@nest\"",
        "\"@none\"",
        "\"@prefix\"",
        "\"@propagate\"",
        "\"@protected\"",
        "\"@reverse\"",
        "\"@set\"",
        "\"@type\"",
        "\"@value\"",
        "\"@version\"",
        "\"@vocab\"",
    ]

    public override func getTokens(_ text: String) -> [Token] {
        // Delegate base tokenization to JsonLexer, then retokenize keywords.
        let base = JsonLexer(options: options)
        let tokens = base.getTokens(text)
        if tokens.isEmpty { return tokens }

        return tokens.map { t in
            if t.type == .name.child("Tag"), Self.jsonLdKeywords.contains(t.value) {
                return Token(start: t.start, startScalar: t.startScalar, type: .name.child("Decorator"), value: t.value)
            }
            return t
        }
    }
}
