import Foundation

/// Pragmatic Mojo lexer (smoke-test level).
///
/// Mojo is Python-like with additional keywords for systems programming.
public final class MojoLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "fn", "struct", "trait", "impl", "let", "var", "alias", "type",
            "import", "from", "as", "return", "yield",
            "if", "elif", "else", "for", "while", "in", "break", "continue",
            "try", "except", "finally", "raise", "with",
            "true", "false", "none"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings (basic)
                .rule(Rule("\"\"\".*?\"\"\"", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("'''\\s*.*?'''", options: [.dotMatchesLineSeparators], action: .token(.string))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f_]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b0[bB][01_]+\\b", action: .token(.number.child("Bin")))),
                .rule(Rule("\\b\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?\\b", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("==|!=|<=|>=|:=|->|[-+*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
