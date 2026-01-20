import Foundation

/// Pragmatic Ada lexer (smoke-test level).
///
/// Highlights common Ada tokens: comments, strings/chars, keywords,
/// numbers, punctuation/operators, and identifiers.
public final class AdaLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
            "array", "at", "begin", "body", "case", "constant", "declare", "delay",
            "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
            "exit", "for", "function", "generic", "goto", "if", "in", "interface",
            "is", "limited", "loop", "mod", "new", "not", "null", "of", "or",
            "others", "out", "overriding", "package", "pragma", "private", "procedure",
            "protected", "raise", "range", "record", "rem", "renames", "requeue",
            "return", "reverse", "select", "separate", "subtype", "synchronized",
            "tagged", "task", "terminate", "then", "type", "until", "use", "when",
            "while", "with", "xor",
        ], suffix: "\\b")

        let builtinTypes = RegexHelpers.words([
            "boolean", "character", "duration", "float", "integer", "long_float",
            "long_integer", "natural", "positive", "string",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false"], suffix: "\\b")

        let ident = #"[A-Za-z][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings / chars (simplified)
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'[^'\\n]+'", action: .token(.string.child("Char")))),

                // Keywords / types / constants
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),
                .rule(Rule(builtinTypes, options: [.caseInsensitive], action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, options: [.caseInsensitive], action: .token(.keyword.child("Constant")))),

                // Numbers (simplified)
                .rule(Rule("\\d+#[0-9A-Fa-f_]+#", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+(?:_\\d+)*)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(:=|=>|\\.\\.|\\*\\*)", action: .token(.operator))),
                .rule(Rule("[+\\-*/=<>|&^~]", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "dq": [
                .rule(Rule("\"\"", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^\"]+", action: .token(.string))),
            ],
        ]
    }
}
