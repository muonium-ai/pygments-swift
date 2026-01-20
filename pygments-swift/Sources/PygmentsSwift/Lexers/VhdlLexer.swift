import Foundation

/// Pragmatic VHDL lexer (smoke-test level).
///
/// Highlights comments, strings, keywords, numbers, punctuation/operators,
/// and identifiers.
public final class VhdlLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "architecture", "array", "assert", "attribute", "begin", "block", "body",
            "buffer", "bus", "case", "component", "configuration", "constant", "downto",
            "else", "elsif", "end", "entity", "exit", "file", "for", "function",
            "generate", "generic", "group", "guarded", "if", "impure", "in", "inertial",
            "inout", "is", "label", "library", "linkage", "literal", "loop", "map",
            "new", "next", "null", "of", "on", "open", "others", "out", "package",
            "port", "postponed", "procedure", "process", "pure", "range", "record",
            "register", "reject", "report", "return", "rol", "ror", "select", "severity",
            "signal", "shared", "subtype", "then", "to", "transport", "type", "units",
            "until", "use", "variable", "wait", "when", "while", "with",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "bit", "bit_vector", "boolean", "character", "integer", "natural", "positive",
            "real", "string", "std_logic", "std_logic_vector", "unsigned", "signed",
        ], suffix: "\\b")

        let ident = #"[A-Za-z][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),

                // Bit-string literals like X"FF" / B"1010"
                .rule(Rule("[xXbBoO]\\\"[0-9A-Fa-f_]+\\\"", action: .token(.string))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'[^'\\n]+'", action: .token(.string.child("Char")))),

                // Keywords / types
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),
                .rule(Rule(types, options: [.caseInsensitive], action: .token(.keyword.child("Type")))),

                // Numbers
                .rule(Rule("\\d+(?:#[_0-9A-Fa-f]+#)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(<=|>=|:=|=>|/=|\\*\\*|\\.\\.)", action: .token(.operator))),
                .rule(Rule("[+\\-*/=<>|&^~]", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "dq": [
                .rule(Rule("\"\"", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"[^\"]+"#, action: .token(.string))),
            ],
        ]
    }
}
