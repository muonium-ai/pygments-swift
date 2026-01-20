import Foundation

/// Pragmatic Eiffel lexer (smoke-test level).
///
/// Highlights comments, strings, keywords, numbers, punctuation/operators,
/// and identifiers.
public final class EiffelLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "across", "agent", "alias", "all", "and", "as", "assign", "attached",
            "attribute", "check", "class", "convert", "create", "current", "debug",
            "deferred", "do", "else", "elseif", "end", "ensure", "expanded", "export",
            "external", "feature", "from", "frozen", "if", "implies", "inherit",
            "inspect", "invariant", "like", "local", "loop", "not", "obsolete", "old",
            "once", "or", "precursor", "redefine", "rename", "require", "rescue",
            "result", "retry", "select", "separate", "then", "true", "false",
            "undefine", "unique", "until", "variant", "void", "when", "xor",
        ], suffix: "\\b")

        let ident = #"[A-Za-z][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("--[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings / chars
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'[^'\\n]+'", action: .token(.string.child("Char")))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Numbers
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(\\.\\.|:=|<=|>=|/=|\\\\/=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/=<>]", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "dq": [
                .rule(Rule("%\"", action: .token(.string.child("Escape")))),
                .rule(Rule("\"\"", action: .token(.string))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^\"]+", action: .token(.string))),
            ],
        ]
    }
}
