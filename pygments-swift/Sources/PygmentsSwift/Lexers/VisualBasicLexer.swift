import Foundation

/// Pragmatic Visual Basic (.vb) lexer (smoke-test level).
///
/// Highlights comments, strings, keywords, numbers, punctuation/operators,
/// and identifiers.
public final class VisualBasicLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "AddHandler", "And", "As", "Boolean", "ByRef", "ByVal", "Byte", "Call",
            "Case", "Catch", "Class", "Const", "Continue", "Date", "Decimal", "Default",
            "Delegate", "Dim", "Do", "Double", "Each", "Else", "ElseIf", "End",
            "Enum", "Exit", "False", "Finally", "For", "Friend", "Function", "Get",
            "Handles", "If", "Imports", "In", "Inherits", "Integer", "Interface",
            "Is", "Let", "Long", "Me", "Mod", "Module", "Namespace", "New", "Next",
            "Not", "Nothing", "Object", "Of", "On", "Optional", "Or", "Overloads",
            "Overrides", "Private", "Protected", "Public", "ReadOnly", "Return", "Select",
            "Set", "Shared", "Short", "Single", "Static", "String", "Structure", "Sub",
            "Then", "Throw", "To", "True", "Try", "Until", "While", "With", "Xor",
        ], suffix: "\\b")

        let ident = #"[_A-Za-z][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("'[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule(#"(?i)\bREM\b[^\n]*"#, action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Keywords
                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                // Numbers
                .rule(Rule("&[hH][0-9A-Fa-f_]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:_\\d+)*(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(<=|>=|<>|=|\\+|\\-|\\*|/|\\\\)", action: .token(.operator))),

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
