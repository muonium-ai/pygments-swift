import Foundation

/// Pragmatic C++ lexer (smoke-test level).
///
/// Extends the C lexer with common C++ keywords, namespaces, templates,
/// and boolean literals.
public final class CppLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            // C/C++ shared
            "auto", "break", "case", "catch", "class", "const", "constexpr", "consteval",
            "constinit", "continue", "default", "delete", "do", "else", "enum", "explicit",
            "export", "extern", "for", "friend", "goto", "if", "inline", "mutable",
            "namespace", "new", "noexcept", "operator", "private", "protected", "public",
            "register", "reinterpret_cast", "return", "sizeof", "static", "static_cast",
            "struct", "switch", "template", "this", "throw", "try", "typedef", "typeid",
            "typename", "union", "using", "virtual", "volatile", "while",
            // C++20 modules / coroutines / concepts-ish
            "co_await", "co_return", "co_yield", "requires", "concept", "module", "import",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "void", "bool", "char", "char8_t", "char16_t", "char32_t", "wchar_t",
            "short", "int", "long", "float", "double",
            "signed", "unsigned",
            "size_t", "ptrdiff_t",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false", "nullptr", "NULL"], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Preprocessor (line-based)
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings / chars (naive; no raw-string parser yet)
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.push("sq")]))),

                // Declarations
                .rule(Rule("(class|struct)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword.child("Declaration"), .whitespace, .name.child("Class")
                ]))),
                .rule(Rule("(namespace)(\\s+)(" + ident + ")", action: .byGroups([
                    .keyword.child("Namespace"), .whitespace, .name.child("Namespace")
                ]))),

                // Keywords / types / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers (simplified)
                .rule(Rule("0[xX][0-9a-fA-F']+", action: .token(.number.child("Hex")))),
                .rule(Rule("0[bB][01']+", action: .token(.number.child("Bin")))),
                .rule(Rule("\\d+(?:'\\d+)*(?:\\.\\d+(?:'\\d+)*)?(?:[eE][+\\-]?\\d+)?[uUlLfF]*", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|->\\*?|::|&&|\\|\\||<=>|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%&|^~<>!?]=?", action: .token(.operator))),
                .rule(Rule("=", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string.child("Char")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\']+"#, action: .token(.string.child("Char")))),
                .rule(Rule("\\\\", action: .token(.string.child("Char")))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
        ]
    }
}
