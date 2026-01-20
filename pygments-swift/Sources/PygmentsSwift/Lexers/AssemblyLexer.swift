import Foundation

/// Minimal assembly lexer (generic).
public final class AssemblyLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule(";[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("^[A-Za-z_\\.][A-Za-z0-9_\\.]*:", options: [.anchorsMatchLines], action: .token(.name.child("Label"))),

                Rule("\\.[A-Za-z_][A-Za-z0-9_]*", action: .token(.keyword)),

                Rule("%[A-Za-z0-9_]+", action: .token(.name.child("Variable"))),
                Rule("\\$[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Variable"))),

                Rule("0x[0-9A-Fa-f]+", action: .token(.number)),
                Rule("\\b\\d+\\b", action: .token(.number)),

                Rule("\\b(?:mov|lea|add|sub|mul|imul|div|idiv|and|or|xor|cmp|test|jmp|je|jne|jg|jge|jl|jle|call|ret|push|pop|nop)\\b", options: [.caseInsensitive], action: .token(.keyword)),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("[A-Za-z_\\.][A-Za-z0-9_\\.]*", action: .token(.name)),

                Rule("[()\\[\\]{},;:]", action: .token(.punctuation)),
                Rule("==|!=|<=|>=|<<|>>|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
