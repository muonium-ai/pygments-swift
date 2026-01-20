import Foundation

/// Pragmatic SystemVerilog lexer (smoke-test level).
///
/// Highlights comments, strings, preprocessor directives, keywords/types,
/// numbers, operators, and identifiers.
public final class SystemVerilogLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "always", "always_comb", "always_ff", "always_latch", "assign", "automatic",
            "begin", "bind", "case", "casex", "casez", "class", "clocking", "config",
            "const", "constraint", "cover", "covergroup", "coverpoint", "default",
            "disable", "do", "else", "end", "endcase", "endclass", "endfunction",
            "endgenerate", "endgroup", "endinterface", "endmodule", "endpackage",
            "endprogram", "endproperty", "endsequence", "endtask", "enum", "export",
            "extends", "extern", "final", "for", "foreach", "forever", "fork", "function",
            "generate", "genvar", "if", "import", "initial", "interface", "join",
            "local", "localparam", "logic", "modport", "module", "new", "package",
            "parameter", "posedge", "negedge", "program", "property", "protected",
            "public", "rand", "randc", "return", "sequence", "static", "struct",
            "task", "this", "typedef", "union", "unique", "virtual", "wait", "while",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "bit", "byte", "shortint", "int", "longint", "integer", "time",
            "real", "shortreal", "realtime", "string", "chandle", "event",
            "wire", "reg", "tri", "tri0", "tri1", "wand", "wor",
        ], suffix: "\\b")

        let constants = RegexHelpers.words([
            "null", "true", "false",
        ], suffix: "\\b")

        let ident = #"[_A-Za-z][A-Za-z0-9_$]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Preprocessor directives
                .rule(Rule(#"`[^\n]*"#, action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Keywords / types / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers (simplified)
                .rule(Rule(#"\d+'[bBoOdDhH][0-9A-Fa-f_xzXZ?]+"#, action: .token(.number))),
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|&&|\\|\\||<<|>>|\\+\\+|--|::|->|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=)", action: .token(.operator))),
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

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],
        ]
    }
}
