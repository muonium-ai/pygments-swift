import Foundation

/// Pragmatic LLVM IR lexer (smoke-test level).
///
/// Highlights comments, keywords/instructions, types, numbers, strings,
/// globals/locals, punctuation/operators.
public final class LlvmLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "define", "declare", "global", "constant", "private", "internal", "external",
            "linkonce", "weak", "common", "unnamed_addr", "addrspace", "align", "section",
            "attributes", "target", "datalayout", "triple", "type", "metadata",
        ], suffix: "\\b")

        let instructions = RegexHelpers.words([
            "add", "sub", "mul", "udiv", "sdiv", "urem", "srem",
            "fadd", "fsub", "fmul", "fdiv", "frem",
            "and", "or", "xor", "shl", "lshr", "ashr",
            "alloca", "load", "store", "getelementptr", "bitcast",
            "ptrtoint", "inttoptr", "trunc", "zext", "sext", "fptrunc", "fpext",
            "icmp", "fcmp", "phi", "select",
            "call", "ret", "br", "switch", "invoke", "resume", "unreachable",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "void", "half", "float", "double", "fp128", "x86_fp80", "ppc_fp128",
            "label", "metadata", "token",
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule(";[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings (constants like c"..." or "...")
                .rule(Rule("c\"", action: .token(.string), newState: .ops([.push("cstring")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Keywords / instructions / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(instructions, action: .token(.keyword))),
                .rule(Rule("i\\d+\\b", action: .token(.keyword.child("Type")))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),

                // Globals / locals
                .rule(Rule("%[-$._A-Za-z0-9]+", action: .token(.name.child("Variable")))),
                .rule(Rule("@[-$._A-Za-z0-9]+", action: .token(.name.child("Namespace")))),

                // Labels
                .rule(Rule("[-$._A-Za-z0-9]+:", action: .token(.name.child("Label")))),

                // Numbers
                .rule(Rule("-?\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,]", action: .token(.punctuation))),
                .rule(Rule("(=|\\*|\\+|\\-|/|<|>)", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "cstring": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\[0-9A-Fa-f]{2}"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\"]+"#, action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
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
