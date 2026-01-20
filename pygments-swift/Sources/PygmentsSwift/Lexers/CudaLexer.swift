import Foundation

/// Pragmatic CUDA lexer (C/C++-like with CUDA keywords; smoke-test level).
public final class CudaLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let cudaKeywords = [
            "__global__", "__device__", "__host__", "__shared__", "__constant__", "__managed__",
            "__syncthreads", "threadIdx", "blockIdx", "blockDim", "gridDim", "warpSize",
        ]
        let keywordPattern = "(?:" + cudaKeywords.map { NSRegularExpression.escapedPattern(for: $0) }.joined(separator: "|") + ")"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Preprocessor
                .rule(Rule("#\\s*\\w+", action: .token(.comment.child("Preproc")))),

                // Strings / chars
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string.child("Char")))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?\\b", action: .token(.number))),

                // CUDA keywords
                .rule(Rule(keywordPattern, action: .token(.keyword))),

                // Operators / punctuation
                .rule(Rule("<<<|>>>|<<=?|>>=?|[-+*/%]=?|==?=?|!=?=?|<=|>=|&&|\\|\\||[?:~^&|]", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;]", action: .token(.punctuation))),

                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
