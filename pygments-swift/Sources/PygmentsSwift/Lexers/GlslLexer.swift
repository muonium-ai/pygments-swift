import Foundation

/// Pragmatic GLSL lexer (smoke-test level).
///
/// Highlights comments, preprocessor, strings, keywords/types, numbers,
/// operators, and identifiers.
public final class GlslLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "if", "else", "for", "while", "do", "break", "continue", "return", "discard",
            "switch", "case", "default", "struct", "const", "in", "out", "inout",
            "uniform", "layout", "precision", "highp", "mediump", "lowp",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "void", "bool", "int", "uint", "float", "double",
            "vec2", "vec3", "vec4", "bvec2", "bvec3", "bvec4",
            "ivec2", "ivec3", "ivec4", "uvec2", "uvec3", "uvec4",
            "mat2", "mat3", "mat4", "mat2x2", "mat2x3", "mat2x4",
            "mat3x2", "mat3x3", "mat3x4", "mat4x2", "mat4x3", "mat4x4",
            "sampler2D", "samplerCube", "sampler2DShadow", "samplerCubeShadow",
        ], suffix: "\\b")

        let constants = RegexHelpers.words(["true", "false"], suffix: "\\b")

        let ident = #"[_\p{XID_Start}][_\p{XID_Continue}]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Preprocessor
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Strings
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),

                // Keywords / types / constants
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),
                .rule(Rule(constants, action: .token(.keyword.child("Constant")))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("\\d+(?:\\.\\d+)?(?:[eE][+\\-]?\\d+)?", action: .token(.number))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]{}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|\\+\\+|--|&&|\\|\\||\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=)", action: .token(.operator))),
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
