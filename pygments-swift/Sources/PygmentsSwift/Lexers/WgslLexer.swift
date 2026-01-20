import Foundation

/// Pragmatic WGSL lexer (smoke-test level).
///
/// Covers common tokens in WebGPU Shading Language: comments, strings, numbers,
/// keywords/types, attributes, operators and identifiers.
public final class WgslLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "fn", "let", "var", "const", "override",
            "if", "else", "switch", "case", "default",
            "loop", "for", "while", "break", "continue", "return", "discard",
            "struct",
            "true", "false",
        ], suffix: "\\b")

        let types = RegexHelpers.words([
            "bool", "i32", "u32", "f32", "f16",
            "vec2", "vec3", "vec4",
            "mat2x2", "mat2x3", "mat2x4",
            "mat3x2", "mat3x3", "mat3x4",
            "mat4x2", "mat4x3", "mat4x4",
            "array", "ptr", "atomic",
            "sampler", "sampler_comparison",
            "texture_1d", "texture_2d", "texture_2d_array", "texture_3d", "texture_cube", "texture_cube_array",
            "texture_multisampled_2d", "texture_storage_1d", "texture_storage_2d", "texture_storage_2d_array", "texture_storage_3d",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Attributes
                .rule(Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d+)(?:\\.\\d+)?(?:[eE][-+]?\\d+)?", action: .token(.number))),

                // Keywords / types
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(types, action: .token(.keyword.child("Type")))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]\\{\\}:.,;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|->)", action: .token(.operator))),
                .rule(Rule("[+\\-*/%<>=!&|^~?]+", action: .token(.operator))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
