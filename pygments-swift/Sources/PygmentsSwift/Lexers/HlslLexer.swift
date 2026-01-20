import Foundation

/// Pragmatic HLSL lexer (smoke-test level).
public final class HlslLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "cbuffer", "register",
            "struct", "return",
            "if", "else", "for", "while", "do", "break", "continue",
            "static", "const", "volatile",
            "true", "false",
            // common types
            "bool", "int", "uint",
            "float", "float2", "float3", "float4",
            "float2x2", "float3x3", "float4x4",
            "half", "double",
            "Texture2D", "Texture3D", "TextureCube",
            "SamplerState", "SamplerComparisonState",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Preprocessor
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Strings
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),

                // Semantics and registers after ':'
                .rule(Rule(":\\s*[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Attribute")))),

                // Numbers
                .rule(Rule("0[xX][0-9A-Fa-f]+", action: .token(.number.child("Hex")))),
                .rule(Rule("[-+]?(?:\\d*\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?[fF]?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[{}\\[\\]();,.:]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=|<<|>>|&&|\\|\\|)", action: .token(.operator))),
                .rule(Rule("[=<>+\\-*/%!&|^~?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
