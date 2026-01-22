import Foundation

/// Pragmatic POV-Ray lexer (smoke-test level).
public final class PovrayLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "camera", "light_source", "sphere", "box", "cylinder", "cone", "plane",
            "pigment", "finish", "texture", "normal", "background", "include", "declare",
            "union", "intersection", "difference", "translate", "rotate", "scale"
        ], suffix: "\\b")

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),
                .rule(Rule("#[A-Za-z_]+", action: .token(.comment.child("Preproc")))),

                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule("[-+]?(?:0|[1-9]\\d*)(?:\\.\\d+)?", action: .token(.number))),

                .rule(Rule(keywords, options: [.caseInsensitive], action: .token(.keyword))),

                .rule(Rule("[()\\[\\]{},.:;]", action: .token(.punctuation))),
                .rule(Rule("==|!=|<=|>=|[-+*/%&|^~<>!?]=?", action: .token(.operator))),
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
