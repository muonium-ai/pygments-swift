import Foundation

/// Pragmatic Slim template lexer (smoke-test level).
public final class SlimLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Line comments
                .rule(Rule("^[^\\S\\n]*/[^\\n]*", options: [.anchorsMatchLines], action: .token(.comment.child("Single")))),

                // Doctype
                .rule(Rule("^[^\\S\\n]*(doctype)\\b[^\\n]*", options: [.anchorsMatchLines], action: .byGroups([
                    .keyword, nil,
                ]))),

                // Ruby-ish code lines (- or = at start)
                .rule(Rule("^([\\t ]*)([-=])(.*)$", options: [.anchorsMatchLines], action: .byGroups([
                    .whitespace, .punctuation, .other,
                ]))),

                // Tag at line start: `tag#id.class`
                .rule(Rule("^([\\t ]*)([A-Za-z][A-Za-z0-9:_-]*)([.#][^\\s\\n]+)*", options: [.anchorsMatchLines], action: .byGroups([
                    .whitespace, .name.child("Tag"), .name.child("Attribute"),
                ]))),

                // Attribute-ish tokens
                .rule(Rule("[A-Za-z_:][A-Za-z0-9:._-]*", action: .token(.name.child("Attribute")))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
