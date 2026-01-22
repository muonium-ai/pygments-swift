import Foundation

/// Pragmatic ASPX lexer (smoke-test level).
///
/// This is a simple HTML-like lexer with ASP.NET code blocks highlighted.
public final class AspxLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // ASP.NET server-side comments
                .rule(Rule("<%--.*?--%>", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),
                // ASP.NET code blocks
                .rule(Rule("<%=?\\s*.*?%>", options: [.dotMatchesLineSeparators], action: .token(.other))),

                // HTML comments
                .rule(Rule("<!--.*?-->", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline")))),

                // Tags
                .rule(Rule("</?[A-Za-z][A-Za-z0-9:_-]*", action: .token(.name.child("Tag")))),
                .rule(Rule("/?>", action: .token(.name.child("Tag")))),

                // Attributes
                .rule(Rule("[A-Za-z_:][A-Za-z0-9:._-]*", action: .token(.name.child("Attribute")))),
                .rule(Rule("=", action: .token(.operator))),
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
