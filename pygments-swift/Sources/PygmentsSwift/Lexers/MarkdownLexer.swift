import Foundation

/// Minimal Markdown lexer.
///
/// Focuses on common structures (headings, lists, code fences, inline code, links).
public final class MarkdownLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                // Fenced code blocks
                Rule("^(?:```|~~~)[^\\n]*\\n", options: [.anchorsMatchLines], action: .token(.string), newState: .ops([.push("fenced")]) ),

                // ATX headings: # H1
                Rule("^(#{1,6})(\\s+)(.*)$", options: [.anchorsMatchLines], action: .byGroups([.keyword, .whitespace, .string])),

                // Blockquote
                Rule("^(>)(\\s+)", options: [.anchorsMatchLines], action: .byGroups([.punctuation, .whitespace])),

                // List markers
                Rule("^(\\s*)([*+-]|\\d+\\.)(\\s+)", options: [.anchorsMatchLines], action: .byGroups([.whitespace, .punctuation, .whitespace])),

                // Inline code
                Rule("`[^`\\n]+`", action: .token(.string)),

                // Links: [text](url)
                Rule("\\[[^\\]\\n]+\\]\\([^\\)\\n]+\\)", action: .token(.name)),

                // Emphasis markers
                Rule("\\*\\*|__|\\*|_", action: .token(.punctuation)),

                // Plain text until a special delimiter
                Rule("[^\\n`\\*\\[_]+", action: .token(.text)),

                // Fallback single characters
                Rule(".", action: .token(.text)),
            ],

            "fenced": [
                Rule("^(?:```|~~~)[^\\n]*\\n", options: [.anchorsMatchLines], action: .token(.string), newState: .ops([.pop]) ),
                Rule("[^\\n]+", action: .token(.string)),
                Rule("\\n", action: .token(.string)),
            ],
        ]
    }
}
