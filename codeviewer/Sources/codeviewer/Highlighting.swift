import AppKit
import Foundation
import PygmentsSwift

struct CodeHighlighter {
    static func highlight(text: String, lexer: Lexer, theme: CodeTheme, font: NSFont) -> NSAttributedString {
        let tokens = lexer.getTokens(text)
        let out = NSMutableAttributedString()

        let paragraph = NSMutableParagraphStyle()
        // Code often contains long tokens without spaces; wrap by character to avoid clipping.
        paragraph.lineBreakMode = .byCharWrapping

        let base: [NSAttributedString.Key: Any] = [
            .font: font,
            .foregroundColor: theme.defaultForeground,
            .paragraphStyle: paragraph
        ]

        for t in tokens {
            var attrs = base
            attrs[.foregroundColor] = theme.foreground(for: t.type)

            // Keep whitespace legible and avoid accidental bolding/italics.
            if t.type.isSubtype(of: .whitespace) || t.type.isSubtype(of: .text) {
                attrs[.foregroundColor] = theme.defaultForeground
            }

            out.append(NSAttributedString(string: t.value, attributes: attrs))
        }

        return out
    }
}
