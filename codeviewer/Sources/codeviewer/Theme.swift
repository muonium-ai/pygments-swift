import AppKit
import PygmentsSwift

extension NSColor {
    convenience init(_ color: RGBAColor) {
        self.init(calibratedRed: color.r, green: color.g, blue: color.b, alpha: color.a)
    }
}

struct CodeTheme {
    let theme: HighlightTheme

    static var allNames: [String] {
        HighlightTheme.allNames
    }

    static func named(_ raw: String) throws -> CodeTheme {
        do {
            return CodeTheme(theme: try HighlightTheme.named(raw))
        } catch {
            throw CLIError("Unknown theme: \(raw). Try one of: \(allNames.joined(separator: ", "))")
        }
    }

    var name: String { theme.name }
    var background: NSColor { NSColor(theme.background) }
    var defaultForeground: NSColor { NSColor(theme.defaultForeground) }

    func foreground(for type: TokenType) -> NSColor {
        NSColor(theme.foreground(for: type))
    }
}
