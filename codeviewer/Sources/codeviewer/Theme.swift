import AppKit
import PygmentsSwift

enum CodeTheme {
    case dark
    case light

    init(name: String) throws {
        switch name.lowercased() {
        case "dark": self = .dark
        case "light": self = .light
        default: throw CLIError("Unknown theme: \(name) (expected 'dark' or 'light')")
        }
    }

    var background: NSColor {
        switch self {
        case .dark: return NSColor(calibratedRed: 0.10, green: 0.11, blue: 0.14, alpha: 1)
        case .light: return NSColor(calibratedWhite: 0.98, alpha: 1)
        }
    }

    var defaultForeground: NSColor {
        switch self {
        case .dark: return NSColor(calibratedWhite: 0.92, alpha: 1)
        case .light: return NSColor(calibratedWhite: 0.10, alpha: 1)
        }
    }

    func foreground(for type: TokenType) -> NSColor {
        if type.isSubtype(of: .comment) {
            return self == .dark
                ? NSColor(calibratedWhite: 0.65, alpha: 1)
                : NSColor(calibratedWhite: 0.45, alpha: 1)
        }
        if type.isSubtype(of: .keyword) {
            return self == .dark
                ? NSColor(calibratedRed: 0.53, green: 0.70, blue: 1.00, alpha: 1)
                : NSColor(calibratedRed: 0.12, green: 0.30, blue: 0.85, alpha: 1)
        }
        if type.isSubtype(of: .string) {
            return self == .dark
                ? NSColor(calibratedRed: 0.98, green: 0.62, blue: 0.62, alpha: 1)
                : NSColor(calibratedRed: 0.70, green: 0.15, blue: 0.18, alpha: 1)
        }
        if type.isSubtype(of: .number) {
            return self == .dark
                ? NSColor(calibratedRed: 0.98, green: 0.78, blue: 0.55, alpha: 1)
                : NSColor(calibratedRed: 0.62, green: 0.40, blue: 0.05, alpha: 1)
        }
        if type.isSubtype(of: .name) {
            return defaultForeground
        }
        if type.isSubtype(of: .operator) || type.isSubtype(of: .punctuation) {
            return self == .dark
                ? NSColor(calibratedWhite: 0.85, alpha: 1)
                : NSColor(calibratedWhite: 0.20, alpha: 1)
        }
        if type.isSubtype(of: .error) {
            return NSColor.systemRed
        }
        return defaultForeground
    }
}
