import AppKit
import PygmentsSwift

struct CodeTheme {
    struct Palette {
        let background: NSColor
        let foreground: NSColor
        let comment: NSColor
        let keyword: NSColor
        let keywordType: NSColor
        let keywordDeclaration: NSColor
        let keywordConstant: NSColor
        let string: NSColor
        let stringEscape: NSColor
        let number: NSColor
        let name: NSColor
        let decorator: NSColor
        let punctuation: NSColor
        let `operator`: NSColor
        let error: NSColor
    }

    let name: String
    let palette: Palette

    static var allNames: [String] {
        return all.map { $0.name }.sorted()
    }

    static func named(_ raw: String) throws -> CodeTheme {
        let key = raw.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        if let t = all.first(where: { $0.name == key }) {
            return t
        }
        throw CLIError("Unknown theme: \(raw). Try one of: \(allNames.joined(separator: ", "))")
    }

    var background: NSColor { palette.background }
    var defaultForeground: NSColor { palette.foreground }

    func foreground(for type: TokenType) -> NSColor {
        if type.isSubtype(of: .error) { return palette.error }

        if type.isSubtype(of: .comment) { return palette.comment }

        if type.isSubtype(of: .name.child("Decorator")) { return palette.decorator }

        if type.isSubtype(of: .keyword.child("Declaration")) { return palette.keywordDeclaration }
        if type.isSubtype(of: .keyword.child("Type")) { return palette.keywordType }
        if type.isSubtype(of: .keyword.child("Constant")) { return palette.keywordConstant }
        if type.isSubtype(of: .keyword) { return palette.keyword }

        if type.isSubtype(of: .string.child("Escape")) { return palette.stringEscape }
        if type.isSubtype(of: .string) { return palette.string }

        if type.isSubtype(of: .number) { return palette.number }

        if type.isSubtype(of: .operator) { return palette.operator }
        if type.isSubtype(of: .punctuation) { return palette.punctuation }

        if type.isSubtype(of: .name) { return palette.name }

        return palette.foreground
    }

    private static let all: [CodeTheme] = [
        // A couple of pleasing defaults (not exact clones of any specific theme).
        CodeTheme(
            name: "github-dark",
            palette: Palette(
                background: NSColor(calibratedRed: 0.05, green: 0.07, blue: 0.10, alpha: 1),
                foreground: NSColor(calibratedWhite: 0.92, alpha: 1),
                comment: NSColor(calibratedWhite: 0.62, alpha: 1),
                keyword: NSColor(calibratedRed: 0.54, green: 0.70, blue: 1.00, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.52, green: 0.85, blue: 0.95, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.78, green: 0.62, blue: 1.00, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.98, green: 0.80, blue: 0.55, alpha: 1),
                string: NSColor(calibratedRed: 0.98, green: 0.62, blue: 0.62, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.98, green: 0.80, blue: 0.60, alpha: 1),
                number: NSColor(calibratedRed: 0.98, green: 0.78, blue: 0.55, alpha: 1),
                name: NSColor(calibratedWhite: 0.92, alpha: 1),
                decorator: NSColor(calibratedRed: 0.62, green: 0.90, blue: 0.75, alpha: 1),
                punctuation: NSColor(calibratedWhite: 0.80, alpha: 1),
                operator: NSColor(calibratedWhite: 0.84, alpha: 1),
                error: NSColor.systemRed
            )
        ),
        CodeTheme(
            name: "github-light",
            palette: Palette(
                background: NSColor(calibratedWhite: 0.99, alpha: 1),
                foreground: NSColor(calibratedWhite: 0.12, alpha: 1),
                comment: NSColor(calibratedWhite: 0.45, alpha: 1),
                keyword: NSColor(calibratedRed: 0.12, green: 0.30, blue: 0.85, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.10, green: 0.45, blue: 0.80, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.40, green: 0.20, blue: 0.70, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.62, green: 0.40, blue: 0.05, alpha: 1),
                string: NSColor(calibratedRed: 0.70, green: 0.15, blue: 0.18, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.70, green: 0.45, blue: 0.10, alpha: 1),
                number: NSColor(calibratedRed: 0.62, green: 0.40, blue: 0.05, alpha: 1),
                name: NSColor(calibratedWhite: 0.12, alpha: 1),
                decorator: NSColor(calibratedRed: 0.10, green: 0.55, blue: 0.35, alpha: 1),
                punctuation: NSColor(calibratedWhite: 0.20, alpha: 1),
                operator: NSColor(calibratedWhite: 0.18, alpha: 1),
                error: NSColor.systemRed
            )
        ),
        CodeTheme(
            name: "dracula",
            palette: Palette(
                background: NSColor(calibratedRed: 0.16, green: 0.16, blue: 0.21, alpha: 1),
                foreground: NSColor(calibratedRed: 0.96, green: 0.96, blue: 0.98, alpha: 1),
                comment: NSColor(calibratedRed: 0.45, green: 0.47, blue: 0.62, alpha: 1),
                keyword: NSColor(calibratedRed: 0.74, green: 0.52, blue: 0.98, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.34, green: 0.85, blue: 0.96, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.98, green: 0.56, blue: 0.78, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.95, green: 0.85, blue: 0.55, alpha: 1),
                string: NSColor(calibratedRed: 0.94, green: 0.80, blue: 0.55, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.55, green: 0.91, blue: 0.99, alpha: 1),
                number: NSColor(calibratedRed: 0.74, green: 0.92, blue: 0.57, alpha: 1),
                name: NSColor(calibratedRed: 0.96, green: 0.96, blue: 0.98, alpha: 1),
                decorator: NSColor(calibratedRed: 0.55, green: 0.91, blue: 0.99, alpha: 1),
                punctuation: NSColor(calibratedRed: 0.74, green: 0.75, blue: 0.87, alpha: 1),
                operator: NSColor(calibratedRed: 0.98, green: 0.56, blue: 0.78, alpha: 1),
                error: NSColor.systemRed
            )
        ),
        CodeTheme(
            name: "monokai",
            palette: Palette(
                background: NSColor(calibratedRed: 0.11, green: 0.11, blue: 0.11, alpha: 1),
                foreground: NSColor(calibratedWhite: 0.92, alpha: 1),
                comment: NSColor(calibratedRed: 0.46, green: 0.49, blue: 0.43, alpha: 1),
                keyword: NSColor(calibratedRed: 0.98, green: 0.33, blue: 0.52, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.41, green: 0.95, blue: 0.78, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.98, green: 0.33, blue: 0.52, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.68, green: 0.85, blue: 0.99, alpha: 1),
                string: NSColor(calibratedRed: 0.90, green: 0.97, blue: 0.55, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.99, green: 0.62, blue: 0.20, alpha: 1),
                number: NSColor(calibratedRed: 0.68, green: 0.85, blue: 0.99, alpha: 1),
                name: NSColor(calibratedWhite: 0.92, alpha: 1),
                decorator: NSColor(calibratedRed: 0.99, green: 0.62, blue: 0.20, alpha: 1),
                punctuation: NSColor(calibratedWhite: 0.85, alpha: 1),
                operator: NSColor(calibratedWhite: 0.88, alpha: 1),
                error: NSColor.systemRed
            )
        ),
        CodeTheme(
            name: "solarized-dark",
            palette: Palette(
                background: NSColor(calibratedRed: 0.00, green: 0.17, blue: 0.21, alpha: 1),
                foreground: NSColor(calibratedRed: 0.51, green: 0.58, blue: 0.59, alpha: 1),
                comment: NSColor(calibratedRed: 0.35, green: 0.43, blue: 0.46, alpha: 1),
                keyword: NSColor(calibratedRed: 0.71, green: 0.54, blue: 0.00, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.83, green: 0.21, blue: 0.51, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                string: NSColor(calibratedRed: 0.16, green: 0.63, blue: 0.60, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.83, green: 0.21, blue: 0.51, alpha: 1),
                number: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                name: NSColor(calibratedRed: 0.51, green: 0.58, blue: 0.59, alpha: 1),
                decorator: NSColor(calibratedRed: 0.59, green: 0.49, blue: 0.00, alpha: 1),
                punctuation: NSColor(calibratedRed: 0.51, green: 0.58, blue: 0.59, alpha: 1),
                operator: NSColor(calibratedRed: 0.51, green: 0.58, blue: 0.59, alpha: 1),
                error: NSColor.systemRed
            )
        ),
        CodeTheme(
            name: "solarized-light",
            palette: Palette(
                background: NSColor(calibratedRed: 0.99, green: 0.96, blue: 0.89, alpha: 1),
                foreground: NSColor(calibratedRed: 0.40, green: 0.48, blue: 0.51, alpha: 1),
                comment: NSColor(calibratedRed: 0.58, green: 0.63, blue: 0.63, alpha: 1),
                keyword: NSColor(calibratedRed: 0.71, green: 0.54, blue: 0.00, alpha: 1),
                keywordType: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                keywordDeclaration: NSColor(calibratedRed: 0.83, green: 0.21, blue: 0.51, alpha: 1),
                keywordConstant: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                string: NSColor(calibratedRed: 0.16, green: 0.63, blue: 0.60, alpha: 1),
                stringEscape: NSColor(calibratedRed: 0.83, green: 0.21, blue: 0.51, alpha: 1),
                number: NSColor(calibratedRed: 0.15, green: 0.55, blue: 0.82, alpha: 1),
                name: NSColor(calibratedRed: 0.40, green: 0.48, blue: 0.51, alpha: 1),
                decorator: NSColor(calibratedRed: 0.59, green: 0.49, blue: 0.00, alpha: 1),
                punctuation: NSColor(calibratedRed: 0.40, green: 0.48, blue: 0.51, alpha: 1),
                operator: NSColor(calibratedRed: 0.40, green: 0.48, blue: 0.51, alpha: 1),
                error: NSColor.systemRed
            )
        ),
    ]
}
