public struct HighlightTheme: Hashable, Sendable {
    public struct Palette: Hashable, Sendable {
        public let background: RGBAColor
        public let foreground: RGBAColor
        public let comment: RGBAColor
        public let keyword: RGBAColor
        public let keywordType: RGBAColor
        public let keywordDeclaration: RGBAColor
        public let keywordConstant: RGBAColor
        public let string: RGBAColor
        public let stringEscape: RGBAColor
        public let number: RGBAColor
        public let name: RGBAColor
        public let decorator: RGBAColor
        public let punctuation: RGBAColor
        public let `operator`: RGBAColor
        public let error: RGBAColor

        public init(
            background: RGBAColor,
            foreground: RGBAColor,
            comment: RGBAColor,
            keyword: RGBAColor,
            keywordType: RGBAColor,
            keywordDeclaration: RGBAColor,
            keywordConstant: RGBAColor,
            string: RGBAColor,
            stringEscape: RGBAColor,
            number: RGBAColor,
            name: RGBAColor,
            decorator: RGBAColor,
            punctuation: RGBAColor,
            operator: RGBAColor,
            error: RGBAColor
        ) {
            self.background = background
            self.foreground = foreground
            self.comment = comment
            self.keyword = keyword
            self.keywordType = keywordType
            self.keywordDeclaration = keywordDeclaration
            self.keywordConstant = keywordConstant
            self.string = string
            self.stringEscape = stringEscape
            self.number = number
            self.name = name
            self.decorator = decorator
            self.punctuation = punctuation
            self.operator = `operator`
            self.error = error
        }
    }

    public enum ThemeError: Error, CustomStringConvertible {
        case unknownTheme(String)

        public var description: String {
            switch self {
            case .unknownTheme(let name):
                return "Unknown theme: \(name)"
            }
        }
    }

    public let name: String
    public let palette: Palette

    public init(name: String, palette: Palette) {
        self.name = name
        self.palette = palette
    }

    public static var allNames: [String] {
        all.map { $0.name }.sorted()
    }

    public static func named(_ raw: String) throws -> HighlightTheme {
        let key = raw.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        if let theme = all.first(where: { $0.name == key }) {
            return theme
        }
        throw ThemeError.unknownTheme(raw)
    }

    public var background: RGBAColor { palette.background }
    public var defaultForeground: RGBAColor { palette.foreground }

    public func foreground(for type: TokenType) -> RGBAColor {
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

    // 10 built-in themes intended to look good both on-screen (editor) and in exported PNG/PDF.
    private static let all: [HighlightTheme] = [
        HighlightTheme(
            name: "github-dark",
            palette: Palette(
                background: RGBAColor(hex: 0x0D1117),
                foreground: RGBAColor(hex: 0xC9D1D9),
                comment: RGBAColor(hex: 0x8B949E),
                keyword: RGBAColor(hex: 0x79C0FF),
                keywordType: RGBAColor(hex: 0x56D4DD),
                keywordDeclaration: RGBAColor(hex: 0xD2A8FF),
                keywordConstant: RGBAColor(hex: 0xFFA657),
                string: RGBAColor(hex: 0xA5D6FF),
                stringEscape: RGBAColor(hex: 0xFFA657),
                number: RGBAColor(hex: 0xFFA657),
                name: RGBAColor(hex: 0xC9D1D9),
                decorator: RGBAColor(hex: 0x7EE787),
                punctuation: RGBAColor(hex: 0xB1BAC4),
                operator: RGBAColor(hex: 0xB1BAC4),
                error: RGBAColor(hex: 0xFF7B72)
            )
        ),
        HighlightTheme(
            name: "github-light",
            palette: Palette(
                background: RGBAColor(hex: 0xFFFFFF),
                foreground: RGBAColor(hex: 0x24292F),
                comment: RGBAColor(hex: 0x6E7781),
                keyword: RGBAColor(hex: 0x0550AE),
                keywordType: RGBAColor(hex: 0x0A3069),
                keywordDeclaration: RGBAColor(hex: 0x8250DF),
                keywordConstant: RGBAColor(hex: 0x953800),
                string: RGBAColor(hex: 0x0A3069),
                stringEscape: RGBAColor(hex: 0x953800),
                number: RGBAColor(hex: 0x953800),
                name: RGBAColor(hex: 0x24292F),
                decorator: RGBAColor(hex: 0x116329),
                punctuation: RGBAColor(hex: 0x24292F),
                operator: RGBAColor(hex: 0x24292F),
                error: RGBAColor(hex: 0xCF222E)
            )
        ),
        HighlightTheme(
            name: "dracula",
            palette: Palette(
                background: RGBAColor(hex: 0x282A36),
                foreground: RGBAColor(hex: 0xF8F8F2),
                comment: RGBAColor(hex: 0x6272A4),
                keyword: RGBAColor(hex: 0xBD93F9),
                keywordType: RGBAColor(hex: 0x8BE9FD),
                keywordDeclaration: RGBAColor(hex: 0xFF79C6),
                keywordConstant: RGBAColor(hex: 0xF1FA8C),
                string: RGBAColor(hex: 0xF1FA8C),
                stringEscape: RGBAColor(hex: 0xFFB86C),
                number: RGBAColor(hex: 0xBD93F9),
                name: RGBAColor(hex: 0xF8F8F2),
                decorator: RGBAColor(hex: 0x50FA7B),
                punctuation: RGBAColor(hex: 0xF8F8F2),
                operator: RGBAColor(hex: 0xFF79C6),
                error: RGBAColor(hex: 0xFF5555)
            )
        ),
        HighlightTheme(
            name: "monokai",
            palette: Palette(
                background: RGBAColor(hex: 0x1E1E1E),
                foreground: RGBAColor(hex: 0xF8F8F2),
                comment: RGBAColor(hex: 0x75715E),
                keyword: RGBAColor(hex: 0xF92672),
                keywordType: RGBAColor(hex: 0x66D9EF),
                keywordDeclaration: RGBAColor(hex: 0xF92672),
                keywordConstant: RGBAColor(hex: 0xAE81FF),
                string: RGBAColor(hex: 0xE6DB74),
                stringEscape: RGBAColor(hex: 0xFD971F),
                number: RGBAColor(hex: 0xAE81FF),
                name: RGBAColor(hex: 0xF8F8F2),
                decorator: RGBAColor(hex: 0xA6E22E),
                punctuation: RGBAColor(hex: 0xF8F8F2),
                operator: RGBAColor(hex: 0xF92672),
                error: RGBAColor(hex: 0xFF5555)
            )
        ),
        HighlightTheme(
            name: "solarized-dark",
            palette: Palette(
                background: RGBAColor(hex: 0x002B36),
                foreground: RGBAColor(hex: 0x839496),
                comment: RGBAColor(hex: 0x586E75),
                keyword: RGBAColor(hex: 0xB58900),
                keywordType: RGBAColor(hex: 0x268BD2),
                keywordDeclaration: RGBAColor(hex: 0x6C71C4),
                keywordConstant: RGBAColor(hex: 0x2AA198),
                string: RGBAColor(hex: 0x2AA198),
                stringEscape: RGBAColor(hex: 0xCB4B16),
                number: RGBAColor(hex: 0xD33682),
                name: RGBAColor(hex: 0x839496),
                decorator: RGBAColor(hex: 0x859900),
                punctuation: RGBAColor(hex: 0x93A1A1),
                operator: RGBAColor(hex: 0x93A1A1),
                error: RGBAColor(hex: 0xDC322F)
            )
        ),
        HighlightTheme(
            name: "solarized-light",
            palette: Palette(
                background: RGBAColor(hex: 0xFDF6E3),
                foreground: RGBAColor(hex: 0x586E75),
                comment: RGBAColor(hex: 0x93A1A1),
                keyword: RGBAColor(hex: 0xB58900),
                keywordType: RGBAColor(hex: 0x268BD2),
                keywordDeclaration: RGBAColor(hex: 0x6C71C4),
                keywordConstant: RGBAColor(hex: 0x2AA198),
                string: RGBAColor(hex: 0x2AA198),
                stringEscape: RGBAColor(hex: 0xCB4B16),
                number: RGBAColor(hex: 0xD33682),
                name: RGBAColor(hex: 0x586E75),
                decorator: RGBAColor(hex: 0x859900),
                punctuation: RGBAColor(hex: 0x657B83),
                operator: RGBAColor(hex: 0x657B83),
                error: RGBAColor(hex: 0xDC322F)
            )
        ),
        HighlightTheme(
            name: "nord",
            palette: Palette(
                background: RGBAColor(hex: 0x2E3440),
                foreground: RGBAColor(hex: 0xD8DEE9),
                comment: RGBAColor(hex: 0x616E88),
                keyword: RGBAColor(hex: 0x81A1C1),
                keywordType: RGBAColor(hex: 0x8FBCBB),
                keywordDeclaration: RGBAColor(hex: 0xB48EAD),
                keywordConstant: RGBAColor(hex: 0xEBCB8B),
                string: RGBAColor(hex: 0xA3BE8C),
                stringEscape: RGBAColor(hex: 0xEBCB8B),
                number: RGBAColor(hex: 0xD08770),
                name: RGBAColor(hex: 0xD8DEE9),
                decorator: RGBAColor(hex: 0x88C0D0),
                punctuation: RGBAColor(hex: 0xE5E9F0),
                operator: RGBAColor(hex: 0xE5E9F0),
                error: RGBAColor(hex: 0xBF616A)
            )
        ),
        HighlightTheme(
            name: "one-dark",
            palette: Palette(
                background: RGBAColor(hex: 0x282C34),
                foreground: RGBAColor(hex: 0xABB2BF),
                comment: RGBAColor(hex: 0x5C6370),
                keyword: RGBAColor(hex: 0xC678DD),
                keywordType: RGBAColor(hex: 0x56B6C2),
                keywordDeclaration: RGBAColor(hex: 0xE06C75),
                keywordConstant: RGBAColor(hex: 0xD19A66),
                string: RGBAColor(hex: 0x98C379),
                stringEscape: RGBAColor(hex: 0xD19A66),
                number: RGBAColor(hex: 0xD19A66),
                name: RGBAColor(hex: 0xABB2BF),
                decorator: RGBAColor(hex: 0x61AFEF),
                punctuation: RGBAColor(hex: 0xABB2BF),
                operator: RGBAColor(hex: 0xABB2BF),
                error: RGBAColor(hex: 0xE06C75)
            )
        ),
        HighlightTheme(
            name: "gruvbox-dark",
            palette: Palette(
                background: RGBAColor(hex: 0x282828),
                foreground: RGBAColor(hex: 0xEBDBB2),
                comment: RGBAColor(hex: 0x928374),
                keyword: RGBAColor(hex: 0xFB4934),
                keywordType: RGBAColor(hex: 0x83A598),
                keywordDeclaration: RGBAColor(hex: 0xD3869B),
                keywordConstant: RGBAColor(hex: 0xFABD2F),
                string: RGBAColor(hex: 0xB8BB26),
                stringEscape: RGBAColor(hex: 0xFE8019),
                number: RGBAColor(hex: 0xD3869B),
                name: RGBAColor(hex: 0xEBDBB2),
                decorator: RGBAColor(hex: 0x8EC07C),
                punctuation: RGBAColor(hex: 0xEBDBB2),
                operator: RGBAColor(hex: 0xEBDBB2),
                error: RGBAColor(hex: 0xFB4934)
            )
        ),
        HighlightTheme(
            name: "tokyo-night",
            palette: Palette(
                background: RGBAColor(hex: 0x1A1B26),
                foreground: RGBAColor(hex: 0xC0CAF5),
                comment: RGBAColor(hex: 0x565F89),
                keyword: RGBAColor(hex: 0x7AA2F7),
                keywordType: RGBAColor(hex: 0x2AC3DE),
                keywordDeclaration: RGBAColor(hex: 0xBB9AF7),
                keywordConstant: RGBAColor(hex: 0xFF9E64),
                string: RGBAColor(hex: 0x9ECE6A),
                stringEscape: RGBAColor(hex: 0xFF9E64),
                number: RGBAColor(hex: 0xFF9E64),
                name: RGBAColor(hex: 0xC0CAF5),
                decorator: RGBAColor(hex: 0x73DACA),
                punctuation: RGBAColor(hex: 0xC0CAF5),
                operator: RGBAColor(hex: 0xC0CAF5),
                error: RGBAColor(hex: 0xF7768E)
            )
        ),
    ]
}
