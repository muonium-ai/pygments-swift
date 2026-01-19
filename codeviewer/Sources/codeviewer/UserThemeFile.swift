import Foundation
import PygmentsSwift

private enum UserThemeFileFormat {
    static let templateJSON: String = """
    {
      \"name\": \"my-theme\",
      \"palette\": {
        \"background\": \"#0D1117\",
        \"foreground\": \"#C9D1D9\",
        \"comment\": \"#8B949E\",
        \"keyword\": \"#79C0FF\",
        \"keywordType\": \"#56D4DD\",
        \"keywordDeclaration\": \"#D2A8FF\",
        \"keywordConstant\": \"#FFA657\",
        \"string\": \"#A5D6FF\",
        \"stringEscape\": \"#FFA657\",
        \"number\": \"#FFA657\",
        \"name\": \"#C9D1D9\",
        \"decorator\": \"#7EE787\",
        \"punctuation\": \"#B1BAC4\",
        \"operator\": \"#B1BAC4\",
        \"error\": \"#FF7B72\"
      }
    }
    """
}

struct UserThemeFile: Decodable {
    struct Palette: Decodable {
        let background: String
        let foreground: String
        let comment: String
        let keyword: String
        let keywordType: String
        let keywordDeclaration: String
        let keywordConstant: String
        let string: String
        let stringEscape: String
        let number: String
        let name: String
        let decorator: String
        let punctuation: String
        let `operator`: String
        let error: String
    }

    let name: String?
    let palette: Palette

    static func template() -> String {
        UserThemeFileFormat.templateJSON
    }

    static func load(data: Data, nameHint: String?) throws -> HighlightTheme {
        let decoder = JSONDecoder()
        let parsed = try decoder.decode(UserThemeFile.self, from: data)

        let themeName = (parsed.name?.trimmingCharacters(in: .whitespacesAndNewlines)).flatMap { $0.isEmpty ? nil : $0 }
            ?? nameHint
            ?? "user-theme"

        func c(_ s: String) throws -> RGBAColor {
            try RGBAColorParser.parse(s)
        }

        let p = parsed.palette
        return HighlightTheme(
            name: themeName.lowercased(),
            palette: HighlightTheme.Palette(
                background: try c(p.background),
                foreground: try c(p.foreground),
                comment: try c(p.comment),
                keyword: try c(p.keyword),
                keywordType: try c(p.keywordType),
                keywordDeclaration: try c(p.keywordDeclaration),
                keywordConstant: try c(p.keywordConstant),
                string: try c(p.string),
                stringEscape: try c(p.stringEscape),
                number: try c(p.number),
                name: try c(p.name),
                decorator: try c(p.decorator),
                punctuation: try c(p.punctuation),
                operator: try c(p.`operator`),
                error: try c(p.error)
            )
        )
    }

    static func load(url: URL) throws -> HighlightTheme {
        let data = try Data(contentsOf: url)
        let hint = url.deletingPathExtension().lastPathComponent
        return try load(data: data, nameHint: hint)
    }
}

enum RGBAColorParser {
    static func parse(_ raw: String) throws -> RGBAColor {
        let s = raw.trimmingCharacters(in: .whitespacesAndNewlines)

        // Supported:
        // - "#RRGGBB"
        // - "RRGGBB"
        // - "0xRRGGBB"
        // - "#RRGGBBAA" (alpha in last 2 hex digits)
        let lower = s.lowercased()
        let hex: String
        if lower.hasPrefix("#") {
            hex = String(lower.dropFirst())
        } else if lower.hasPrefix("0x") {
            hex = String(lower.dropFirst(2))
        } else {
            hex = lower
        }

        guard hex.count == 6 || hex.count == 8 else {
            throw CLIError("Invalid color: \(raw). Expected #RRGGBB or #RRGGBBAA")
        }

        func parseByte(_ substring: Substring) throws -> Int {
            guard let v = Int(substring, radix: 16) else {
                throw CLIError("Invalid color: \(raw)")
            }
            return v
        }

        let r = try parseByte(hex.prefix(2))
        let g = try parseByte(hex.dropFirst(2).prefix(2))
        let b = try parseByte(hex.dropFirst(4).prefix(2))
        let a: Double
        if hex.count == 8 {
            let alphaByte = try parseByte(hex.dropFirst(6).prefix(2))
            a = Double(alphaByte) / 255.0
        } else {
            a = 1.0
        }
        return RGBAColor(r, g, b, alpha: a)
    }
}
