import AppKit
import Foundation
import PygmentsSwift

do {
    let opts = try CLIOptions.parse(CommandLine.arguments)
    let inputURL = URL(fileURLWithPath: opts.inputPath)
    let outDirURL = URL(fileURLWithPath: opts.outDir, isDirectory: true)

    let source = try String(contentsOf: inputURL, encoding: .utf8)

    let lexer: Lexer = {
        if let lang = opts.languageOverride {
            if let lx = LexerRegistry.makeLexer(languageName: lang) {
                return lx
            }
            // Fall back to file-based detection even if --lang was provided but unknown.
        }
        if let lx = LexerRegistry.makeLexer(filename: inputURL.lastPathComponent) {
            return lx
        }
        // As a last resort, treat as plain text via RegexLexer root that emits Text.
        // (We don't have a full TextLexer yet.)
        final class FallbackTextLexer: RegexLexer {
            override var tokenDefs: [String: [TokenRuleDef]] {
                [
                    "root": [
                        .rule(Rule(".+", options: [.dotMatchesLineSeparators], action: .token(.text))),
                    ]
                ]
            }
        }
        return FallbackTextLexer()
    }()

    let theme: CodeTheme = try {
        if let themeFile = opts.themeFile {
            if themeFile == "-" {
                let data = FileHandle.standardInput.readDataToEndOfFile()
                if data.isEmpty {
                    throw CLIError("--theme-file - expects theme JSON on stdin")
                }
                let ht = try UserThemeFile.load(data: data, nameHint: "stdin-theme")
                return CodeTheme(theme: ht)
            } else {
                let url = URL(fileURLWithPath: themeFile)
                let ht = try UserThemeFile.load(url: url)
                return CodeTheme(theme: ht)
            }
        }
        return try CodeTheme.named(opts.theme)
    }()
    let font = NSFont.monospacedSystemFont(ofSize: CGFloat(opts.fontSize), weight: .regular)

    let attributed = CodeHighlighter.highlight(text: source, lexer: lexer, theme: theme, font: font)

    try FileManager.default.createDirectory(at: outDirURL, withIntermediateDirectories: true)

    // Use the full filename to avoid collisions like fibonacci.py vs fibonacci.rs.
    let outputPrefix = inputURL.lastPathComponent
    let pdfURL = outDirURL.appendingPathComponent(outputPrefix + ".pdf")
    let pngURL = outDirURL.appendingPathComponent(outputPrefix + ".png")

    let renderOptions = RenderOptions(
        width: opts.width.map { CGFloat($0) },
        padding: 18,
        background: theme.background,
        foreground: theme.defaultForeground
    )

    let pdfData = CodeRender.renderPDF(attributed: attributed, options: renderOptions)
    try pdfData.write(to: pdfURL, options: .atomic)

    let pngData = try CodeRender.renderPNG(attributed: attributed, options: renderOptions)
    try pngData.write(to: pngURL, options: .atomic)

    fputs("Wrote: \(pdfURL.path)\n", stderr)
    fputs("Wrote: \(pngURL.path)\n", stderr)
} catch let h as CLIHelp {
    print(h.text)
} catch {
    fputs("Error: \(error.localizedDescription)\n", stderr)
    exit(2)
}
