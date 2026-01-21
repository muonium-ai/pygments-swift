import AppKit
import Foundation
import PygmentsSwift

private func cssLikeRGBA(_ color: NSColor) -> String {
    let c = color.usingColorSpace(.deviceRGB) ?? color
    return String(format: "rgba(%d,%d,%d,%.3f)",
                  Int((c.redComponent * 255.0).rounded()),
                  Int((c.greenComponent * 255.0).rounded()),
                  Int((c.blueComponent * 255.0).rounded()),
                  c.alphaComponent)
}

private func hexRGBA(_ color: NSColor) -> String {
    let c = color.usingColorSpace(.deviceRGB) ?? color
    let r = Int((c.redComponent * 255.0).rounded())
    let g = Int((c.greenComponent * 255.0).rounded())
    let b = Int((c.blueComponent * 255.0).rounded())
    let a = Int((c.alphaComponent * 255.0).rounded())
    return String(format: "#%02X%02X%02X%02X", r, g, b, a)
}

do {
    let opts = try CLIOptions.parse(CommandLine.arguments)

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

    if opts.printThemeColors {
        let bg = hexRGBA(theme.background)
        let fg = hexRGBA(theme.defaultForeground)
        print("{\"name\":\"\(theme.name)\",\"background\":\"\(bg)\",\"foreground\":\"\(fg)\"}")
        exit(0)
    }

    guard let inputPath = opts.inputPath else {
        throw CLIError("Missing <input-file>")
    }

    let inputURL = URL(fileURLWithPath: inputPath)
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

    let tokens = lexer.getTokens(source)

    let font = NSFont.monospacedSystemFont(ofSize: CGFloat(opts.fontSize), weight: .regular)

    if opts.dumpTokenSummary {
        var counts: [String: Int] = [:]
        for t in tokens {
            let k = t.type.description
            counts[k, default: 0] += 1
        }
        let top = counts.sorted { $0.value > $1.value }.prefix(40)
        fputs("Token summary (top \(top.count)):\n", stderr)
        for (k, v) in top {
            fputs("  \(v)\t\(k)\n", stderr)
        }
    }

    let attributed = CodeHighlighter.highlight(tokens: tokens, theme: theme, font: font)

    if opts.dumpAttributeSummary {
        var counts: [String: Int] = [:]
        attributed.enumerateAttribute(.foregroundColor, in: NSRange(location: 0, length: attributed.length)) { v, range, _ in
            if let c = v as? NSColor {
                counts[cssLikeRGBA(c), default: 0] += range.length
            } else {
                counts["(nil)", default: 0] += range.length
            }
        }
        let top = counts.sorted { $0.value > $1.value }.prefix(20)
        fputs("Foreground color runs: \(counts.count) distinct (top \(top.count))\n", stderr)
        for (k, v) in top {
            fputs("  \(v)\t\(k)\n", stderr)
        }
    }

    try FileManager.default.createDirectory(at: outDirURL, withIntermediateDirectories: true)

    // Use the full filename to avoid collisions like fibonacci.py vs fibonacci.rs.
    let outputPrefix = inputURL.lastPathComponent
    let pdfURL = outDirURL.appendingPathComponent(outputPrefix + ".pdf")
    let pngURL = outDirURL.appendingPathComponent(outputPrefix + ".png")
    let htmlURL = outDirURL.appendingPathComponent(outputPrefix + ".html")

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

    if opts.emitHTML {
        let html = HTMLRender.renderHTML(tokens: tokens, theme: theme.theme, title: outputPrefix)
        try html.write(to: htmlURL, atomically: true, encoding: .utf8)
        fputs("Wrote: \(htmlURL.path)\n", stderr)
    }

    fputs("Wrote: \(pdfURL.path)\n", stderr)
    fputs("Wrote: \(pngURL.path)\n", stderr)
} catch let h as CLIHelp {
    print(h.text)
} catch {
    fputs("Error: \(error.localizedDescription)\n", stderr)
    exit(2)
}
