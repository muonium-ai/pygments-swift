import Foundation

struct CLIOptions {
    var inputPath: String?
    var outDir: String
    var languageOverride: String?
    var theme: String
    var themeFile: String?
    var emitHTML: Bool
    var dumpTokenSummary: Bool
    var dumpAttributeSummary: Bool
    var printThemeColors: Bool
    var fontSize: Double
    var width: Double?

    static func usage(program: String) -> String {
        return """
        Usage:
                    \(program) <input-file> [--outdir <dir>] [--lang <name>] [--theme <name>] [--theme-file <path>] [--html] [--dump-token-summary] [--dump-attribute-summary] [--font-size <n>] [--width <n>]
                    \(program) --list-themes
                    \(program) --print-theme-template
                    \(program) --print-theme-colors [--theme <name>] [--theme-file <path>]

        Examples:
          \(program) MyFile.swift
                    \(program) MyFile.py --outdir out --theme github-light
                    \(program) MyFile.py --outdir out --theme-file my-theme.json
                                        \(program) MyFile.py --outdir out --html
                                        \(program) MyFile.py --dump-token-summary
                                        \(program) MyFile.py --dump-attribute-summary
          \(program) unknown.txt --lang swift
                    \(program) --list-themes
                                        \(program) --print-theme-template > my-theme.json
                                                    \(program) --print-theme-colors --theme github-dark

        Notes:
                    - Output files are written as <filename>.<ext>.pdf and <filename>.<ext>.png in --outdir (default: current directory)
          - Lexer selection uses filename extension unless --lang is provided
                    - --theme-file overrides --theme
                    - Use --theme-file - to read the theme JSON from stdin
                    - --html writes an additional <filename>.<ext>.html file for easy inspection in a browser
                    - --dump-attribute-summary prints how many distinct foreground colors are present in the rendered attributed string (helps debug “all text same color” issues)
        """
    }

    static func parse(_ args: [String]) throws -> CLIOptions {
        let program = (args.first as NSString?)?.lastPathComponent ?? "codeviewer"
        var it = args.dropFirst().makeIterator()

        func requireValue(_ flag: String) throws -> String {
            guard let v = it.next(), !v.hasPrefix("--") else {
                throw CLIError("Missing value for \(flag)\n\n\(usage(program: program))")
            }
            return v
        }

        var inputPath: String?
        var outDir = FileManager.default.currentDirectoryPath
        var languageOverride: String?
        var theme = "github-dark"
        var themeFile: String?
        var emitHTML = false
        var dumpTokenSummary = false
        var dumpAttributeSummary = false
        var printThemeColors = false
        var fontSize: Double = 13
        var width: Double?

        while let a = it.next() {
            switch a {
            case "-h", "--help":
                throw CLIHelp(usage(program: program))
            case "--list-themes":
                throw CLIHelp(CodeTheme.allNames.joined(separator: "\n"))
            case "--print-theme-template":
                throw CLIHelp(UserThemeFile.template())
            case "--print-theme-colors":
                printThemeColors = true
            case "--outdir":
                outDir = try requireValue(a)
            case "--lang":
                languageOverride = try requireValue(a)
            case "--theme":
                theme = try requireValue(a)
            case "--theme-file":
                themeFile = try requireValue(a)
            case "--html":
                emitHTML = true
            case "--dump-token-summary":
                dumpTokenSummary = true
            case "--dump-attribute-summary":
                dumpAttributeSummary = true
            case "--font-size":
                let v = try requireValue(a)
                guard let n = Double(v), n > 0 else { throw CLIError("Invalid --font-size: \(v)") }
                fontSize = n
            case "--width":
                let v = try requireValue(a)
                guard let n = Double(v), n > 0 else { throw CLIError("Invalid --width: \(v)") }
                width = n
            default:
                if a.hasPrefix("--") {
                    throw CLIError("Unknown option: \(a)\n\n\(usage(program: program))")
                }
                if inputPath == nil {
                    inputPath = a
                } else {
                    throw CLIError("Unexpected argument: \(a)\n\n\(usage(program: program))")
                }
            }
        }

        if !printThemeColors {
            guard inputPath != nil else {
                throw CLIError("Missing <input-file>\n\n\(usage(program: program))")
            }
        }

        if themeFile != nil {
            // Theme file wins, but we still allow --theme for backwards compat.
        }

        return CLIOptions(
            inputPath: inputPath,
            outDir: outDir,
            languageOverride: languageOverride,
            theme: theme,
            themeFile: themeFile,
            emitHTML: emitHTML,
            dumpTokenSummary: dumpTokenSummary,
            dumpAttributeSummary: dumpAttributeSummary,
            printThemeColors: printThemeColors,
            fontSize: fontSize,
            width: width
        )
    }
}

struct CLIError: LocalizedError {
    let message: String
    init(_ message: String) { self.message = message }
    var errorDescription: String? { message }
}

struct CLIHelp: Error {
    let text: String
    init(_ text: String) { self.text = text }
}
