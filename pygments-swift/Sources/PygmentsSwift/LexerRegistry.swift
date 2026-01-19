import Foundation

public enum BuiltinLanguage: String, CaseIterable, Sendable {
    case swift
    case json
    case jsonld
    case python
    case javascript
    case java
}

public enum LexerRegistry {
    public static func makeLexer(language: BuiltinLanguage, options: LexerOptions = .init()) -> Lexer {
        switch language {
        case .swift:
            return SwiftLexer(options: options)
        case .json:
            return JsonLexer(options: options)
        case .jsonld:
            return JsonLdLexer(options: options)
        case .python:
            return PythonLexer(options: options)
        case .javascript:
            return JavaScriptLexer(options: options)
        case .java:
            return JavaLexer(options: options)
        }
    }

    /// Returns a lexer for a language name (e.g. "python", "js", "swift").
    public static func makeLexer(languageName: String, options: LexerOptions = .init()) -> Lexer? {
        let normalized = languageName.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        switch normalized {
        case "swift":
            return makeLexer(language: .swift, options: options)
        case "json":
            return makeLexer(language: .json, options: options)
        case "jsonld", "json-ld":
            return makeLexer(language: .jsonld, options: options)
        case "python", "py":
            return makeLexer(language: .python, options: options)
        case "javascript", "js":
            return makeLexer(language: .javascript, options: options)
        case "java":
            return makeLexer(language: .java, options: options)
        default:
            return nil
        }
    }

    /// Returns a lexer for a filename extension (e.g. ".py", "js", "java").
    public static func makeLexer(filename: String, options: LexerOptions = .init()) -> Lexer? {
        let ext = (filename as NSString).pathExtension.lowercased()
        switch ext {
        case "swift":
            return makeLexer(language: .swift, options: options)
        case "json", "jsonl", "ndjson":
            return makeLexer(language: .json, options: options)
        case "jsonld":
            return makeLexer(language: .jsonld, options: options)
        case "py":
            return makeLexer(language: .python, options: options)
        case "js", "mjs", "cjs":
            return makeLexer(language: .javascript, options: options)
        case "java":
            return makeLexer(language: .java, options: options)
        default:
            return nil
        }
    }
}
