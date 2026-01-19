import Foundation

public enum BuiltinLanguage: String, CaseIterable, Sendable {
    case swift
    case json
    case jsonld
    case python
    case javascript
    case java
    case typescript
    case c
    case cpp
    case csharp
    case go
    case rust
    case kotlin
    case ruby
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
        case .typescript:
            return TypeScriptLexer(options: options)
        case .c:
            return CLexer(options: options)
        case .cpp:
            return CppLexer(options: options)
        case .csharp:
            return CSharpLexer(options: options)
        case .go:
            return GoLexer(options: options)
        case .rust:
            return RustLexer(options: options)
        case .kotlin:
            return KotlinLexer(options: options)
        case .ruby:
            return RubyLexer(options: options)
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
        case "typescript", "ts":
            return makeLexer(language: .typescript, options: options)
        case "c":
            return makeLexer(language: .c, options: options)
        case "c++", "cpp", "cplusplus", "cxx":
            return makeLexer(language: .cpp, options: options)
        case "c#", "csharp", "cs":
            return makeLexer(language: .csharp, options: options)
        case "go", "golang":
            return makeLexer(language: .go, options: options)
        case "rust", "rs":
            return makeLexer(language: .rust, options: options)
        case "kotlin", "kt":
            return makeLexer(language: .kotlin, options: options)
        case "ruby", "rb":
            return makeLexer(language: .ruby, options: options)
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
        case "ts", "tsx":
            return makeLexer(language: .typescript, options: options)
        case "c", "h":
            return makeLexer(language: .c, options: options)
        case "cc", "cpp", "cxx", "hh", "hpp", "hxx":
            return makeLexer(language: .cpp, options: options)
        case "cs":
            return makeLexer(language: .csharp, options: options)
        case "go":
            return makeLexer(language: .go, options: options)
        case "rs":
            return makeLexer(language: .rust, options: options)
        case "kt", "kts":
            return makeLexer(language: .kotlin, options: options)
        case "rb", "erb", "rake":
            return makeLexer(language: .ruby, options: options)
        default:
            return nil
        }
    }
}
