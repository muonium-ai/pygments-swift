# Pygments → Swift Port Progress (TODO)

Last updated: 2026-01-20

## ✅ Implemented (pygments-swift)

## 🎯 Top-10 Language Support (tracker)

Goal: highlight source code for ~10 widely used programming languages.

- [x] Swift (ported from Pygments SwiftLexer + Swift-only Unicode identifiers)
- [x] Python (pragmatic lexer; not yet full Pygments parity)
- [x] JavaScript (pragmatic lexer; not yet full Pygments parity)
- [x] Java (pragmatic lexer; not yet full Pygments parity)
- [x] TypeScript (pragmatic lexer; not yet full Pygments parity)
- [x] C (pragmatic lexer; not yet full Pygments parity)
- [x] C++ (pragmatic lexer; not yet full Pygments parity)
- [x] C# (pragmatic lexer; not yet full Pygments parity)
- [x] Go (pragmatic lexer; not yet full Pygments parity)
- [x] Rust (pragmatic lexer; not yet full Pygments parity)

## ➕ Next Set (tracker)

Goal: expand coverage beyond the top-10.

- [x] Kotlin (pragmatic lexer)
- [x] PHP (pragmatic lexer)
- [x] Ruby (pragmatic lexer)
- [ ] R
- [ ] Scala
- [x] Bash / Shell (pragmatic lexer)

Non-language formats already supported:
- [x] JSON
- [x] JSON-LD

## 🔗 Key Files (jump points)

- Package: [pygments-swift/Package.swift](pygments-swift/Package.swift)
- Core tokens:
  - [pygments-swift/Sources/PygmentsSwift/TokenType.swift](pygments-swift/Sources/PygmentsSwift/TokenType.swift)
  - [pygments-swift/Sources/PygmentsSwift/Token.swift](pygments-swift/Sources/PygmentsSwift/Token.swift)
- Lexer runtime:
  - [pygments-swift/Sources/PygmentsSwift/Lexer.swift](pygments-swift/Sources/PygmentsSwift/Lexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/RegexLexer.swift](pygments-swift/Sources/PygmentsSwift/RegexLexer.swift)
- Regex helpers:
  - [pygments-swift/Sources/PygmentsSwift/RegexOpt.swift](pygments-swift/Sources/PygmentsSwift/RegexOpt.swift)
  - [pygments-swift/Sources/PygmentsSwift/RegexHelpers.swift](pygments-swift/Sources/PygmentsSwift/RegexHelpers.swift)
- Lexer registry:
  - [pygments-swift/Sources/PygmentsSwift/LexerRegistry.swift](pygments-swift/Sources/PygmentsSwift/LexerRegistry.swift)
- Lexers:
  - [pygments-swift/Sources/PygmentsSwift/Lexers/SwiftLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/SwiftLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/IniLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/IniLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/JsonLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/JsonLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/JsonLdLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/JsonLdLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/PythonLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/PythonLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/JavaScriptLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/JavaScriptLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/JavaLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/JavaLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/TypeScriptLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/TypeScriptLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/CLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/CLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/CppLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/CppLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/CSharpLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/CSharpLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/GoLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/GoLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/RustLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/RustLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/KotlinLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/KotlinLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/RubyLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/RubyLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/PHPLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/PHPLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/ShellLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/ShellLexer.swift)
- Tests:
  - Parity: [pygments-swift/Tests/PygmentsSwiftTests/SwiftLexerParityTests.swift](pygments-swift/Tests/PygmentsSwiftTests/SwiftLexerParityTests.swift)
  - Engine features: [pygments-swift/Tests/PygmentsSwiftTests/RegexLexerFeatureTests.swift](pygments-swift/Tests/PygmentsSwiftTests/RegexLexerFeatureTests.swift)
  - Token types: [pygments-swift/Tests/PygmentsSwiftTests/TokenTypeTests.swift](pygments-swift/Tests/PygmentsSwiftTests/TokenTypeTests.swift)
- Python reference runner (used by parity tests):
  - [pygments-swift/Tests/Support/pygments_swift_ref.py](pygments-swift/Tests/Support/pygments_swift_ref.py)

### Project / packaging
- [x] SwiftPM package scaffolded (PygmentsSwift) targeting macOS
- [x] XCTest test suite wired up; `swift test` passing

### Core model
- [x] `TokenType` model with hierarchy/subtype checks and parsing from string
- [x] `Token` model includes:
  - [x] `start` (UTF-16 offset, matches `NSString`/`NSRange` world)
  - [x] `startScalar` (Unicode-scalar offset for parity with Python indices)

### Lexer runtime
- [x] Pygments-like preprocessing for inputs (newline normalization, etc.)
- [x] Regex/state-machine lexer engine (RegexLexer-style)
- [x] State stack semantics (push/pop/switch)
- [x] Rule actions:
  - [x] single token
  - [x] `byGroups(...)`
  - [x] `using(...)` (delegate lexing to another lexer)
  - [x] `usingThis(...)` (delegate lexing to same lexer type)
  - [x] `default(...)` (zero-length transition; maps to `(?:)` for `NSRegularExpression`)
- [x] State composition:
  - [x] `.include("state")`
  - [x] `combined(...)` support via temporary runtime-composed state
  - [x] `inherit` support: merge/splice parent states during compilation
- [x] Loop-safety for zero-length matches (prevents infinite loops)

### Regex helpers
- [x] Ported Pygments `regexopt` algorithm (keyword list → optimized regex)
- [x] `words(...)` helper (prefix/suffix + optimized alternation)

### Lexers
- [x] `IniLexer` (sanity / baseline)
- [x] `SwiftLexer` ported from Python Pygments (`pygments.lexers.objective.SwiftLexer`)
  - [x] Major states implemented (root/keywords/comments/strings/preproc/module/etc.)
  - [x] Expanded builtin patterns to match Pygments lists
  - [x] Unicode-aware identifier matching in Swift port (XID_Start/XID_Continue)
- [x] `JsonLexer` ported from Python Pygments (`pygments.lexers.data.JsonLexer`)
  - [x] Supports object-key reclassification (`String.Double` → `Name.Tag` before `:`)
  - [x] Supports JS-style comments like Pygments (`//`, `/* ... */`)
- [x] `JsonLdLexer` ported from Python Pygments (`pygments.lexers.data.JsonLdLexer`)
  - [x] Retokenizes JSON-LD keywords (`"@context"`, `"@id"`, etc.) as `Name.Decorator`
- [x] `PythonLexer` (pragmatic)
- [x] `JavaScriptLexer` (pragmatic)
- [x] `JavaLexer` (pragmatic)
- [x] `TypeScriptLexer` (pragmatic)
- [x] `CLexer` (pragmatic)
- [x] `CppLexer` (pragmatic)
- [x] `CSharpLexer` (pragmatic)
- [x] `GoLexer` (pragmatic)
- [x] `RustLexer` (pragmatic)

### Parity testing vs Python Pygments
- [x] Python reference runner script emits JSON tokens (from in-repo `pygments-master`)
- [x] Python reference uses `_preprocess_lexer_input` to match real Pygments behavior
- [x] Strict parity test for an ASCII sample (type/value/start)
- [x] Unicode parity coverage via `startScalar` comparisons
- [x] Python parity supports selecting lexer via `PYGMENTS_LEXER` (swift/json)
- [x] Python parity supports `PYGMENTS_LEXER=jsonld`

---

## ⏳ Pending / Next (recommended order)

### Top-10 lexers to add next
- [ ] Add `TypeScriptLexer` (likely build on JS lexer)
- [ ] Add `CLexer` and `CppLexer`
- [ ] Add `CSharpLexer`
- [ ] Add `GoLexer`
- [ ] Add `RustLexer`

### Expand parity corpus
- [ ] Add more Swift snippets covering:
  - [ ] nested/multiline comments
  - [ ] string interpolation edge cases
  - [ ] raw strings / multi-line strings (if supported by the Python lexer)
  - [ ] preprocessor branches and directives
- [ ] Run strict parity across multiple samples (keep failures actionable)

### Engine completeness / polish
- [ ] Performance pass for `startScalar` mapping (optimize if it becomes hot)
- [ ] Improve error reporting for no-match situations (diagnostics & debugging helpers)

### Scope expansion
- [x] Port one more “real” lexer to validate engine generality (JSON)
- [ ] Decide how formatters will be handled in Swift (later milestone)

---

## Notes
- Parity strategy: Swift tests call a Python runner against `pygments-master` and compare token streams.
- Current parity focus is the Swift lexer (good stress test) plus engine feature tests.
