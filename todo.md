# Pygments → Swift Port Progress (TODO)

Last updated: 2026-01-23

## ✅ Implemented (pygments-swift)

Source of truth: `BuiltinLanguage` in `pygments-swift/Sources/PygmentsSwift/LexerRegistry.swift`.

Implemented lexers (languages + formats):
- [x] Swift (`swift`)
- [x] JSON (`json`)
- [x] JSON-LD (`jsonld`)
- [x] Python (`python`)
- [x] JavaScript (`javascript`)
- [x] Java (`java`)
- [x] TypeScript (`typescript`)
- [x] C (`c`)
- [x] C++ (`cpp`)
- [x] C# (`csharp`)
- [x] Go (`go`)
- [x] Rust (`rust`)
- [x] Kotlin (`kotlin`)
- [x] Ruby (`ruby`)
- [x] PHP (`php`)
- [x] Shell / Bash (`shell`)
- [x] Scala (`scala`)
- [x] R (`r`)
- [x] Markdown (`markdown`)
- [x] YAML (`yaml`)
- [x] TOML (`toml`)
- [x] HTML (`html`)
- [x] XML (`xml`)
- [x] CSS (`css`)
- [x] SQL (`sql`)
- [x] Diff (`diff`)
- [x] Dockerfile (`dockerfile`)
- [x] Makefile (`makefile`)
- [x] INI (`ini`)
- [x] Dotenv (`dotenv`)
- [x] Gherkin (`gherkin`)
- [x] Vue (`vue`)
- [x] Svelte (`svelte`)
- [x] Hjson (`hjson`)
- [x] KDL (`kdl`)
- [x] Bicep (`bicep`)
- [x] Dhall (`dhall`)
- [x] Kusto / KQL (`kusto`)
- [x] Lua (`lua`)
- [x] Perl (`perl`)
- [x] Haskell (`haskell`)
- [x] Clojure (`clojure`)
- [x] Elixir (`elixir`)
- [x] Erlang (`erlang`)
- [x] Dart (`dart`)
- [x] Julia (`julia`)
- [x] PowerShell (`powershell`)
- [x] Groovy (`groovy`)
- [x] CMake (`cmake`)
- [x] GraphQL (`graphql`)
- [x] Terraform (`terraform`)
- [x] Nginx (`nginx`)
- [x] Objective-C (`objectivec`)
- [x] Objective-C++ (`objectivecpp`)
- [x] Vim (`vim`)
- [x] Zig (`zig`)
- [x] Nim (`nim`)
- [x] Solidity (`solidity`)
- [x] CoffeeScript (`coffeescript`)
- [x] SCSS (`scss`)
- [x] Less (`less`)
- [x] Haml (`haml`)
- [x] Pug (`pug`)
- [x] Protocol Buffers (`protobuf`)
- [x] OCaml (`ocaml`)
- [x] F# (`fsharp`)
- [x] Fortran (`fortran`)
- [x] Assembly (`assembly`)
- [x] reStructuredText (`restructuredtext`)
- [x] LaTeX (`latex`)
- [x] .gitignore (`gitignore`)
- [x] EditorConfig (`editorconfig`)
- [x] Java .properties (`properties`)
- [x] CSV (`csv`)
- [x] Graphviz / DOT (`graphviz`)
- [x] PlantUML (`plantuml`)
- [x] Mermaid (`mermaid`)
- [x] ApacheConf (`apacheconf`)
- [x] Tcl (`tcl`)
- [x] Awk (`awk`)
- [x] sed (`sed`)
- [x] Batchfile (`batchfile`)
- [x] VBScript (`vbscript`)
- [x] Pascal (`pascal`)
- [x] Racket (`racket`)
- [x] Scheme (`scheme`)
- [x] Common Lisp (`commonlisp`)
- [x] Emacs Lisp (`emacslisp`)
- [x] D (`d`)
- [x] Crystal (`crystal`)
- [x] Elm (`elm`)
- [x] Haxe (`haxe`)
- [x] V (`v`)
- [x] Nix (`nix`)
- [x] Fish (`fish`)
- [x] Raku (`raku`)
- [x] Cue (`cue`)
- [x] Nushell (`nushell`)
- [x] Ada (`ada`)
- [x] COBOL (`cobol`)
- [x] Prolog (`prolog`)
- [x] Smalltalk (`smalltalk`)
- [x] Eiffel (`eiffel`)
- [x] Visual Basic (`visualbasic`)
- [x] SystemVerilog (`systemverilog`)
- [x] VHDL (`vhdl`)
- [x] LLVM (`llvm`)
- [x] GLSL (`glsl`)
- [x] JSON5 (`json5`)
- [x] Jsonnet (`jsonnet`)
- [x] YARA (`yara`)
- [x] YANG (`yang`)
- [x] WGSL (`wgsl`)
- [x] WebAssembly (`webassembly`)
- [x] Web IDL (`webidl`)
- [x] Meson (`meson`)
- [x] GDScript (`gdscript`)
- [x] Teal (`teal`)
- [x] AsciiDoc (`asciidoc`)
- [x] Org mode (`org`)
- [x] Kconfig (`kconfig`)
- [x] Caddyfile (`caddyfile`)
- [x] SPARQL (`sparql`)
- [x] Turtle (`turtle`)
- [x] Thrift (`thrift`)
- [x] Cap’n Proto (`capnproto`)
- [x] QML (`qml`)
- [x] HLSL (`hlsl`)
- [x] BibTeX (`bibtex`)
- [x] ASN.1 (`asn1`)
- [x] CDDL (`cddl`)
- [x] Devicetree (`devicetree`)
- [x] PromQL (`promql`)
- [x] Rego (`rego`)
- [x] JMESPath (`jmespath`)
- [x] PRQL (`prql`)
- [x] Typst (`typst`)
- [x] Smithy (`smithy`)
- [x] ABNF (`abnf`)
- [x] ActionScript (`actionscript`)
- [x] Arduino (`arduino`)
- [x] AutoHotkey (`autohotkey`)
- [x] CUDA (`cuda`)
- [x] Asymptote (`asymptote`)
- [x] Chapel (`chapel`)
- [x] Alloy (`alloy`)
- [x] Augeas (`augeas`)
- [x] CodeQL (`codeql`)
- [x] Procfile (`procfile`)
- [x] PostgreSQL console (`psql`)
- [x] AppleScript (`applescript`)
- [x] ABAP (`abap`)
- [x] APL (`apl`)
- [x] AspectJ (`aspectj`)
- [x] AutoIt (`autoit`)
- [x] ANTLR (`antlr`)
- [x] Angular 2 templates (`angular2`)
- [x] Bash session (`bashsession`)
- [x] Windows Registry (`reg`)
- [x] Slim (`slim`)
- [x] DTD (`dtd`)
- [x] CFML (`cfml`)
- [x] ChaiScript (`chaiscript`)
- [x] ASPX (`aspx`)
- [x] Gnuplot (`gnuplot`)
- [x] FoxPro (`foxpro`)
- [x] BASIC (`basic`)
- [x] POV-Ray (`povray`)
- [x] PL/pgSQL (`plpgsql`)
- [x] OpenSCAD (`openscad`)
- [x] Mojo (`mojo`)
- [x] Liquid (`liquid`)
- [x] Plain text (`text`)

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
- [x] R (pragmatic lexer)
- [x] Scala (pragmatic lexer)
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
  - [pygments-swift/Sources/PygmentsSwift/Lexers/ScalaLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/ScalaLexer.swift)
  - [pygments-swift/Sources/PygmentsSwift/Lexers/RLexer.swift](pygments-swift/Sources/PygmentsSwift/Lexers/RLexer.swift)
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
- [x] Add `TypeScriptLexer` (likely build on JS lexer)
- [x] Add `CLexer` and `CppLexer`
- [x] Add `CSharpLexer`
- [x] Add `GoLexer`
- [x] Add `RustLexer`

### Expand parity corpus
- [ ] Add more Swift snippets covering:
  - [x] nested/multiline comments
  - [x] string interpolation edge cases
  - [x] raw strings / multi-line strings (if supported by the Python lexer)
  - [x] preprocessor branches and directives
- [x] Run strict parity across multiple samples (keep failures actionable)

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
