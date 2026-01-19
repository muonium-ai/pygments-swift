# Pygments Port

This repo ports the Python **Pygments** syntax highlighting library into other languages.

## Swift port

The Swift implementation lives in [pygments-swift](pygments-swift/) as a SwiftPM package.

### What’s included

- A Pygments-inspired `RegexLexer` engine (state machine with push/pop, include/inherit/combined states, `byGroups`, `using`, and `default` transitions).
- A lexer registry for selecting lexers by language name or filename extension.
- Parity tests for selected lexers against the in-repo Python Pygments source in [pygments-master](pygments-master/).

### Supported lexers (Swift)

Strict parity (Swift tests compare token streams to Python Pygments for chosen samples):
- Swift
- JSON
- JSON-LD

Pragmatic (smoke-test level highlighting for common code):
- Python, JavaScript, TypeScript, Java
- C, C++, C#, Go, Rust
- Kotlin, Ruby, PHP
- Bash/Shell
- Scala
- R

### Build & test

```bash
cd pygments-swift
swift test
```

### Basic usage

Use the registry to pick a lexer by language or filename:

```swift
import PygmentsSwift

let lexer = LexerRegistry.makeLexer(languageName: "swift")!
let tokens = lexer.getTokens("let x = 1")
```

