# PygmentsSwift (work in progress)

A Swift port (starting minimal) of the Python [Pygments](https://pygments.org/) lexer engine.

## Status

- Core token model: in progress
- Regex/state-machine lexer: minimal implementation
- Example lexer: INI

## Supported lexers

Strict parity (Swift tests compare token streams to Python Pygments for chosen samples):
- Swift
- JSON
- JSON-LD

Pragmatic (smoke-test level highlighting for common code):
- Python, JavaScript, TypeScript, Java
- C, C++, C#, Go, Rust
- D, Crystal
- Kotlin, Ruby, PHP
- Bash/Shell
- Tcl, Awk, Sed
- Windows Batchfile, VBScript
- Pascal
- Racket, Scheme, Common Lisp, Emacs Lisp
- Elm, Haxe, V
- Nix
- Fish, Nushell
- Raku
- CUE
- Scala
- R
- reStructuredText, LaTeX
- GitIgnore, EditorConfig, Properties, CSV
- Graphviz (DOT), PlantUML, Mermaid
- ApacheConf
- Ada, COBOL, Prolog, Smalltalk, Eiffel, Visual Basic
- SystemVerilog, VHDL, LLVM IR, GLSL
- JSON5, Jsonnet, YARA, YANG, WGSL, WebAssembly (WAT), WebIDL, Meson, GDScript, Teal
- AsciiDoc, Org-mode, Kconfig, Caddyfile, SPARQL, Turtle, Thrift, Cap'n Proto, QML, HLSL

## Quick start

```swift
import PygmentsSwift

let lexer = IniLexer()
let tokens = lexer.getTokens("""
[section]
key = value
""")

for t in tokens {
    print("\(t.start)\t\(t.type)\t\(t.value.debugDescription)")
}
```

## Run tests

From this folder:

- `swift test`
