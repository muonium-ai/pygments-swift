# PygmentsSwift (work in progress)

A Swift port (starting minimal) of the Python [Pygments](https://pygments.org/) lexer engine.

## Status

- Core token model: in progress
- Regex/state-machine lexer: minimal implementation
- Example lexer: INI

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
