# Pygments Port

Vibe coded by Senthil Nayagam (@senthilnayagam) from Muonium AI Venture Studios.

This repo ports the Python **Pygments** syntax highlighting library into other languages.

## Swift port

The Swift implementation lives in [pygments-swift](pygments-swift/) as a SwiftPM package.

### What’s included

- A Pygments-inspired `RegexLexer` engine (state machine with push/pop, include/inherit/combined states, `byGroups`, `using`, and `default` transitions).
- A lexer registry for selecting lexers by language name or filename extension.
- Parity tests for selected lexers against the in-repo Python Pygments source.

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

## OCR-based render regression checks

To catch rasterization/layout regressions in PNG output (for example, missing tail lines), this repo includes an OCR-based checker:

- Script: [tools/ocr_compare_png.py](tools/ocr_compare_png.py)
- Make targets: `make ocr-check` and `make ocr-check-pdf`

### Dependencies (macOS)

- `python3` (system Python is fine; no virtualenv required)
- `tesseract` (installed via Homebrew)
- `sips` (built into macOS; required only for the PDF baseline mode)

### Install Tesseract (Homebrew)

```bash
brew install tesseract
```

Verify:

```bash
which tesseract
tesseract --version
```

### Verify `sips` is available

`sips` is included with macOS:

```bash
which sips
sips --version
```

### Run the checks

Generate sample renders first:

```bash
make render-samples-all CONFIG=release
```

Run PNG OCR checks (fails if any PNG is flagged `CROPPED?`):

```bash
make ocr-check
```

Optional: also OCR-check the matching PDFs via `sips` as a baseline (this may show false positives depending on OCR noise/theme contrast):

```bash
make ocr-check-pdf
```

To make the PDF baseline strict:

```bash
make ocr-check-pdf FAIL=1
```

To print source/OCR tails for flagged cases:

```bash
make ocr-check SHOW_TAILS=1
make ocr-check-pdf SHOW_TAILS=1
```

