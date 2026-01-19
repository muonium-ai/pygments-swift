# Feature Parity: Python Pygments → Swift

This document describes what “parity” means for this repo and how we get from today’s Swift implementation to progressively deeper parity with Python Pygments.

## What does “parity” mean?

Pick a target level (so the work is measurable):

1. **Engine parity**
   - Given equivalent lexer rules, the Swift lexer engine produces the same token stream as Python Pygments.
   - Measured as exact match on `(tokentype, value, start)` after Pygments preprocessing.

2. **Lexer parity (subset)**
   - A defined set of lexers (e.g., “top-10 + X”) matches Python token-for-token across a corpus of fixtures.

3. **Project parity**
   - “Pygments in Swift”: broad lexer coverage + options + filters + formatters + discovery behavior.
   - This is a large scope unless we automate most lexer porting.

**Recommended strategy:** Engine parity → Lexer parity (subset) → scale via automation.

## How parity is measured

Parity should always be validated via automated diffs:

- Use Python Pygments as the reference implementation.
- Generate a JSON token stream from Python for a given `(lexer, options, input)`.
- Run Swift for the same `(lexer, options, input)`.
- Diff the token sequences.

Key requirements to keep comparisons meaningful:

- Apply the same preprocessing as Pygments (`_preprocess_lexer_input`) on the Python side.
- Compare token sequences exactly: `type` and `value`.
- Track positions carefully (Swift string indexing differs from Python). Prefer a stable index definition (e.g. Unicode scalar offset) for parity-sensitive tests.

## What we need to reach “full parity”

### 1) Harden the lexer engine first

Before scaling to many lexers, ensure the engine matches Pygments semantics in the corners, otherwise every lexer will accumulate one-off mismatches.

High-impact areas to validate with targeted tests:

- State composition and precedence (`include`, `inherit`, combined states).
- State stack semantics (push/pop/switch; `default(...)` transitions).
- Zero-width matches and loop-safety (must be safe *and* correct).
- Delegation semantics (`using(...)`, `usingThis(...)`).
- Regex flag behavior parity with Python `re` where applicable.
- Unicode + indexing parity (token `start` behavior must be consistent and documented).

### 2) Build a parity corpus runner

Move from “spot-check parity tests” to a repeatable parity pipeline:

- A fixture format: `(lexerName, options, inputPath)` → `expected.json` (or regenerate expected on demand).
- A bulk runner that can:
  - run Python Pygments across all fixtures;
  - run Swift across all fixtures;
  - produce a diff summary grouped by lexer/fixture.

Start corpus sources:

- Upstream Pygments test files and example snippets vendored in `pygments-master/`.
- A curated set of “edge-case” snippets per lexer family (strings, comments, nested states, unicode identifiers, etc.).

### 3) Scale lexer ports via automation (critical)

Python Pygments contains hundreds of lexers; hand-porting is not a path to “full parity”.

Two pragmatic scaling options:

- **Codegen for RegexLexer-based lexers (recommended)**
  - Parse Python lexer `tokens = { ... }` definitions and emit Swift `tokenDefs`.
  - Support include/inherit/combined/byGroups/using/default and any recurring patterns.
  - This is the only practical way to approach broad lexer coverage while keeping Swift-only runtime.

- **Hybrid runtime fallback (optional)**
  - For lexers not yet ported, delegate lexing to Python (embedded Python or WASM/Pyodide).
  - Gives output parity quickly but introduces a runtime dependency and performance/packaging complexity.

### 4) Decide scope beyond lexers

If the product needs formatting/output parity, plan for:

- Filters (e.g., transformations on token streams)
- Formatters (HTML/terminal/RTF/etc.)
- Lexer discovery and aliasing behavior

If the product only needs tokenization, we can defer filters/formatters and still achieve “token parity for N lexers”.

## Suggested milestones

1. **Milestone A: Engine parity confidence**
   - Expand engine-focused tests until differences are rare and explainable.

2. **Milestone B: Subset lexer parity**
   - Pick ~10–20 representative lexers; build fixture corpora; keep `swift test` parity green.

3. **Milestone C: Codegen prototype**
   - Generate Swift lexer definitions for a small set of Pygments RegexLexer lexers.

4. **Milestone D: Broad coverage**
   - Incrementally widen codegen coverage and corpus fixtures; track parity in a matrix.

## Decision points (to confirm)

- What is the parity goal: Engine-only, subset lexers, or “everything”?
- Is a Python runtime dependency acceptable as a temporary or permanent fallback?
- Do we need formatter parity, or only lexing/token parity?
