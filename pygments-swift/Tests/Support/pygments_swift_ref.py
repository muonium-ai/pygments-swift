#!/usr/bin/env python3

import json
import os
import sys


def main() -> int:
    # Ensure we load the in-repo pygments, not whatever is installed.
    pygments_master = os.environ.get("PYGMENTS_MASTER")
    if pygments_master:
        sys.path.insert(0, pygments_master)

    lexer_name = (os.environ.get("PYGMENTS_LEXER") or "swift").strip().lower()

    try:
        if lexer_name in {"swift"}:
            from pygments.lexers.objective import SwiftLexer as SelectedLexer
        elif lexer_name in {"json", "json-object", "jsonlexer"}:
            from pygments.lexers.data import JsonLexer as SelectedLexer
        elif lexer_name in {"jsonld", "json-ld"}:
            from pygments.lexers.data import JsonLdLexer as SelectedLexer
        else:
            print(json.dumps({"error": f"Unsupported Pygments lexer: {lexer_name}"}, ensure_ascii=False))
            return 2
    except Exception as e:
        print(json.dumps({"error": f"Failed to import Pygments lexer ({lexer_name}): {e}"}, ensure_ascii=False))
        return 2

    text = sys.stdin.read()
    lexer = SelectedLexer()

    # Match Pygments' normal path (`Lexer.get_tokens()`), which preprocesses
    # the input (newline normalization, ensure trailing newline, etc.).
    text = lexer._preprocess_lexer_input(text)

    out = []
    for i, ttype, value in lexer.get_tokens_unprocessed(text):
        out.append({
            "start": int(i),
            "type": repr(ttype),
            "value": value,
        })

    sys.stdout.write(json.dumps(out, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
