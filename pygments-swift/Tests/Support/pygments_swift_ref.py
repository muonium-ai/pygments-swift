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
        from pygments.lexers import get_lexer_by_name
        lexer = get_lexer_by_name(lexer_name)
    except Exception as e:
        print(json.dumps({"error": f"Failed to load Pygments lexer ({lexer_name}): {e}"}, ensure_ascii=False))
        return 2

    text = sys.stdin.read()

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
