#!/usr/bin/env python3

import json
import os
import sys


def main() -> int:
    # Ensure we load the in-repo pygments, not whatever is installed.
    pygments_master = os.environ.get("PYGMENTS_MASTER")
    if pygments_master:
        sys.path.insert(0, pygments_master)

    try:
        from pygments.lexers.objective import SwiftLexer
    except Exception as e:
        print(json.dumps({"error": f"Failed to import Pygments SwiftLexer: {e}"}))
        return 2

    text = sys.stdin.read()
    lexer = SwiftLexer()

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
