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

    def make_custom_lexer(key: str):
        from pygments.lexer import RegexLexer, include, default
        from pygments.token import Text, Name, Keyword, Punctuation

        if key == "include_precedence":
            class IncludePrecedenceLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        include("inc"),
                        (r"x", Keyword),
                    ],
                    "inc": [
                        (r"x", Name),
                    ],
                }

            return IncludePrecedenceLexer()

        if key == "default_pop":
            class DefaultPopLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\{", Punctuation, "inner"),
                        (r"[a-zA-Z_]+", Name),
                    ],
                    "inner": [
                        default("#pop"),
                    ],
                }

            return DefaultPopLexer()

        if key == "nomatch_newline_reset":
            # Intentionally do NOT match \n in inner; Pygments' RegexLexer will
            # treat unmatched newlines specially (emit Whitespace and reset to root).
            class NoMatchNewlineResetLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\{", Punctuation, "inner"),
                        (r"a", Name),
                    ],
                    "inner": [
                        (r"a", Keyword),
                    ],
                }

            return NoMatchNewlineResetLexer()

        raise ValueError(f"unknown custom lexer: {key}")

    try:
        if lexer_name.startswith("custom:"):
            lexer = make_custom_lexer(lexer_name.split(":", 1)[1])
        else:
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
