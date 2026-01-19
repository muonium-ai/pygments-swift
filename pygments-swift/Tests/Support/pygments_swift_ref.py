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
        from pygments.lexer import RegexLexer, include, default, combined, bygroups, inherit
        from pygments.token import Text, Name, Keyword, Punctuation, String

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

        if key == "stack_push":
            # Exercise '#push' semantics (push current state).
            # Observable behavior: a single '#pop' should leave us in the pushed state.
            class StackPushLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\{", Punctuation, "inner"),
                        (r"a", Name),
                    ],
                    "inner": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"!", Punctuation, "#push"),
                        (r"\}", Punctuation, "#pop"),
                        (r"a", Keyword),
                    ],
                }

            return StackPushLexer()

        if key == "stack_popn":
            # Exercise '#pop:n' semantics including over-pop behavior.
            class StackPopNLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\{", Punctuation, "a"),
                        (r"a", Name),
                    ],
                    "a": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\[", Punctuation, "b"),
                        (r"a", Keyword),
                    ],
                    "b": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"a", Text),
                        (r"\]", Punctuation, "#pop:2"),
                        (r"!", Punctuation, "#pop:99"),
                    ],
                }

            return StackPopNLexer()

        if key == "combined_precedence":
            # Exercise `combined(...)` precedence when multiple states have overlapping rules.
            # Pygments combines rules in the order of the passed state names.
            class CombinedPrecedenceLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"\{", Punctuation, combined("a", "b")),
                    ],
                    "a": [
                        (r"x", Name),
                    ],
                    "b": [
                        (r"x", Keyword),
                        (r"\}", Punctuation, "#pop"),
                    ],
                }

            return CombinedPrecedenceLexer()

        if key == "inherit_splice":
            # Exercise `inherit` splicing order for RegexLexer subclasses.
            class InheritBaseLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"a", Name),
                    ],
                }

            class InheritSpliceLexer(InheritBaseLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"b", Keyword),
                        inherit,
                        (r"c", String),
                    ],
                }

            return InheritSpliceLexer()

        if key == "bygroups":
            # Exercise byGroups capture range extraction.
            class ByGroupsLexer(RegexLexer):
                tokens = {
                    "root": [
                        (r"\n", Text.Whitespace),
                        (r"[\t\f ]+", Text.Whitespace),
                        (r"(\{)(x)(\})", bygroups(Punctuation, Name, Punctuation)),
                    ],
                }

            return ByGroupsLexer()

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
