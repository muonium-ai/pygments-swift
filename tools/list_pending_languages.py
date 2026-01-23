#!/usr/bin/env python3

import argparse
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple


@dataclass(frozen=True)
class PythonLexerInfo:
    name: str
    aliases: Tuple[str, ...]
    filenames: Tuple[str, ...]


def _load_python_pygments_lexers(repo_root: Path) -> List[PythonLexerInfo]:
    vendored = repo_root / "pygments-master"
    if not vendored.exists():
        raise SystemExit(f"Missing vendored pygments-master at {vendored}")

    vendored_str = str(vendored)
    sys.path.insert(0, vendored_str)
    try:
        try:
            from pygments.lexers import get_all_lexers  # type: ignore
        except Exception as e:
            raise SystemExit(f"Failed to import vendored pygments: {e}")

        out: List[PythonLexerInfo] = []
        for (name, aliases, filenames, _mimetypes) in get_all_lexers():
            out.append(
                PythonLexerInfo(
                    name=str(name),
                    aliases=tuple(str(a) for a in aliases),
                    filenames=tuple(str(f) for f in filenames),
                )
            )
        return out
    finally:
        if sys.path and sys.path[0] == vendored_str:
            sys.path.pop(0)


def _extract_swift_builtin_languages(lexer_registry_swift: str) -> Set[str]:
    # Parse `public enum BuiltinLanguage ... { case foo ... }`
    m = re.search(r"public\s+enum\s+BuiltinLanguage\b[^{]*\{", lexer_registry_swift)
    if not m:
        raise SystemExit("Could not find BuiltinLanguage enum")

    start = m.end()
    level = 1
    end = None
    for i in range(start, len(lexer_registry_swift)):
        ch = lexer_registry_swift[i]
        if ch == "{":
            level += 1
        elif ch == "}":
            level -= 1
            if level == 0:
                end = i
                break
    if end is None:
        raise SystemExit("Could not find end of BuiltinLanguage enum")

    block = lexer_registry_swift[start:end]
    names: Set[str] = set()
    for line in block.splitlines():
        line = line.strip()
        if not line.startswith("case "):
            continue
        rest = line[len("case ") :]
        for part in rest.split(","):
            ident = part.strip().split()[0]
            if ident:
                names.add(ident.lower())
    return names


def _extract_swift_language_name_aliases(lexer_registry_swift: str) -> Set[str]:
    # Parse cases like: case "py", "python": return makeLexer(language: .python, ...)
    # We'll collect all string literals that appear after `case` in `makeLexer(languageName:)`.

    # Narrow to the function body if possible.
    fn = re.search(
        r"public\s+static\s+func\s+makeLexer\(\s*languageName\s*:\s*String[^{]*\{",
        lexer_registry_swift,
    )
    if not fn:
        return set()

    start = fn.end()
    level = 1
    end = None
    for i in range(start, len(lexer_registry_swift)):
        ch = lexer_registry_swift[i]
        if ch == "{":
            level += 1
        elif ch == "}":
            level -= 1
            if level == 0:
                end = i
                break
    if end is None:
        return set()

    body = lexer_registry_swift[start:end]

    aliases: Set[str] = set()
    # Match `case "a", "b":` capturing the comma-separated string literals.
    for m in re.finditer(r"\bcase\s+((?:\s*\"[^\"]+\"\s*,?)*)\s*:", body):
        chunk = m.group(1)
        for s in re.finditer(r"\"([^\"]+)\"", chunk):
            aliases.add(s.group(1).strip().lower())
    return aliases


def compute_pending(
    python_lexers: List[PythonLexerInfo],
    swift_supported_names: Set[str],
) -> Tuple[List[PythonLexerInfo], List[PythonLexerInfo]]:
    supported: List[PythonLexerInfo] = []
    pending: List[PythonLexerInfo] = []

    for lx in python_lexers:
        aliases = {a.lower() for a in lx.aliases}
        if aliases & swift_supported_names:
            supported.append(lx)
        else:
            pending.append(lx)

    supported.sort(key=lambda x: x.name.lower())
    pending.sort(key=lambda x: x.name.lower())
    return supported, pending


def main(argv: Sequence[str]) -> int:
    p = argparse.ArgumentParser(
        description=(
            "Compare Swift lexer support vs vendored Python Pygments and list pending languages."
            "\nSupported is determined by matching any Python lexer alias against Swift BuiltinLanguage raw values and makeLexer(languageName:) aliases."
        )
    )
    p.add_argument(
        "--out",
        default="out/language-coverage",
        help="Output directory (default: out/language-coverage)",
    )
    args = p.parse_args(list(argv))

    repo_root = Path(__file__).resolve().parents[1]
    lexer_registry_path = repo_root / "pygments-swift" / "Sources" / "PygmentsSwift" / "LexerRegistry.swift"
    lexer_registry_swift = lexer_registry_path.read_text(encoding="utf-8")

    python_lexers = _load_python_pygments_lexers(repo_root)

    swift_builtins = _extract_swift_builtin_languages(lexer_registry_swift)
    swift_aliases = _extract_swift_language_name_aliases(lexer_registry_swift)
    swift_supported_names = swift_builtins | swift_aliases

    supported, pending = compute_pending(python_lexers, swift_supported_names)

    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)

    (out_dir / "summary.json").write_text(
        json.dumps(
            {
                "python_lexers_total": len(python_lexers),
                "swift_supported_names_count": len(swift_supported_names),
                "python_lexers_supported_by_swift": len(supported),
                "python_lexers_pending": len(pending),
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )

    def dump_list(path: Path, items: List[PythonLexerInfo]) -> None:
        lines = []
        for lx in items:
            alias = lx.aliases[0] if lx.aliases else ""
            lines.append(f"{lx.name}\t{alias}\t{', '.join(lx.filenames[:3])}")
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")

    dump_list(out_dir / "python_supported_by_swift.tsv", supported)
    dump_list(out_dir / "python_pending.tsv", pending)

    print(
        f"Wrote {out_dir}/summary.json, python_supported_by_swift.tsv, python_pending.tsv\n"
        f"Python lexers total: {len(python_lexers)}\n"
        f"Python lexers covered by Swift (alias match): {len(supported)}\n"
        f"Python lexers pending: {len(pending)}"
    )

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
