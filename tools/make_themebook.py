#!/usr/bin/env python3

import argparse
import io
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Tuple
import json
import subprocess


def _escape_pdf_text(text: str) -> str:
    # Minimal escaping for PDF literal strings.
    return (
        text.replace("\\", "\\\\")
        .replace("(", "\\(")
        .replace(")", "\\)")
        .replace("\r", " ")
        .replace("\n", " ")
    )


def _make_label_pdf(text: str, width: float, height: float) -> bytes:
    """Generate a single-page PDF containing a label string.

    We do this without external PDF-generation deps. The output is a very small
    PDF using Helvetica and a single content stream.
    """

    # Clamp to something sane if mediabox is missing/weird.
    if width <= 0 or height <= 0:
        width, height = 612.0, 792.0  # US Letter

    text = _escape_pdf_text(text)

    # Place label near top-left with margins.
    font_size = 20
    margin_x = 48
    margin_y = 56
    y = max(margin_y, height - margin_y)

    # Default label colors (will be overridden if theme colors are provided).
    bg = (1.0, 1.0, 1.0)
    fg = (0.0, 0.0, 0.0)

    contents = (
        "q\n"
        f"{bg[0]:.4f} {bg[1]:.4f} {bg[2]:.4f} rg\n"
        f"0 0 {width:.2f} {height:.2f} re f\n"
        "BT\n"
        f"/F1 {font_size} Tf\n"
        f"{fg[0]:.4f} {fg[1]:.4f} {fg[2]:.4f} rg\n"
        f"{margin_x} {y} Td\n"
        f"({text}) Tj\n"
        "ET\n"
        "Q\n"
    )
    contents_bytes = contents.encode("utf-8")

    objects: List[bytes] = []

    # 1: Catalog
    objects.append(b"<< /Type /Catalog /Pages 2 0 R >>")
    # 2: Pages
    objects.append(b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    # 3: Page
    objects.append(
        (
            "<< /Type /Page /Parent 2 0 R "
            f"/MediaBox [0 0 {width:.2f} {height:.2f}] "
            "/Resources << /Font << /F1 4 0 R >> >> "
            "/Contents 5 0 R >>"
        ).encode("utf-8")
    )
    # 4: Font
    objects.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    # 5: Contents
    objects.append(
        b"<< /Length "
        + str(len(contents_bytes)).encode("ascii")
        + b" >>\nstream\n"
        + contents_bytes
        + b"endstream"
    )

    out = io.BytesIO()
    out.write(b"%PDF-1.4\n")

    offsets: List[int] = [0]  # xref requires object 0

    for i, obj in enumerate(objects, start=1):
        offsets.append(out.tell())
        out.write(f"{i} 0 obj\n".encode("ascii"))
        out.write(obj)
        out.write(b"\nendobj\n")

    xref_pos = out.tell()
    out.write(b"xref\n")
    out.write(f"0 {len(offsets)}\n".encode("ascii"))
    out.write(b"0000000000 65535 f \n")
    for off in offsets[1:]:
        out.write(f"{off:010d} 00000 n \n".encode("ascii"))

    out.write(b"trailer\n")
    out.write(f"<< /Size {len(offsets)} /Root 1 0 R >>\n".encode("ascii"))
    out.write(b"startxref\n")
    out.write(f"{xref_pos}\n".encode("ascii"))
    out.write(b"%%EOF\n")

    return out.getvalue()


def _hex_to_rgb01(hex_rgba: str) -> Tuple[float, float, float]:
    s = hex_rgba.strip()
    if s.startswith("#"):
        s = s[1:]
    if len(s) not in (6, 8):
        raise ValueError(f"Unexpected color hex: {hex_rgba}")
    r = int(s[0:2], 16) / 255.0
    g = int(s[2:4], 16) / 255.0
    b = int(s[4:6], 16) / 255.0
    return (r, g, b)


def _find_codeviewer(repo_root: Path) -> Optional[Path]:
    candidates = [
        repo_root / "codeviewer" / ".build" / "debug" / "codeviewer",
        repo_root / "codeviewer" / ".build" / "release" / "codeviewer",
    ]
    for p in candidates:
        if p.exists() and p.is_file():
            return p
    return None


def _get_theme_colors(
    *,
    repo_root: Path,
    codeviewer_bin: Optional[Path],
    theme: Optional[str],
    theme_file: Optional[Path],
) -> Optional[Tuple[Tuple[float, float, float], Tuple[float, float, float]]]:
    """Return (bg_rgb01, fg_rgb01) or None."""

    cv = codeviewer_bin or _find_codeviewer(repo_root)
    if not cv:
        return None

    cmd = [str(cv), "--print-theme-colors"]
    if theme_file is not None:
        cmd += ["--theme-file", str(theme_file)]
    elif theme is not None:
        cmd += ["--theme", theme]

    try:
        out = subprocess.check_output(cmd, stderr=subprocess.DEVNULL)
        data = json.loads(out.decode("utf-8"))
        bg = _hex_to_rgb01(data["background"])
        fg = _hex_to_rgb01(data["foreground"])
        return (bg, fg)
    except Exception:
        return None


def _iter_pdfs(theme_dir: Path) -> List[Path]:
    pdfs = sorted(p for p in theme_dir.glob("*.pdf") if p.is_file())
    # Filter out any already-generated themebook PDFs if someone points at that folder.
    return [p for p in pdfs if not p.name.endswith(".themebook.pdf")]


def _try_import_pygments(repo_root: Path):
    """Import pygments, preferring the vendored pygments-master in this repo."""
    try:
        import pygments  # noqa: F401

        return True
    except Exception:
        vendored = repo_root / "pygments-master"
        if vendored.exists():
            sys.path.insert(0, str(vendored))
            try:
                import pygments  # noqa: F401

                return True
            except Exception:
                return False
        return False


def _guess_language_name(rendered_stem: str, samples_dir: Path, repo_root: Path) -> Optional[str]:
    if not _try_import_pygments(repo_root):
        return None

    try:
        from pygments.lexers import get_lexer_for_filename
    except Exception:
        return None

    sample_path = samples_dir / rendered_stem
    code: Optional[str] = None
    if sample_path.exists() and sample_path.is_file():
        try:
            code = sample_path.read_text(encoding="utf-8", errors="replace")
        except Exception:
            code = None

    try:
        lexer = get_lexer_for_filename(rendered_stem, code=code or "")
        name = getattr(lexer, "name", None)
        return str(name) if name else None
    except Exception:
        return None


def _require_pypdf():
    try:
        import pypdf  # noqa: F401

        return
    except Exception:
        print(
            "Missing dependency: pypdf\n"
            "Install with: python3 -m pip install --user pypdf\n",
            file=sys.stderr,
        )
        raise SystemExit(2)


def _label_for_pdf(pdf_path: Path, mode: str) -> str:
    # Rendered PDFs are typically named like "fibonacci.py.pdf".
    rendered_stem = pdf_path.stem  # "fibonacci.py"
    original_suffix = Path(rendered_stem).suffix  # ".py" (or "" if none)

    if mode == "filename":
        # Includes original extension when present (e.g. "fibonacci.py").
        return rendered_stem
    if mode == "filename-language":
        # Special-cased by build_themebook where we have samples_dir/repo_root.
        return rendered_stem
    if mode == "original-ext":
        return original_suffix.lstrip(".") if original_suffix else "(no ext)"
    if mode == "filename+original-ext":
        # Redundant for the default naming scheme, but kept for explicitness.
        if original_suffix and not rendered_stem.endswith(original_suffix):
            return f"{rendered_stem}{original_suffix}"
        return rendered_stem

    raise ValueError(f"Unknown label mode: {mode}")


def build_themebook(
    theme_dir: Path,
    out_path: Path,
    label_mode: str,
    samples_dir: Path,
    theme: Optional[str],
    theme_file: Optional[Path],
    codeviewer_bin: Optional[Path],
) -> Tuple[int, int]:
    _require_pypdf()
    from pypdf import PdfReader, PdfWriter

    pdfs = _iter_pdfs(theme_dir)
    if not pdfs:
        print(f"No PDFs found in: {theme_dir}", file=sys.stderr)
        return (0, 0)

    out_path.parent.mkdir(parents=True, exist_ok=True)

    writer = PdfWriter()

    repo_root = Path(__file__).resolve().parents[1]
    theme_colors = _get_theme_colors(
        repo_root=repo_root,
        codeviewer_bin=codeviewer_bin,
        theme=theme,
        theme_file=theme_file,
    )

    for pdf_path in pdfs:
        reader = PdfReader(str(pdf_path))
        if not reader.pages:
            continue

        first = reader.pages[0]
        width = float(first.mediabox.width)
        height = float(first.mediabox.height)

        label = _label_for_pdf(pdf_path, label_mode)
        if label_mode == "filename-language":
            lang = _guess_language_name(label, samples_dir=samples_dir, repo_root=repo_root)
            if lang:
                label = f"{label} — {lang}"
        label_pdf = _make_label_pdf(label, width, height)
        if theme_colors is not None:
            # Regenerate with theme-matched colors.
            bg, fg = theme_colors
            label_pdf = _make_label_pdf_with_colors(label, width, height, bg=bg, fg=fg)
        label_reader = PdfReader(io.BytesIO(label_pdf))

        writer.add_page(label_reader.pages[0])
        for page in reader.pages:
            writer.add_page(page)

    with out_path.open("wb") as f:
        writer.write(f)

    return (len(pdfs), len(writer.pages))


def _make_label_pdf_with_colors(
    text: str,
    width: float,
    height: float,
    *,
    bg: Tuple[float, float, float],
    fg: Tuple[float, float, float],
) -> bytes:
    # Same as _make_label_pdf, but with specified bg/fg.
    if width <= 0 or height <= 0:
        width, height = 612.0, 792.0

    text = _escape_pdf_text(text)
    font_size = 20
    margin_x = 48
    margin_y = 56
    y = max(margin_y, height - margin_y)

    contents = (
        "q\n"
        f"{bg[0]:.4f} {bg[1]:.4f} {bg[2]:.4f} rg\n"
        f"0 0 {width:.2f} {height:.2f} re f\n"
        "BT\n"
        f"/F1 {font_size} Tf\n"
        f"{fg[0]:.4f} {fg[1]:.4f} {fg[2]:.4f} rg\n"
        f"{margin_x} {y} Td\n"
        f"({text}) Tj\n"
        "ET\n"
        "Q\n"
    )
    contents_bytes = contents.encode("utf-8")

    objects: List[bytes] = []
    objects.append(b"<< /Type /Catalog /Pages 2 0 R >>")
    objects.append(b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    objects.append(
        (
            "<< /Type /Page /Parent 2 0 R "
            f"/MediaBox [0 0 {width:.2f} {height:.2f}] "
            "/Resources << /Font << /F1 4 0 R >> >> "
            "/Contents 5 0 R >>"
        ).encode("utf-8")
    )
    objects.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    objects.append(
        b"<< /Length "
        + str(len(contents_bytes)).encode("ascii")
        + b" >>\nstream\n"
        + contents_bytes
        + b"endstream"
    )

    out = io.BytesIO()
    out.write(b"%PDF-1.4\n")
    offsets: List[int] = [0]
    for i, obj in enumerate(objects, start=1):
        offsets.append(out.tell())
        out.write(f"{i} 0 obj\n".encode("ascii"))
        out.write(obj)
        out.write(b"\nendobj\n")
    xref_pos = out.tell()
    out.write(b"xref\n")
    out.write(f"0 {len(offsets)}\n".encode("ascii"))
    out.write(b"0000000000 65535 f \n")
    for off in offsets[1:]:
        out.write(f"{off:010d} 00000 n \n".encode("ascii"))
    out.write(b"trailer\n")
    out.write(f"<< /Size {len(offsets)} /Root 1 0 R >>\n".encode("ascii"))
    out.write(b"startxref\n")
    out.write(f"{xref_pos}\n".encode("ascii"))
    out.write(b"%%EOF\n")
    return out.getvalue()


def main(argv: Sequence[str]) -> int:
    p = argparse.ArgumentParser(
        description=(
            "Create a themebook PDF by concatenating rendered sample PDFs for a theme, "
            "inserting a one-page label before each sample." 
        )
    )
    p.add_argument(
        "--dir",
        required=True,
        help="Theme output directory (e.g. out/samples/github-dark)",
    )
    p.add_argument(
        "--theme",
        default=None,
        help="Theme name (built-in themes). Used to match label colors.",
    )
    p.add_argument(
        "--theme-file",
        default=None,
        help="Theme JSON file path (overrides --theme). Used to match label colors.",
    )
    p.add_argument(
        "--codeviewer",
        default=None,
        help="Path to codeviewer binary (optional; auto-detected if omitted).",
    )
    p.add_argument(
        "--out",
        required=True,
        help="Output PDF path (e.g. out/themebooks/github-dark.pdf)",
    )
    p.add_argument(
        "--label",
        choices=["filename", "filename-language", "original-ext", "filename+original-ext"],
        default="filename",
        help="What to put on the inserted label page.",
    )
    p.add_argument(
        "--samples",
        default=None,
        help="Directory containing original sample inputs (default: <repo>/sample-code)",
    )

    args = p.parse_args(list(argv))

    theme_dir = Path(args.dir)
    out_path = Path(args.out)

    repo_root = Path(__file__).resolve().parents[1]
    samples_dir = Path(args.samples) if args.samples else (repo_root / "sample-code")

    theme_file = Path(args.theme_file) if args.theme_file else None
    codeviewer_bin = Path(args.codeviewer) if args.codeviewer else None

    pdf_count, page_count = build_themebook(
        theme_dir,
        out_path,
        args.label,
        samples_dir,
        theme=args.theme,
        theme_file=theme_file,
        codeviewer_bin=codeviewer_bin,
    )
    print(f"Wrote {out_path} (inputs={pdf_count}, pages={page_count})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
