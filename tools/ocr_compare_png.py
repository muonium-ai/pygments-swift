#!/usr/bin/env python3
"""OCR-compare codeviewer PNG renders against their source files.

This script is designed for macOS/system Python usage (no venv, no extra deps).
It uses Tesseract (must be installed) to OCR PNG files and then fuzzy-matches
OCR lines against the corresponding source file lines.

Typical usage:
  python3 tools/ocr_compare_png.py --theme tokyo-night
  python3 tools/ocr_compare_png.py --dir out/samples/tokyo-night
  python3 tools/ocr_compare_png.py --png out/samples/tokyo-night/fibonacci.go.png

Exit code:
  0 = ran successfully
  2 = tesseract not found / missing inputs
"""

from __future__ import annotations

import argparse
import difflib
import os
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple
import tempfile


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SAMPLES_DIR = REPO_ROOT / "sample-code"
DEFAULT_OUT_SAMPLES_DIR = REPO_ROOT / "out" / "samples"


@dataclass(frozen=True)
class CompareResult:
    png_path: Path
    src_path: Optional[Path]
    src_line_count: int
    ocr_nonempty_line_count: int
    matched_lines: int
    unmatched_lines: int
    unmatched_tail_lines: int
    match_ratio: float
    cropped_likely: bool
    ocr_tail: List[str]


def _norm_line(s: str) -> str:
    s = s.strip().lower()
    # Normalize common OCR punctuation variants.
    s = s.replace("“", '"').replace("”", '"').replace("’", "'")
    s = s.replace("`", "'")
    # Collapse whitespace.
    s = re.sub(r"\s+", " ", s)
    return s


def _nonempty_lines(text: str) -> List[str]:
    return [ln for ln in text.splitlines() if ln.strip()]


def _tesseract_path(explicit: Optional[str]) -> Optional[str]:
    if explicit:
        return explicit
    return shutil.which("tesseract")


def _sips_path(explicit: Optional[str]) -> Optional[str]:
    if explicit:
        return explicit
    return shutil.which("sips")


def ocr_png(png_path: Path, tesseract: str, lang: str = "eng", psm: str = "6") -> str:
    # stdout output avoids generating temp files.
    proc = subprocess.run(
        [tesseract, str(png_path), "stdout", "-l", lang, "--psm", psm],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or f"tesseract failed for {png_path}")
    return proc.stdout


def ocr_pdf_via_sips(pdf_path: Path, *, tesseract: str, sips: str, lang: str = "eng", psm: str = "6") -> str:
    # Rasterize the (single-page) PDF to a temporary PNG and OCR it.
    tmp_dir = Path(tempfile.mkdtemp(prefix="ocr-compare-"))
    try:
        out_png = tmp_dir / (pdf_path.stem + ".png")

        # sips reads PDF and can write a PNG; for multi-page PDFs it typically uses the first page.
        proc = subprocess.run(
            [sips, "-s", "format", "png", str(pdf_path), "--out", str(out_png)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if proc.returncode != 0 or not out_png.exists():
            err = (proc.stderr or proc.stdout).strip()
            raise RuntimeError(err or f"sips failed for {pdf_path}")

        return ocr_png(out_png, tesseract=tesseract, lang=lang, psm=psm)
    finally:
        try:
            for p in tmp_dir.glob("*"):
                p.unlink(missing_ok=True)
            tmp_dir.rmdir()
        except Exception:
            pass


def fuzzy_compare_source_to_ocr(
    src_lines: List[str],
    ocr_lines: List[str],
    *,
    per_line_threshold: float = 0.72,
    early_break_threshold: float = 0.82,
    tail_check_lines: int = 6,
) -> Tuple[int, int, int]:
    """Return (matched_lines, unmatched_lines, unmatched_tail_lines)."""

    ocr_norm = [_norm_line(x) for x in ocr_lines]

    matched = 0
    unmatched_indices: List[int] = []

    for i, src in enumerate(src_lines, start=1):
        n = _norm_line(src)
        if not n:
            matched += 1
            continue

        best = 0.0
        for o in ocr_norm:
            if not o:
                continue
            r = difflib.SequenceMatcher(None, n, o).ratio()
            if r > best:
                best = r
                if best >= early_break_threshold:
                    break

        if best >= per_line_threshold:
            matched += 1
        else:
            unmatched_indices.append(i)

    unmatched = len(unmatched_indices)
    tail_start = max(1, len(src_lines) - tail_check_lines + 1)
    unmatched_tail = sum(1 for idx in unmatched_indices if idx >= tail_start)

    return matched, unmatched, unmatched_tail


def guess_source_for_png(png_path: Path, samples_dir: Path) -> Optional[Path]:
    # Map: out/samples/<theme>/name.ext.png -> sample-code/name.ext
    name = png_path.name
    if not name.endswith(".png"):
        return None
    base = name[: -len(".png")]
    src = samples_dir / base
    if src.exists() and src.is_file():
        return src
    return None


def guess_source_for_pdf(pdf_path: Path, samples_dir: Path) -> Optional[Path]:
    # Map: out/samples/<theme>/name.ext.pdf -> sample-code/name.ext
    name = pdf_path.name
    if not name.endswith(".pdf"):
        return None
    base = name[: -len(".pdf")]
    src = samples_dir / base
    if src.exists() and src.is_file():
        return src
    return None


def iter_pngs(dir_path: Path) -> Iterable[Path]:
    yield from sorted(p for p in dir_path.glob("*.png") if p.is_file())


def iter_pdfs(dir_path: Path) -> Iterable[Path]:
    yield from sorted(p for p in dir_path.glob("*.pdf") if p.is_file())


def compare_one(png_path: Path, *, samples_dir: Path, tesseract: str) -> CompareResult:
    src_path = guess_source_for_png(png_path, samples_dir)
    src_lines: List[str] = []
    if src_path is not None:
        try:
            src_lines = src_path.read_text("utf-8").splitlines()
        except Exception:
            src_lines = []
            src_path = None

    ocr_text = ocr_png(png_path, tesseract=tesseract)
    ocr_lines = _nonempty_lines(ocr_text)

    matched, unmatched, unmatched_tail = fuzzy_compare_source_to_ocr(src_lines, ocr_lines)

    src_count = len(src_lines)
    ratio = (matched / src_count) if src_count else 0.0

    # Heuristic: If we fail to match most of the tail, it's likely bottom-cropped.
    # (OCR can be noisy; the tail test is the strongest cropping signal.)
    cropped_likely = False
    if src_count >= 8:
        # If >= half of the last 6 lines are unmatched, call it cropped-likely.
        cropped_likely = unmatched_tail >= 3

    return CompareResult(
        png_path=png_path,
        src_path=src_path,
        src_line_count=src_count,
        ocr_nonempty_line_count=len(ocr_lines),
        matched_lines=matched,
        unmatched_lines=unmatched,
        unmatched_tail_lines=unmatched_tail,
        match_ratio=ratio,
        cropped_likely=cropped_likely,
        ocr_tail=ocr_lines[-20:],
    )


def compare_one_pdf(pdf_path: Path, *, samples_dir: Path, tesseract: str, sips: str) -> CompareResult:
    src_path = guess_source_for_pdf(pdf_path, samples_dir)
    src_lines: List[str] = []
    if src_path is not None:
        try:
            src_lines = src_path.read_text("utf-8").splitlines()
        except Exception:
            src_lines = []
            src_path = None

    ocr_text = ocr_pdf_via_sips(pdf_path, tesseract=tesseract, sips=sips)
    ocr_lines = _nonempty_lines(ocr_text)

    matched, unmatched, unmatched_tail = fuzzy_compare_source_to_ocr(src_lines, ocr_lines)

    src_count = len(src_lines)
    ratio = (matched / src_count) if src_count else 0.0
    cropped_likely = False
    if src_count >= 8:
        cropped_likely = unmatched_tail >= 3

    return CompareResult(
        png_path=pdf_path,
        src_path=src_path,
        src_line_count=src_count,
        ocr_nonempty_line_count=len(ocr_lines),
        matched_lines=matched,
        unmatched_lines=unmatched,
        unmatched_tail_lines=unmatched_tail,
        match_ratio=ratio,
        cropped_likely=cropped_likely,
        ocr_tail=ocr_lines[-20:],
    )


def print_result(r: CompareResult) -> None:
    if r.src_path:
        try:
            src_display = str(r.src_path.relative_to(REPO_ROOT))
        except ValueError:
            src_display = str(r.src_path)
    else:
        src_display = "(no source match)"

    try:
        png_display = str(r.png_path.relative_to(REPO_ROOT))
    except ValueError:
        png_display = str(r.png_path)
    status = "CROPPED?" if r.cropped_likely else "ok"
    print(
        f"{status:8}  {png_display}  |  src={src_display}  "
        f"|  src_lines={r.src_line_count}  ocr_lines={r.ocr_nonempty_line_count}  "
        f"matched={r.matched_lines}  unmatched_tail={r.unmatched_tail_lines}  "
        f"ratio={r.match_ratio:.0%}"
    )


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description="OCR-compare codeviewer PNG renders vs source")
    ap.add_argument("--theme", help="Theme name under out/samples/<theme>")
    ap.add_argument("--dir", help="Directory containing .png files (e.g. out/samples/tokyo-night)")
    ap.add_argument("--png", action="append", help="Specific PNG file(s) to check", default=[])
    ap.add_argument("--pdf", action="append", help="Specific PDF file(s) to check", default=[])
    ap.add_argument("--samples", default=str(DEFAULT_SAMPLES_DIR), help="sample-code directory")
    ap.add_argument("--tesseract", default=None, help="Path to tesseract binary (optional)")
    ap.add_argument("--sips", default=None, help="Path to sips binary (optional, needed for PDF OCR)")
    ap.add_argument("--limit", type=int, default=0, help="Limit number of PNGs processed (0 = no limit)")
    ap.add_argument("--only-cropped", action="store_true", help="Only print results flagged as CROPPED?")
    ap.add_argument(
        "--show-tails",
        action="store_true",
        help="For CROPPED? results, print the last few source lines and OCR tail (helps diagnose how much was lost)",
    )
    ap.add_argument(
        "--also-pdf",
        action="store_true",
        help="If a matching .pdf exists for each selected .png, also OCR-check the PDF.",
    )
    args = ap.parse_args(argv)

    start_time = time.perf_counter()

    tesseract = _tesseract_path(args.tesseract)
    if not tesseract:
        print("Error: tesseract not found on PATH (or via --tesseract)", file=sys.stderr)
        return 2

    sips = _sips_path(args.sips)

    samples_dir = Path(args.samples).expanduser().resolve()
    if not samples_dir.exists():
        print(f"Error: samples dir not found: {samples_dir}", file=sys.stderr)
        return 2

    pngs: List[Path] = []
    pdfs: List[Path] = []

    if args.png:
        for p in args.png:
            pp = Path(p).expanduser().resolve()
            if not pp.exists():
                print(f"Error: PNG not found: {pp}", file=sys.stderr)
                return 2
            pngs.append(pp)

    if args.pdf:
        for p in args.pdf:
            pp = Path(p).expanduser().resolve()
            if not pp.exists():
                print(f"Error: PDF not found: {pp}", file=sys.stderr)
                return 2
            pdfs.append(pp)

    if args.theme and not args.dir:
        args.dir = str((DEFAULT_OUT_SAMPLES_DIR / args.theme).resolve())

    if args.dir:
        d = Path(args.dir).expanduser().resolve()
        if not d.exists():
            print(f"Error: directory not found: {d}", file=sys.stderr)
            return 2
        pngs.extend(iter_pngs(d))

    # De-dupe
    seen = set()
    uniq: List[Path] = []
    for p in pngs:
        if p in seen:
            continue
        seen.add(p)
        uniq.append(p)
    pngs = uniq

    seen_pdf = set()
    uniq_pdf: List[Path] = []
    for p in pdfs:
        if p in seen_pdf:
            continue
        seen_pdf.add(p)
        uniq_pdf.append(p)
    pdfs = uniq_pdf

    if not pngs and not pdfs:
        print("Error: no inputs selected. Use --theme, --dir, --png, or --pdf.", file=sys.stderr)
        return 2

    if args.limit and args.limit > 0:
        if pngs:
            pngs = pngs[: args.limit]
        if pdfs:
            pdfs = pdfs[: args.limit]

    print(f"tesseract: {tesseract}")
    if args.pdf or args.also_pdf:
        if not sips:
            print("Error: sips not found on PATH (or via --sips); required for PDF OCR", file=sys.stderr)
            return 2
        print(f"sips:      {sips}")
    print(f"samples:   {samples_dir}")
    print(f"png count:  {len(pngs)}")
    print(f"pdf count:  {len(pdfs)}")
    print("")

    total_inputs = len(pngs) + len(pdfs)
    label = args.theme or (Path(args.dir).name if args.dir else None) or "(selection)"

    def _progress(processed_now: int) -> None:
        # Print progress to stderr so it remains visible even when stdout is captured by Make.
        # Keep this quiet for small runs.
        if total_inputs < 50:
            return
        elapsed = time.perf_counter() - start_time
        print(f"progress: {label}  {processed_now}/{total_inputs}  elapsed={elapsed:.1f}s", file=sys.stderr)

    cropped = 0
    processed = 0
    printed = 0

    next_progress_at = 25

    for p in pngs:
        r = compare_one(p, samples_dir=samples_dir, tesseract=tesseract)
        processed += 1

        if processed >= next_progress_at:
            _progress(processed)
            next_progress_at += 25

        if r.cropped_likely:
            cropped += 1

        should_print = (not args.only_cropped) or r.cropped_likely
        if should_print:
            print_result(r)
            printed += 1
            if r.cropped_likely and args.show_tails and r.src_path:
                try:
                    src_lines = r.src_path.read_text("utf-8").splitlines()
                except Exception:
                    src_lines = []

                print("  Source tail:")
                for ln in src_lines[-10:]:
                    print(f"    {ln}")
                print("  OCR tail:")
                for ln in r.ocr_tail:
                    print(f"    {ln}")

        if args.also_pdf:
            pdf_path = p.with_suffix(".pdf")
            if pdf_path.exists():
                rr = compare_one_pdf(pdf_path, samples_dir=samples_dir, tesseract=tesseract, sips=sips)
                processed += 1

                if processed >= next_progress_at:
                    _progress(processed)
                    next_progress_at += 25
                if rr.cropped_likely:
                    cropped += 1

                should_print_pdf = (not args.only_cropped) or rr.cropped_likely
                if should_print_pdf:
                    print_result(rr)
                    printed += 1

    for p in pdfs:
        rr = compare_one_pdf(p, samples_dir=samples_dir, tesseract=tesseract, sips=sips)
        processed += 1

        if processed >= next_progress_at:
            _progress(processed)
            next_progress_at += 25

        if rr.cropped_likely:
            cropped += 1

        should_print = (not args.only_cropped) or rr.cropped_likely
        if should_print:
            print_result(rr)
            printed += 1
            if rr.cropped_likely and args.show_tails and rr.src_path:
                try:
                    src_lines = rr.src_path.read_text("utf-8").splitlines()
                except Exception:
                    src_lines = []
                print("  Source tail:")
                for ln in src_lines[-10:]:
                    print(f"    {ln}")
                print("  OCR tail:")
                for ln in rr.ocr_tail:
                    print(f"    {ln}")

    print("")
    print(f"Summary: cropped_likely={cropped}/{processed} (printed={printed})")

    elapsed = time.perf_counter() - start_time
    print(f"Time: {elapsed:.2f}s")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
