#!/usr/bin/env python3
"""Render notas_charla_IIFET.md to a print-ready PDF.

    python make_notas_pdf.py

Needs pandoc and xelatex on PATH. Styling lives in notas_pdf_preamble.tex;
this script only handles the two things pandoc cannot: substituting the
emoji (no print font carries them) and dropping the markdown horizontal
rules, which would double up with the rule the preamble draws above every
section heading.

Everything else -- accents, dashes, arrows, Greek, superscripts -- is left
alone: Cambria covers all of it, verified against the xelatex log.
"""

import pathlib
import re
import shutil
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
SRC = HERE / "notas_charla_IIFET.md"
PREAMBLE = HERE / "notas_pdf_preamble.tex"
OUT = HERE / "notas_charla_IIFET.pdf"

TITLE = "Notas de charla — IIFET 2026, Tórshavn"
DATE = ("Sesión: jueves 20 ago, 13:30–15:00 · Spatial Trade-offs of EBM "
        "(Part 2) · hablamos últimos")

# Emoji -> macros defined in the preamble. ⚠ may carry the U+FE0F
# variation selector, so strip that first.
SUBS = [
    ("️", ""),
    ("⭐", r"\hero{}"),
    ("⚠", r"\warn{}"),
]


def main() -> int:
    for tool in ("pandoc", "xelatex"):
        if shutil.which(tool) is None:
            sys.exit(f"{tool} not found on PATH")

    text = SRC.read_text(encoding="utf-8")

    # Drop the H1 -- it is passed as document metadata instead, so pandoc's
    # title block renders it rather than it becoming the first section.
    text = re.sub(r"\A#[^#\n].*\n", "", text)

    for old, new in SUBS:
        text = text.replace(old, new)

    # Thematic breaks only: a bare --- line, never a table delimiter.
    text = re.sub(r"(?m)^-{3,}\s*$", "", text)

    tmp = HERE / ".notas_print.md"
    tmp.write_text(text, encoding="utf-8")

    cmd = [
        "pandoc", str(tmp), "-o", str(OUT),
        "--pdf-engine=xelatex",
        "--from=markdown+raw_tex",
        # One slide per markdown "##" heading; promote so those become
        # \section and drive the running head (\@ssect never sets marks).
        "--shift-heading-level-by=-1",
        "-H", str(PREAMBLE),
        "-M", f"title={TITLE}",
        "-M", f"date={DATE}",
        "-V", "documentclass=article",
        "-V", "fontsize=11pt",
        "-V", "papersize=a4",
        "-V", "geometry:a4paper",
        "-V", "geometry:top=2.2cm,bottom=2.0cm,left=3.0cm,right=3.0cm",
        "-V", "mainfont=Cambria",
        "-V", "sansfont=Segoe UI",
        "-V", "monofont=Consolas",
        "-V", "linestretch=1.06",
        "-V", "colorlinks=true",
        "-V", "linkcolor=udecblue",
        "-V", "urlcolor=udecblue",
        "-V", "lang=es",
    ]
    try:
        subprocess.run(cmd, check=True)
    finally:
        tmp.unlink(missing_ok=True)

    print(f"wrote {OUT}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
