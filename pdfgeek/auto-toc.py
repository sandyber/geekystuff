"""
Auto-generate a flat TOC for a PDF from one sample chapter heading.

Wraps the pdf.tocgen toolchain (pdfxmeta + pdftocgen + pdftocio) so you
only have to type one example. The font/size of that example is used as
a pattern; every other heading in the same style becomes a bookmark.

Usage:
    python auto_toc.py book.pdf "Chapter 1" 15
    python auto_toc.py book.pdf "Chapter 1" 15 --keep-intermediate
    python auto_toc.py book.pdf "Chapter 1" 15 --out mybook.pdf

Requires (on PATH):
    pdfxmeta, pdftocgen, pdftocio   (pip install pdf.tocgen)
"""

import os
import shutil
import subprocess
import sys


def need(binary):
    if not shutil.which(binary):
        print(f"[!] '{binary}' not found on PATH. Install with: pip install pdf.tocgen")
        sys.exit(1)


def run(cmd, stdout_path=None):
    """Run a command. If stdout_path is given, redirect stdout to that file.
    Returns True on success, False on failure (and prints stderr)."""
    try:
        if stdout_path:
            with open(stdout_path, "wb") as f:
                subprocess.run(cmd, check=True, stdout=f, stderr=subprocess.PIPE)
        else:
            subprocess.run(cmd, check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError as e:
        err = e.stderr.decode(errors="replace") if e.stderr else ""
        print(f"[!] command failed: {' '.join(cmd)}")
        if err.strip():
            print(err.strip())
        return False


def parse_args(argv):
    if len(argv) < 4 or "-h" in argv or "--help" in argv:
        print(__doc__)
        sys.exit(0 if "-h" in argv or "--help" in argv else 1)

    pdf_path = argv[1]
    sample_heading = argv[2]
    try:
        sample_page = int(argv[3])
    except ValueError:
        print(f"[!] Page must be an integer, got: {argv[3]}")
        sys.exit(1)

    keep = "--keep-intermediate" in argv
    out_path = None
    if "--out" in argv:
        i = argv.index("--out")
        out_path = argv[i + 1]

    return pdf_path, sample_heading, sample_page, keep, out_path


def main():
    for b in ("pdfxmeta", "pdftocgen", "pdftocio"):
        need(b)

    pdf_path, heading, page, keep, out_path = parse_args(sys.argv)

    if not os.path.exists(pdf_path):
        print(f"[!] File not found: {pdf_path}")
        sys.exit(1)

    base = os.path.splitext(pdf_path)[0]
    recipe = base + "_recipe.toml"
    toc_txt = base + "_toc.txt"

    # Step 1: learn the heading's font/size and write a level-1 recipe.
    print(f"--- 1/3: extracting font metadata for '{heading}' on page {page} ---")
    if not run(
        ["pdfxmeta", "-p", str(page), "-a", "1", pdf_path, heading],
        stdout_path=recipe,
    ):
        sys.exit(1)
    if os.path.getsize(recipe) == 0:
        print(f"[!] pdfxmeta produced an empty recipe. The string '{heading}' "
              f"may not appear verbatim on page {page}. Check spelling/case.")
        sys.exit(1)
    print(f"  -> wrote {recipe}")

    # Step 2: scan the whole PDF for matching headings.
    print(f"--- 2/3: finding all matching headings ---")
    if not run(
        ["pdftocgen", "-r", recipe, pdf_path],
        stdout_path=toc_txt,
    ):
        sys.exit(1)
    # Count entries by counting non-blank lines.
    with open(toc_txt, encoding="utf-8") as f:
        n_entries = sum(1 for line in f if line.strip())
    if n_entries == 0:
        print("[!] pdftocgen found 0 headings. The recipe may be too strict, "
              "or the sample heading uses a font no other heading shares.")
        sys.exit(1)
    print(f"  -> wrote {toc_txt} ({n_entries} entries)")

    # Step 3: stamp the TOC onto the PDF.
    print(f"--- 3/3: injecting TOC into PDF ---")
    cmd = ["pdftocio", "-t", toc_txt]
    if out_path:
        cmd += ["-o", out_path]
    else:
        cmd += ["-o", base + "_toc.pdf"]
    cmd.append(pdf_path)
    # if out_path:
    #     cmd += ["-o", out_path]
    # cmd.append(pdf_path)
    if not run(cmd):
        sys.exit(1)

    # pdftocio writes <base>_toc.pdf by default unless --out was used.
    final = out_path if out_path else base + "_toc.pdf"
    if os.path.exists(final):
        size_mb = os.path.getsize(final) / 1024 / 1024
        print(f"  -> {final} ({size_mb:.1f} MB)")
    else:
        print(f"  -> done (check working directory for pdftocio output)")

    if not keep:
        for f in (recipe, toc_txt):
            try:
                os.remove(f)
            except OSError:
                pass
        print("  (intermediate files removed; pass --keep-intermediate to keep them)")
    else:
        print(f"  (kept: {recipe}, {toc_txt})")


if __name__ == "__main__":
    main()
