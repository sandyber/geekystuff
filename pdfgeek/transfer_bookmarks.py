"""
transfer_bookmarks.py

Copy the bookmark outline (table of contents) from one PDF to another.
Useful when you've reprocessed a PDF -- stripped watermarks, redacted
footers, re-OCR'd, removed front matter -- and ended up with a clean
file that has lost its bookmarks.

The two PDFs must have the same page *content* in the same order; only
a constant shift in page numbering is supported. If the target adds or
removes pages in the middle, this script won't help.

See the in-script README (run with no args) for usage details.
"""
import os
import sys
from pathlib import Path

import pymupdf  # aka fitz


def shift_toc_simple(toc, offset):
    """Shift page numbers in a simple TOC: [[level, title, page], ...]."""
    return [[lvl, title, page + offset] for lvl, title, page in toc]


def shift_toc_detailed(toc, offset):
    """
    Shift page numbers in a detailed TOC:
    [[level, title, page, dest_dict], ...]

    The dest_dict may contain a 'page' key (0-based) that also needs shifting.
    """
    shifted = []
    for entry in toc:
        lvl, title, page, dest = entry
        new_dest = dict(dest) if isinstance(dest, dict) else dest
        if isinstance(new_dest, dict) and "page" in new_dest:
            new_dest["page"] = new_dest["page"] + offset
        shifted.append([lvl, title, page + offset, new_dest])
    return shifted


def transfer_bookmarks(source, target, output, offset=0, simple=False):
    src = pymupdf.open(source)
    dst = pymupdf.open(target)

    try:
        toc = src.get_toc(simple=simple)
        if not toc:
            print(f"[!] No bookmarks found in {source}", file=sys.stderr)
            dst.save(output)
            return 0

        if offset != 0:
            if simple:
                toc = shift_toc_simple(toc, offset)
            else:
                toc = shift_toc_detailed(toc, offset)

        n_pages = dst.page_count
        valid = []
        dropped = 0
        for entry in toc:
            page = entry[2]
            if 1 <= page <= n_pages:
                valid.append(entry)
            else:
                dropped += 1

        if dropped:
            print(
                f"[!] Dropped {dropped} bookmark(s) pointing outside "
                f"target's page range (1..{n_pages})",
                file=sys.stderr,
            )

        if not valid:
            print(
                "[!] No bookmarks remain after offset adjustment.",
                file=sys.stderr,
            )
            return 1

        dst.set_toc(valid)
        dst.save(output)
        print(
            f"Wrote {output} with {len(valid)} bookmark(s) "
            f"(offset {offset:+d})."
        )
        return 0
    finally:
        src.close()
        dst.close()


def print_help(script_name):
    print("\n" + "=" * 75)
    print("       PDF BOOKMARK TRANSFER (copy TOC between matching PDFs)")
    print("=" * 75)
    print("USAGE:")
    print(f"  python {script_name} <source.pdf> <target.pdf> [output.pdf]")
    print(f"                            [offset:N] [--simple]")
    print("\nEXAMPLES:")
    print(f"  python {script_name} original.pdf cleaned.pdf")
    print(f"  python {script_name} original.pdf cleaned.pdf cleaned_toc.pdf")
    print(f"  python {script_name} original.pdf cleaned.pdf offset:-4")
    print(f"  python {script_name} original.pdf cleaned.pdf offset:2 --simple")
    print("\nARGUMENTS:")
    print("  source.pdf    PDF whose bookmarks you want to copy.")
    print("  target.pdf    PDF that should receive the bookmarks (not modified).")
    print("  output.pdf    Optional. Defaults to '<target>_toc.pdf' next to target.")
    print("\nOPTIONS:")
    print("  offset:N      Integer added to every bookmark's page number.")
    print("                Positive  -> target has extra pages at the front.")
    print("                Negative  -> target is missing front-matter pages.")
    print("                Rule of thumb: offset = (target page) - (source page)")
    print("                for any page that appears in both files. Default 0.")
    print("                Bookmarks falling outside the target's page range")
    print("                after the shift are dropped with a warning.")
    print("  --simple      Use a flat TOC: [level, title, page] only, discarding")
    print("                zoom/position metadata. More robust when source and")
    print("                target differ in subtle ways (e.g. one re-OCR'd, page")
    print("                boxes resized). Try this first if bookmarks land on")
    print("                the right page but the wrong scroll position.")
    print("\nVERIFYING THE RESULT:")
    print("  python -c \"import pymupdf; print(pymupdf.open(r'OUT.pdf').get_toc())\"")
    print("\nREQUIREMENTS:")
    print("  PyMuPDF       (pip install pymupdf)")
    print("\nLIMITATIONS:")
    print("  - Only constant offsets supported; non-uniform changes (page")
    print("    inserted in the middle, sections reordered) need a manual map.")
    print("  - Named destinations preserved in detailed mode but resolve only")
    print("    if the same names exist in target. Use --simple to flatten.")
    print("  - Target's existing bookmarks (if any) are replaced, not merged.")
    print("=" * 75 + "\n")


def main():
    script_name = os.path.basename(sys.argv[0])

    if len(sys.argv) < 3 or "--help" in sys.argv or "-h" in sys.argv:
        print_help(script_name)
        return

    args = sys.argv[1:]

    # Pull out flag-style options first.
    use_simple = "--simple" in args
    args = [a for a in args if a != "--simple"]

    offset = 0
    remaining = []
    for a in args:
        low = a.lower()
        if low.startswith("offset:") or low.startswith("offset="):
            try:
                offset = int(a.split(":", 1)[-1].split("=", 1)[-1])
            except ValueError:
                print(f"[!] Invalid offset: {a}", file=sys.stderr)
                return
        else:
            remaining.append(a)

    if len(remaining) < 2:
        print("[!] Need at least <source.pdf> and <target.pdf>.", file=sys.stderr)
        print(f"    Run: python {script_name} --help", file=sys.stderr)
        return

    source = Path(remaining[0])
    target = Path(remaining[1])
    if len(remaining) >= 3:
        output = Path(remaining[2])
    else:
        output = target.with_name(f"{target.stem}_toc.pdf")

    for path in (source, target):
        if not path.is_file():
            print(f"[!] Not a file: {path}", file=sys.stderr)
            sys.exit(2)

    sys.exit(
        transfer_bookmarks(
            str(source),
            str(target),
            str(output),
            offset=offset,
            simple=use_simple,
        )
    )


if __name__ == "__main__":
    main()
