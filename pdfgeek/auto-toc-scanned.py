"""
Build a TOC for a scanned PDF from its printed Contents.

By default the TOC is flat (one level). With --levels, ALL-CAPS title
lines become level 1 and the rest become level 2, giving a two-level
outline (chapters with nested subsections).

Two ways to supply the entries:

1. --scan FIRST LAST
   Pull text from the Contents pages themselves (works only if the PDF
   has a text layer, e.g. it went through ocrmypdf). The script parses
   lines like "Chapter 4   The Will ........ 87" into (title, page).

2. --from-file toc.txt
   You typed/pasted the entries by hand, one per line:
       The Argument        12
       The First Treatise  34
   (tab or 2+ spaces between title and page).

Either way, --offset N maps PRINTED page numbers to PDF page numbers.
The book's printed page 1 is usually not PDF page 1 because of front
matter. If printed page 1 == PDF page 13, pass --offset 12.

Usage:
    python contents_toc.py book.pdf --scan 4 5 --dump
    python contents_toc.py book.pdf --scan 5 8 --offset 12
    python contents_toc.py book.pdf --scan 5 8 --offset 12 --dry-run
    python contents_toc.py book.pdf --from-file toc.txt --offset 12
    python contents_toc.py book.pdf --from-file toc.txt --offset 12 --levels

--dump (with --scan) writes the raw text of the Contents pages to
<base>_toc_draft.txt, one line per non-empty source line, with junk
leader lines removed. It does NOT parse or guess - you edit the draft
into 'Title<2+ spaces>page' lines, then feed it back with --from-file.

--levels makes a two-level TOC: a line whose title is entirely in capitals
(no lowercase letters) is level 1, everything else is level 2. Useful when
chapter headings are printed in caps and subsections in mixed case. If a
level-2 line appears before any level-1 line, it is promoted to keep the
outline valid.

--dry-run prints what it parsed without writing the PDF, so you can
check the offset and parsing before committing.

Requires: pymupdf  (pip install pymupdf)
"""

import os
import re
import sys
import pymupdf


# Match "Some Title <leader/space> 87" with the page as a trailing integer.
# Leader can be dots, spaces, or a mix. Roman numerals are handled separately.
LINE_RE = re.compile(r"^(.*?)[\s.]{2,}(\d+)\s*$")


def is_all_caps(title):
    """True if the title has at least one A-Z letter and no lowercase a-z.
    Digits, spaces, and punctuation don't count either way, so headings like
    'PRIMARY AND SECONDARY QUALITIES' or 'CHAPTER 3' match while a normal
    subsection title with any lowercase letter does not."""
    return bool(re.search(r"[A-Z]", title)) and not re.search(r"[a-z]", title)


def parse_lines(text, two_levels=False):
    """Turn raw TOC text into [(level, title, printed_page), ...].

    With two_levels=False every entry is level 1.
    With two_levels=True, ALL-CAPS title lines are level 1 and everything
    else is level 2."""
    entries = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        m = LINE_RE.match(line)
        if not m:
            # Lines without a trailing page number are usually headers
            # ("CONTENTS") or wrapped titles; skip quietly.
            continue
        title = re.sub(r"\s+", " ", m.group(1)).strip(" .")
        # Strip a leading "Chapter N" label's trailing junk if present, but
        # keep the title readable. We leave the title mostly as-is on purpose.
        page = int(m.group(2))
        if title:
            level = 1 if (not two_levels or is_all_caps(title)) else 2
            entries.append((level, title, page))
    return entries


def dump_draft(text):
    """Turn raw Contents-page text into a rough draft for hand-editing.

    This does NOT try to be clever. It keeps every non-empty line, collapses
    runs of whitespace, and drops lines that are obviously just dot-leaders
    or stray punctuation (so the draft isn't littered with '. . :' lines).
    Real titles, page numbers, and garble are all left for you to fix.
    """
    out = []
    for raw in text.splitlines():
        line = re.sub(r"\s+", " ", raw).strip()
        if not line:
            continue
        # Skip lines that are only leader dots / stray punctuation.
        if re.fullmatch(r"[.\u2022\u00b7:;,~\-_\u2018\u2019\u00b0\u00a4\s]+", line):
            continue
        out.append(line)
    return "\n".join(out)


def extract_from_pages(doc, first, last):
    """Concatenate text from PDF pages first..last (1-based, inclusive)."""
    chunks = []
    for p in range(first - 1, last):
        if 0 <= p < doc.page_count:
            chunks.append(doc[p].get_text())
    return "\n".join(chunks)


def _normalize_levels(toc):
    """PyMuPDF's set_toc requires each level to be at most prev_level + 1.
    If a level-2 entry comes before any level-1 entry (e.g. the Contents
    starts with a subsection), clamp it so the hierarchy stays valid.
    toc is a list of [level, title, page]."""
    out, prev = [], 0
    for level, title, page in toc:
        if level > prev + 1:
            level = prev + 1
        out.append([level, title, page])
        prev = level
    return out


def read_file(path):
    with open(path, encoding="utf-8") as f:
        return f.read()


def main():
    argv = sys.argv
    if len(argv) < 2 or "-h" in argv or "--help" in argv:
        print(__doc__)
        sys.exit(0 if ("-h" in argv or "--help" in argv) else 1)

    pdf_path = argv[1]
    if not os.path.exists(pdf_path):
        print(f"[!] File not found: {pdf_path}")
        sys.exit(1)

    offset = 0
    if "--offset" in argv:
        offset = int(argv[argv.index("--offset") + 1])

    dry_run = "--dry-run" in argv
    two_levels = "--levels" in argv

    doc = pymupdf.open(pdf_path)

    # Gather raw text from whichever source was requested.
    if "--scan" in argv:
        i = argv.index("--scan")
        first, last = int(argv[i + 1]), int(argv[i + 2])
        raw = extract_from_pages(doc, first, last)
        if not raw.strip():
            print("[!] No text found on those pages. If this is a pure image "
                  "scan with no text layer, OCR it first (e.g. ocrmypdf) or "
                  "use --from-file with a hand-typed list.")
            doc.close()
            sys.exit(1)
    elif "--from-file" in argv:
        path = argv[argv.index("--from-file") + 1]
        raw = read_file(path)
    else:
        print("[!] Give either --scan FIRST LAST or --from-file PATH.")
        doc.close()
        sys.exit(1)

    # --dump: write the raw text of the Contents pages as a draft and stop.
    # Only meaningful with --scan (you want the PDF's own text). You then edit
    # the draft by hand and feed it back with --from-file.
    if "--dump" in argv:
        draft = dump_draft(raw)
        base, _ = os.path.splitext(pdf_path)
        draft_path = base + "_toc_draft.txt"
        with open(draft_path, "w", encoding="utf-8") as f:
            f.write(draft + "\n")
        n_lines = sum(1 for ln in draft.splitlines() if ln.strip())
        print(f"  -> wrote draft with {n_lines} line(s) to {draft_path}")
        print("     Edit it so each line reads 'Title<2+ spaces>page', fix OCR")
        print("     garble, delete junk lines, then run again with:")
        print(f"       python {os.path.basename(sys.argv[0])} "
              f"{os.path.basename(pdf_path)} --from-file {os.path.basename(draft_path)} "
              f"--offset N --dry-run")
        doc.close()
        return

    entries = parse_lines(raw, two_levels=two_levels)
    if not entries:
        print("[!] Parsed 0 entries. Check the page range / file, or the "
              "TOC format may not match the expected 'Title .... page' shape.")
        doc.close()
        sys.exit(1)

    n = doc.page_count
    toc = []
    print(f"--- parsed {len(entries)} entries (offset {offset:+d}) ---")
    for level, title, printed in entries:
        pdf_page = printed + offset
        flag = ""
        if pdf_page < 1 or pdf_page > n:
            flag = "  <-- OUT OF RANGE, will skip"
        indent = "  " * (level - 1)
        print(f"  {printed:>4} -> {pdf_page:>4}  {indent}{title}{flag}")
        if 1 <= pdf_page <= n:
            toc.append([level, title, pdf_page])

    toc = _normalize_levels(toc)

    if dry_run:
        print("\n[dry-run] nothing written. Adjust --offset if the mapping "
              "looks wrong, then run again without --dry-run.")
        doc.close()
        return

    if not toc:
        print("[!] No in-range entries; nothing written.")
        doc.close()
        return

    doc.set_toc(toc)
    base, ext = os.path.splitext(pdf_path)
    out_path = base + "_toc" + ext
    doc.save(out_path)
    doc.close()
    print(f"\n  -> wrote {len(toc)} bookmark(s) to {out_path}")


if __name__ == "__main__":
    main()
