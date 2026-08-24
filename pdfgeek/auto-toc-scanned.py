"""
Build a TOC for a scanned PDF from its printed Contents.

Three ways to supply the entries. They can be combined.

1. --scan FIRST LAST
   Pull text from the Contents pages themselves (works only if the PDF
   has a text layer, e.g. it went through ocrmypdf). The script parses
   lines like "Chapter 4   The Will ........ 87" into (title, page).

2. --from-file toc.txt
   You typed/pasted the entries by hand, one per line:
       The Argument        12
       The First Treatise  34
   (tab or 2+ spaces between title and page).

3. --find-headings
   Ignore the printed page numbers and find the headings in the body
   text instead, taking the PDF page where each one actually appears.
   Use this when the Contents is unreadable, has no page numbers, or
   disagrees with the book. See "Finding headings" below.

Either way, --offset N maps PRINTED page numbers to PDF page numbers.
The book's printed page 1 is usually not PDF page 1 because of front
matter. If printed page 1 == PDF page 13, pass --offset 12. Or let
--auto-offset work it out for you.

Usage:
    python auto-toc-scanned.py book.pdf --scan 4 5 --dump
    python auto-toc-scanned.py book.pdf --scan 5 8 --offset 12
    python auto-toc-scanned.py book.pdf --scan 5 8 --offset 12 --dry-run
    python auto-toc-scanned.py book.pdf --from-file toc.txt --offset 12
    python auto-toc-scanned.py book.pdf --from-file toc.txt --levels numeric
    python auto-toc-scanned.py book.pdf --find-headings --levels numeric
    python auto-toc-scanned.py book.pdf --scan 4 5 --find-headings --auto-offset

--dump (with --scan) writes the raw text of the Contents pages to
<base>_toc_draft.txt, one line per non-empty source line, with junk
leader lines removed. It does NOT parse or guess - you edit the draft
into 'Title<2+ spaces>page' lines, then feed it back with --from-file.

--dry-run prints what it parsed without writing the PDF, so you can
check the offset and parsing before committing.

-o PATH  output path (default: <base>_toc.pdf).

Levels
------
--levels          ALL-CAPS title lines become level 1, the rest level 2.
                  Useful when chapter headings are printed in caps and
                  subsections in mixed case.
--levels indent   level comes from the indentation of each line in the
                  --from-file draft: two spaces (or one tab) per level.
                  Use this when the book's own Contents is indented and
                  you want the outline to mirror it exactly, titles and
                  all, with no invented section numbers. Any depth works,
                  not just two levels.
--levels numeric  level comes from the section number that starts the
                  title: "3 Ersatzism" is level 1, "3.2 Linguistic
                  Ersatzism" level 2, "3.2.1 ..." level 3. This is the
                  right mode for most academic books, whose chapter
                  titles are usually mixed case and so invisible to the
                  ALL-CAPS heuristic.

With --levels numeric, a section whose parent has no entry of its own
(a 3.1 with no 3) gets one synthesised at the same page, so the outline
never dangles. Pass --no-synth-parents to switch that off.

If a level-2 line appears before any level-1 line, it is promoted to
keep the outline valid.

Finding headings
----------------
--find-headings scans the body for lines that look like numbered
headings and records the PDF page each one is on. Titles still come
from the Contents if you gave one; only the page numbers are replaced.
That combination is the useful one: the Contents gives you good titles
(including chapter titles, which are often set with the number on its
own line and so are not matched in the body), while the body gives you
page numbers that cannot be wrong.

--heading-re RE   override the pattern. It must capture the section
                  number as group 1 and the title as group 2. The
                  default wants a line that is a dotted number, then
                  spaces, then a short title starting with a capital.
--body-from N     ignore matches before PDF page N (keeps the Contents
                  pages themselves from being read as headings).

Only the FIRST occurrence of each section number is kept, so later
cross-references ("as I argued in 3.2") do not win. The parsed numbers
are then checked for order, and anything out of sequence is flagged -
that is usually OCR garble in the number (a 1.7 read as 7.7), which
you then fix by hand.

--auto-offset  works out --offset by comparing where headings actually
               are with the page numbers the Contents claims, and using
               the most common difference. Prints the vote so you can
               see how solid it is.

--verify  after building, check that each bookmark's target page really
          contains that title (running heads make this work even when
          the heading sits mid-page), and report the ones that do not.
          Implied by --dry-run.

Requires: pymupdf  (pip install pymupdf)
"""

import argparse
import os
import re
import sys
from collections import Counter

import pymupdf


# Match "Some Title <leader/space> 87" with the page as a trailing integer.
# Leader can be dots, spaces, or a mix. Roman numerals are handled separately.
LINE_RE = re.compile(r"^(.*?)[\s.]{2,}(\d+)\s*$")

# A numbered heading sitting on its own line in the body: a dotted section
# number, then a short title starting with a capital. The length cap is what
# keeps ordinary prose that happens to open with a number from matching.
HEADING_RE = r"^[ \t]*(\d+(?:\.\d+)+)[ \t]+([A-Z][^\n]{0,60})$"

# A section number at the start of a title, e.g. "3.2 Linguistic Ersatzism".
NUM_RE = re.compile(r"^\s*(\d+(?:\.\d+)*)\s+(.*)$")


def is_all_caps(title):
    """True if the title has at least one A-Z letter and no lowercase a-z.
    Digits, spaces, and punctuation don't count either way, so headings like
    'PRIMARY AND SECONDARY QUALITIES' or 'CHAPTER 3' match while a normal
    subsection title with any lowercase letter does not."""
    return bool(re.search(r"[A-Z]", title)) and not re.search(r"[a-z]", title)


def section_number(title):
    """Return the dotted section number a title starts with, or None."""
    m = NUM_RE.match(title)
    return m.group(1) if m else None


def numeric_level(title):
    """Level from the leading section number: 3 -> 1, 3.2 -> 2, 3.2.1 -> 3.
    Titles with no number are level 1."""
    num = section_number(title)
    return num.count(".") + 1 if num else 1


def parse_lines(text, level_mode=None):
    """Turn raw TOC text into [(level, title, printed_page), ...].

    level_mode None      every entry is level 1
    level_mode 'caps'    ALL-CAPS titles are level 1, the rest level 2
    level_mode 'numeric' level from the leading section number
    level_mode 'indent'  level from the line's leading indentation, two
                         spaces per level (a tab counts as one level)
    """
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
        page = int(m.group(2))
        if not title:
            continue
        if level_mode == "numeric":
            level = numeric_level(title)
        elif level_mode == "caps":
            level = 1 if is_all_caps(title) else 2
        elif level_mode == "indent":
            lead = raw[:len(raw) - len(raw.lstrip())]
            level = 1 + lead.count("\t") + lead.replace("\t", "").count(" ") // 2
        else:
            level = 1
        entries.append((level, title, page))
    return entries


def parse_contents_layout(doc, first, last, level_mode=None):
    """Parse Contents pages by geometry rather than by line text.

    A scanned Contents often comes back from OCR with the titles in one text
    block and the page numbers in another, so nothing ever looks like
    "Title .... 87" on a single line and the line parser finds nothing. Here
    each title line is instead paired with whatever number sits to its right
    on the same horizontal band.

    Returns [(level, title, printed_page_or_None), ...] in reading order.
    Titles with no number are kept with None: --find-headings can still place
    them, and chapter titles printed without a page number are exactly the
    ones you want to keep.
    """
    entries = []
    for p in range(first - 1, last):
        if not 0 <= p < doc.page_count:
            continue
        lines = []
        for b in doc[p].get_text("dict")["blocks"]:
            for l in b.get("lines", []):
                txt = "".join(s["text"] for s in l.get("spans", []))
                txt = re.sub(r"\s+", " ", txt).strip(" .")
                if txt:
                    lines.append((l["bbox"], txt))

        nums, titles = [], []
        for bbox, txt in lines:
            if re.fullmatch(r"\d{1,4}", txt):
                nums.append((bbox, int(txt)))
            else:
                titles.append((bbox, txt))

        for bbox, txt in sorted(titles, key=lambda t: t[0][1]):
            if re.fullmatch(r"(?i)contents", txt):
                continue
            # A number already at the end of the line wins; that's the easy
            # layout and needs no guessing.
            m = LINE_RE.match(txt)
            if m and m.group(1).strip(" ."):
                title, page = re.sub(r"\s+", " ", m.group(1)).strip(" ."), \
                    int(m.group(2))
            else:
                title, page = txt, None
                cy = (bbox[1] + bbox[3]) / 2
                h = max(bbox[3] - bbox[1], 1.0)
                best = None
                for nb, val in nums:
                    ncy = (nb[1] + nb[3]) / 2
                    # same band, and to the right of where the title starts
                    if abs(ncy - cy) <= h * 0.75 and nb[0] >= bbox[0]:
                        d = abs(ncy - cy)
                        if best is None or d < best[0]:
                            best = (d, val)
                if best:
                    page = best[1]
            if not title:
                continue
            if level_mode == "numeric":
                level = numeric_level(title)
            elif level_mode == "caps":
                level = 1 if is_all_caps(title) else 2
            else:
                level = 1
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
        if re.fullmatch(r"[.•·:;,~\-_‘’°¤\s]+", line):
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


def find_headings(doc, pattern, body_from=1):
    """Scan the body for numbered headings.

    Returns {section_number: (title, pdf_page)} keeping the FIRST place each
    number appears, so later cross-references don't displace the real heading.
    """
    rx = re.compile(pattern, re.M)
    found = {}
    for p in range(body_from - 1, doc.page_count):
        for m in rx.finditer(doc[p].get_text()):
            num = m.group(1)
            if num not in found:
                title = re.sub(r"\s+", " ", m.group(2)).strip(" .")
                found[num] = (title, p + 1)
    return found


def _num_key(num):
    return tuple(int(x) for x in num.split("."))


def check_order(found):
    """Flag section numbers that appear out of order - normally OCR garble
    in the number itself. Returns a list of complaint strings."""
    seq = sorted(found.items(), key=lambda kv: kv[1][1])  # by page
    bad = []
    for (n1, (_, p1)), (n2, (_, p2)) in zip(seq, seq[1:]):
        if _num_key(n2) <= _num_key(n1):
            bad.append(f"{n2} (page {p2}) comes after {n1} (page {p1})")
    return bad


def vote_offset(entries, found):
    """Most common (pdf_page - printed_page) over entries we located in the
    body. Returns (offset, votes, total) or None."""
    diffs = []
    for _, title, printed in entries:
        if printed is None:
            continue
        num = section_number(title)
        if num and num in found:
            diffs.append(found[num][1] - printed)
    if not diffs:
        return None
    c = Counter(diffs)
    off, votes = c.most_common(1)[0]
    return off, votes, len(diffs)


def _norm(s):
    return re.sub(r"[^a-z0-9 ]+", " ", s.lower())


def verify(doc, toc):
    """Report bookmarks whose target page doesn't mention the title. Running
    heads mean this usually holds even when the heading is mid-page, so a
    miss is worth a look."""
    bad = []
    for level, title, page in toc:
        body = _norm(title)
        body = NUM_RE.sub(r"\2", body) if NUM_RE.match(body) else body
        words = body.split()
        if len(words) < 2:
            continue  # too generic to check ("Index", "Preface")
        probe = " ".join(words[:4])
        text = _norm(doc[page - 1].get_text())
        if page < doc.page_count:
            text += " " + _norm(doc[page].get_text())
        if probe not in re.sub(r"\s+", " ", text):
            bad.append((title, page))
    return bad


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


def synth_parents(toc, titles=None):
    """Give every numbered section whose parent is missing a parent entry at
    the same page, so the outline doesn't dangle.

    titles maps a section number to a title, and is used when the Contents
    named a chapter but gave no page for it (a chapter head is usually printed
    with its number on a line of its own, so --find-headings never matches it).
    Without a title we fall back to a placeholder.
    """
    titles = titles or {}
    have = {section_number(t) for _, t, _ in toc if section_number(t)}
    extra = []
    for level, title, page in toc:
        num = section_number(title)
        if not num:
            continue
        parts = num.split(".")
        for k in range(1, len(parts)):
            parent = ".".join(parts[:k])
            if parent not in have:
                have.add(parent)
                extra.append([k, titles.get(parent, f"{parent}  (section "
                                                    f"{parent})"), page])
    if not extra:
        return toc
    merged = toc + extra
    merged.sort(key=lambda e: (e[2], e[0]))
    return merged


def read_file(path):
    with open(path, encoding="utf-8") as f:
        return f.read()


def build_parser():
    ap = argparse.ArgumentParser(
        description="Build a TOC for a scanned PDF from its printed Contents.",
        epilog=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf", help="input PDF")
    ap.add_argument("--scan", nargs=2, type=int, metavar=("FIRST", "LAST"),
                    help="read the Contents from these PDF pages")
    ap.add_argument("--from-file", metavar="PATH",
                    help="read entries from a hand-made list")
    ap.add_argument("--layout", action="store_true",
                    help="parse --scan pages by geometry, pairing each title "
                         "with the number to its right (for Contents pages "
                         "whose OCR splits titles and page numbers apart)")
    ap.add_argument("--find-headings", action="store_true",
                    help="take page numbers from headings in the body")
    ap.add_argument("--heading-re", default=HEADING_RE,
                    help="override the heading pattern (groups: number, title)")
    ap.add_argument("--body-from", type=int, default=1, metavar="N",
                    help="ignore heading matches before PDF page N")
    ap.add_argument("--offset", type=int, default=0,
                    help="printed page + offset = PDF page")
    ap.add_argument("--auto-offset", action="store_true",
                    help="work out the offset from located headings")
    ap.add_argument("--levels", nargs="?", const="caps",
                    choices=["caps", "numeric", "indent"], default=None,
                    help="outline levels: 'caps' (default), 'numeric', "
                         "or 'indent' (--from-file only)")
    ap.add_argument("--no-synth-parents", action="store_true",
                    help="don't invent missing parent entries")
    ap.add_argument("--dump", action="store_true",
                    help="write the raw Contents text as a draft and stop")
    ap.add_argument("--verify", action="store_true",
                    help="check each bookmark's target page mentions its title")
    ap.add_argument("--dry-run", action="store_true",
                    help="print what was parsed without writing the PDF")
    ap.add_argument("-o", "--out", help="output path")
    return ap


def main():
    ap = build_parser()

    # Bare invocation: show the help instead of an argparse error.
    if len(sys.argv) == 1:
        ap.print_help()
        return

    args = ap.parse_args()

    if not os.path.exists(args.pdf):
        print(f"[!] File not found: {args.pdf}")
        sys.exit(1)
    if not (args.scan or args.from_file or args.find_headings):
        ap.error("give --scan FIRST LAST, --from-file PATH, or --find-headings")

    doc = pymupdf.open(args.pdf)
    n = doc.page_count

    # --- entries from the Contents, if one was given ------------------------
    raw = None
    if args.scan:
        first, last = args.scan
        raw = extract_from_pages(doc, first, last)
        if not raw.strip():
            print("[!] No text found on those pages. If this is a pure image "
                  "scan with no text layer, OCR it first (e.g. ocrmypdf) or "
                  "use --from-file with a hand-typed list.")
            doc.close()
            sys.exit(1)
    elif args.from_file:
        raw = read_file(args.from_file)

    # --dump: write the raw text of the Contents pages as a draft and stop.
    # Only meaningful with --scan (you want the PDF's own text). You then edit
    # the draft by hand and feed it back with --from-file.
    if args.dump:
        if raw is None:
            print("[!] --dump needs --scan or --from-file.")
            doc.close()
            sys.exit(1)
        draft = dump_draft(raw)
        base, _ = os.path.splitext(args.pdf)
        draft_path = base + "_toc_draft.txt"
        with open(draft_path, "w", encoding="utf-8") as f:
            f.write(draft + "\n")
        n_lines = sum(1 for ln in draft.splitlines() if ln.strip())
        print(f"  -> wrote draft with {n_lines} line(s) to {draft_path}")
        print("     Edit it so each line reads 'Title<2+ spaces>page', fix OCR")
        print("     garble, delete junk lines, then run again with:")
        print(f"       python {os.path.basename(sys.argv[0])} "
              f"{os.path.basename(args.pdf)} "
              f"--from-file {os.path.basename(draft_path)} "
              f"--offset N --dry-run")
        doc.close()
        return

    if args.layout:
        if not args.scan:
            ap.error("--layout only applies to --scan pages")
        entries = parse_contents_layout(doc, args.scan[0], args.scan[1],
                                        args.levels)
    else:
        entries = parse_lines(raw, args.levels) if raw else []
        if args.scan and not entries:
            print("  [?] the line parser found nothing on those pages. If the "
                  "Contents has page numbers in a column of their own, OCR "
                  "has probably split them from the titles - try --layout.")

    # --- headings located in the body ---------------------------------------
    found = {}
    if args.find_headings:
        found = find_headings(doc, args.heading_re, args.body_from)
        print(f"--- found {len(found)} numbered heading(s) in the body ---")
        for num in sorted(found, key=_num_key):
            title, page = found[num]
            print(f"  {num:>7}  PDF page {page:>4}  {title}")
        for complaint in check_order(found):
            print(f"  [?] out of sequence: {complaint}")
            print("      probably OCR garble in the number; fix by hand.")
        if not found:
            print("  [!] nothing matched. Try --heading-re, or check the "
                  "PDF has a text layer.")

    # --- offset --------------------------------------------------------------
    offset = args.offset
    if args.auto_offset:
        if not (entries and found):
            print("[!] --auto-offset needs both a Contents (--scan/--from-file) "
                  "and --find-headings.")
            doc.close()
            sys.exit(1)
        vote = vote_offset(entries, found)
        if not vote:
            print("[!] --auto-offset: no Contents entry could be matched to a "
                  "heading. Give --offset yourself.")
            doc.close()
            sys.exit(1)
        offset, votes, total = vote
        print(f"--- auto-offset: {offset:+d} "
              f"({votes} of {total} matched entries agree) ---")
        if votes < total:
            print("    the rest disagree; check the listing below.")

    # --- combine -------------------------------------------------------------
    if entries:
        toc, replaced, unplaced = [], 0, []
        orphan_titles = {}
        for level, title, printed in entries:
            num = section_number(title)
            if num and num in found:
                page = found[num][1]     # body wins: it cannot be wrong
                replaced += 1
            elif printed is not None:
                page = printed + offset
            else:
                unplaced.append(title)   # no printed number, not found in body
                if num:
                    orphan_titles[num] = title
                continue
            toc.append([level, title, page])
        if found:
            print(f"--- {replaced} of {len(entries)} page number(s) taken from "
                  f"the body, the rest from the Contents at offset "
                  f"{offset:+d} ---")
    else:
        orphan_titles, unplaced = {}, []
        # Headings only: title carries the number so numeric levels work.
        toc = [[numeric_level(f"{num} x"), f"{num}  {found[num][0]}",
                found[num][1]]
               for num in sorted(found, key=_num_key)]

    if not toc:
        print("[!] Parsed 0 entries. Check the page range / file, or the "
              "TOC format may not match the expected 'Title .... page' shape.")
        doc.close()
        sys.exit(1)

    if args.levels == "numeric" and not args.no_synth_parents:
        before = len(toc)
        toc = synth_parents(toc, orphan_titles)
        if len(toc) > before:
            print(f"--- recovered {len(toc)-before} parent entry/ies "
                  f"---")

    if unplaced:
        present = {t for _, t, _ in toc}
        print(f"--- {len(unplaced)} Contents entry/ies had no page number and "
              f"no matching heading ---")
        for title in unplaced:
            if title in present:
                fate = "placed as a parent of its subsections"
            else:
                fate = "DROPPED: nothing to place it by, add it by hand"
            print(f"  [?] {title}  ({fate})")

    dropped = [e for e in toc if not 1 <= e[2] <= n]
    toc = [e for e in toc if 1 <= e[2] <= n]

    print(f"--- {len(toc)} entries (offset {offset:+d}) ---")
    for level, title, page in toc:
        print(f"  {'  ' * (level - 1)}{title}  ->  PDF page {page}")
    for level, title, page in dropped:
        print(f"  {title}  ->  {page}  <-- OUT OF RANGE, skipped")

    toc = _normalize_levels(toc)

    if args.verify or args.dry_run:
        bad = verify(doc, toc)
        if bad:
            print(f"--- {len(bad)} bookmark(s) whose target page does not "
                  f"mention the title ---")
            for title, page in bad:
                print(f"  [?] {title}  ->  PDF page {page}")
            print("      Check the offset, or the entry may be front/back "
                  "matter with no running head.")
        else:
            print("--- verify: every bookmark's target page mentions its "
                  "title ---")

    if args.dry_run:
        print("\n[dry-run] nothing written. Adjust --offset if the mapping "
              "looks wrong, then run again without --dry-run.")
        doc.close()
        return

    if not toc:
        print("[!] No in-range entries; nothing written.")
        doc.close()
        return

    doc.set_toc(toc)
    base, ext = os.path.splitext(args.pdf)
    out_path = args.out or (base + "_toc" + ext)
    if os.path.abspath(out_path) == os.path.abspath(args.pdf):
        print("[!] Output path equals input path; pick a different -o.")
        doc.close()
        sys.exit(1)
    doc.save(out_path)
    doc.close()
    print(f"\n  -> wrote {len(toc)} bookmark(s) to {out_path}")


if __name__ == "__main__":
    main()
