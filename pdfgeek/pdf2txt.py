#!/usr/bin/env python
# -*- coding: utf-8 -*-
r"""
pdf2txt.py -- reflow a compiled paper to plain text, dropping the
revision notes that sit in the right margin.

Normally run from within Emacs (M-x ysb/pdf2txt), which passes the
project's master PDF as the argument.  Running it with no arguments
prints this help.

    python pdf2txt.py paper.pdf                # -> paper.txt
    python pdf2txt.py paper.pdf out.txt
    python pdf2txt.py paper.pdf --page-markers      # [p. N] at each page start
    python pdf2txt.py paper.pdf --keep-math-unicode # leave U+1D4xx italics as-is

Why not pdftotext: the pdftotext on PATH here is Xpdf, not Poppler.  It has no
crop option, and it also drops every math variable (p, q, E, H vanish), shatters
letterspaced run-in headings into single letters ("Nu d g i n g"), and renders
curly quotes as ` and '.  PyMuPDF gets all of that right.

Requires PyMuPDF:  pip install pymupdf

What it does
  * finds the right-hand margin column geometrically and clips it away, so the
    "Added: C5(b)" / "Cont'd" revision notes never reach the text;
  * reflows wrapped lines back into paragraphs, using two independent signals
    (previous line reaches the justified right edge AND next line sits at the
    block's continuation indent) so that numbered definitions, displayed
    examples and the hanging-indent bibliography keep their shape;
  * rejoins paragraphs split across a page break, even when a footnote block
    was typeset between the two halves;
  * stops at the bibliography: the "References" heading and everything after it
    are dropped (pass --keep-refs to retain them);
  * repairs the spacing PyMuPDF loses after a math glyph ("E to" not "Eto"),
    and the \enquote padding (curly quotes with no padding inside);
  * de-hyphenates line-break hyphens, but only where the line actually ends in
    one -- it reports each such join so you can eyeball it.

Every geometric threshold is derived from the file itself, so a recompile that
shifts the layout will not silently start eating body text.  The run prints
what it detected and what it dropped; check that summary each time.
"""

import argparse
import collections
import re
import sys
import unicodedata

try:
    import fitz  # PyMuPDF
except ImportError:
    sys.exit("PyMuPDF is missing.  Install it with:  pip install pymupdf")

FULL_TOL = 12.0    # how near the block's right edge still counts as "wrapped"
INDENT_TOL = 4.0   # slack when matching a line to its block's continuation indent
MATH_RANGE = (0x1D400, 0x1D7FF)
LETTERLIKE = {"ℎ": "h"}          # PLANCK CONSTANT, LaTeX's italic h
MATH_CLASS = "\U0001D400-\U0001D7FFℎ"

# a bare bibliography heading, with or without a section number
REF_HEADING = re.compile(
    r"^(?:\d+\.?\s+)?(references|bibliography|works\s+cited)\.?$", re.I)


# --------------------------------------------------------------------------
# geometry
# --------------------------------------------------------------------------
def raw_lines(doc):
    """Every text line in the document, with its box, text and font size."""
    out = []
    for pno, page in enumerate(doc, 1):
        for b in page.get_text("dict", sort=True)["blocks"]:
            for l in b.get("lines", []):
                txt = "".join(s["text"] for s in l["spans"])
                if txt.strip():
                    out.append((pno, l["bbox"], txt,
                                max(s["size"] for s in l["spans"])))
    return out


def detect_margins(doc):
    """
    Locate the marginal-note columns.

    The justified right edge of the text block is by far the most common line
    x1 in the document, so anything *starting* to the right of it is outside
    the text block -- that is the margin column.  Mirrored for a left margin.
    """
    lines = raw_lines(doc)
    width = doc[0].rect.width
    x0s = [round(bb[0]) for _, bb, _, _ in lines]
    x1s = [round(bb[2]) for _, bb, _, _ in lines]
    body_right = collections.Counter(x1s).most_common(1)[0][0]
    body_left = collections.Counter(x0s).most_common(1)[0][0]

    right_col = [bb[0] for _, bb, _, _ in lines if bb[0] > body_right + 5]
    left_col = [bb[2] for _, bb, _, _ in lines if bb[2] < body_left - 5]

    right_cut = min(right_col) - 1.0 if right_col else width
    left_cut = max(left_col) + 1.0 if left_col else 0.0
    return left_cut, right_cut, body_left, body_right


def dropped_margin_text(doc, left_cut, right_cut):
    """The lines the clip will discard -- printed so they can be eyeballed."""
    out = []
    for pno, bb, txt, _ in raw_lines(doc):
        if bb[0] >= right_cut or bb[2] <= left_cut:
            out.append((pno, round(bb[0]), txt.strip()))
    return out


# --------------------------------------------------------------------------
# line assembly
# --------------------------------------------------------------------------
def visual_lines(block):
    """
    Merge lines that share a baseline.  PyMuPDF splits a footnote marker from
    its text, and an equation tag "(1)" from the definition beside it; both
    belong on one line.
    """
    out = []
    for l in block["lines"]:
        txt = "".join(s["text"] for s in l["spans"])
        if not txt.strip():
            continue
        x0, y0, x1, y1 = l["bbox"]
        size = max(s["size"] for s in l["spans"])
        if out:
            px0, py0, px1, py1, ptxt, psize = out[-1]
            overlap = min(y1, py1) - max(y0, py0)
            side_by_side = x0 > px1 - 1
            if side_by_side and overlap > 0.5 * min(y1 - y0, py1 - py0):
                sep = "" if ptxt.endswith(" ") or txt.startswith(" ") else " "
                out[-1] = (px0, min(y0, py0), x1, max(y1, py1),
                           ptxt.rstrip() + sep + txt.lstrip(), max(size, psize))
                continue
        out.append((x0, y0, x1, y1, txt, size))
    return out


def append(prev, nxt, hyphen_log):
    """Join a wrapped line onto the paragraph so far."""
    prev = prev.rstrip()
    if prev.endswith("-"):
        stem, tail = prev[:-1], nxt.lstrip()
        hyphen_log.append("{}-|{} -> {}{}".format(
            stem[-14:], tail[:14], stem[-14:], tail[:14]))
        return stem + tail
    return prev + " " + nxt.lstrip()


def extract_paragraphs(doc, left_cut, right_cut, hyphen_log):
    clip = fitz.Rect(left_cut, 0, right_cut, doc[0].rect.height)
    paras = []   # [page, text, first_x0, last_x1, block_right, cont_indent, size]
    for pno, page in enumerate(doc, 1):
        for b in page.get_text("dict", clip=clip, sort=True)["blocks"]:
            if "lines" not in b:
                continue
            vl = visual_lines(b)
            if not vl:
                continue
            # bare centred page-number footer
            if (len(vl) == 1 and re.fullmatch(r"\d+", vl[0][4].strip())
                    and vl[0][1] > page.rect.height - 70):
                continue
            tail = vl[1:] if len(vl) > 1 else vl      # the continuation indent
            cont = collections.Counter(                # lives in non-first lines
                round(v[0]) for v in tail).most_common(1)[0][0]
            right = max(v[2] for v in vl)
            cur = None
            for x0, _, x1, _, txt, size in vl:
                wrapped = cur is not None and cur[3] >= right - FULL_TOL
                aligned = abs(x0 - cont) <= INDENT_TOL
                if wrapped and aligned:
                    cur[1] = append(cur[1], txt, hyphen_log)
                    cur[3] = x1
                    cur[6] = max(cur[6], size)
                else:
                    if cur:
                        paras.append(cur)
                    cur = [pno, txt.strip(), x0, x1, right, cont, size]
            if cur:
                paras.append(cur)
    return paras


def modal_size(paras):
    """The document's body font size."""
    return collections.Counter(round(p[6]) for p in paras).most_common(1)[0][0]


def cut_references(paras, modal):
    """
    Drop the bibliography.  Anchored on a paragraph that is *nothing but* a
    references heading, so a mid-sentence mention of "references" can't trigger
    it.  Run this before the cross-page join, or a bibliography entry can be
    welded onto the last body paragraph across the page break.

    The body's last footnotes are typeset at the foot of the page on which the
    bibliography starts, so in reading order they fall *below* the first
    reference entries.  A flat "drop everything after the heading" would lose
    them, so entries are discriminated by size: the bibliography is set at the
    body size, footnotes visibly smaller.  Returns (kept, dropped).
    """
    for i, p in enumerate(paras):
        if REF_HEADING.match(p[1].strip()):
            notes, refs = [], []
            for q in paras[i:]:
                (notes if q[6] < modal - 1.0 else refs).append(q)
            return paras[:i] + notes, refs
    return paras, []


def join_across_pages(paras, hyphen_log):
    """
    Rejoin a body paragraph broken by a page break.  Body is identified by the
    document's modal font size, which keeps footnotes (smaller) out of it; the
    two halves may have footnote blocks emitted between them.
    """
    if not paras:
        return paras, 0
    modal = collections.Counter(round(p[6]) for p in paras).most_common(1)[0][0]

    by_page = collections.defaultdict(list)
    for i, p in enumerate(paras):
        if abs(p[6] - modal) <= 0.6:
            by_page[p[0]].append(i)

    join_into = {}
    for pg in sorted(by_page):
        if pg + 1 not in by_page:
            continue
        a, b = by_page[pg][-1], by_page[pg + 1][0]
        pa, pb = paras[a], paras[b]
        if pa[3] >= pa[4] - FULL_TOL and abs(pb[2] - pb[5]) <= INDENT_TOL:
            join_into[b] = a

    merged, pos = [], {}
    for i, p in enumerate(paras):
        tgt = join_into.get(i)
        if tgt is not None and tgt in pos:
            dest = merged[pos[tgt]]
            dest[1] = append(dest[1], p[1], hyphen_log)
        else:
            pos[i] = len(merged)
            merged.append(p)
    return merged, len(join_into)


# --------------------------------------------------------------------------
# character repairs
# --------------------------------------------------------------------------
def repair(t):
    # PyMuPDF keeps the space before a math glyph but not the one after it
    t = re.sub("([" + MATH_CLASS + "])(?=[A-Za-z(=])", r"\1 ", t)
    t = re.sub("‘ +", "‘", t)      # drop \enquote's internal padding
    t = re.sub(" +’", "’", t)
    t = re.sub(r"[ \t]{2,}", " ", t)
    return t.strip()


def to_ascii_math(t):
    out = []
    for ch in t:
        if MATH_RANGE[0] <= ord(ch) <= MATH_RANGE[1]:
            out.append(unicodedata.normalize("NFKC", ch))
        else:
            out.append(LETTERLIKE.get(ch, ch))
    return "".join(out)


# --------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf", nargs="?", default=None,
                    help="master PDF to convert (normally supplied by Emacs)")
    ap.add_argument("txt", nargs="?", default=None,
                    help="default: input name with .txt")
    ap.add_argument("--page-markers", action="store_true",
                    help="insert a [p. N] line at the start of each page")
    ap.add_argument("--keep-math-unicode", action="store_true",
                    help="keep U+1D4xx math italics instead of folding to ASCII")
    ap.add_argument("--keep-margins", action="store_true",
                    help="do NOT clip the marginal revision notes")
    ap.add_argument("--keep-refs", action="store_true",
                    help="keep the bibliography (dropped by default)")
    a = ap.parse_args()

    if a.pdf is None:
        ap.print_help()
        sys.exit(1)

    if sys.stdout.encoding and sys.stdout.encoding.lower() != "utf-8":
        try:
            sys.stdout.reconfigure(encoding="utf-8")
        except Exception:
            pass

    out_path = a.txt or re.sub(r"\.pdf$", "", a.pdf, flags=re.I) + ".txt"

    doc = fitz.open(a.pdf)
    left, right, bl, br = detect_margins(doc)
    if a.keep_margins:
        left, right = 0.0, doc[0].rect.width

    print("{}: {} pages, {:.0f}x{:.0f}pt".format(
        a.pdf, len(doc), doc[0].rect.width, doc[0].rect.height))
    print("  text block x {}..{}pt -> clipping to x {:.0f}..{:.0f}pt".format(
        bl, br, left, right))

    dropped = dropped_margin_text(doc, left, right)
    if dropped:
        print("  dropped {} marginal line(s) -- check these are all notes:".format(
            len(dropped)))
        for pno, x0, txt in dropped:
            print("      p{:<3} x{:<4} {}".format(pno, x0, txt))
    else:
        print("  no marginal column detected")

    hyphen_log = []
    paras = extract_paragraphs(doc, left, right, hyphen_log)

    if not a.keep_refs:
        before = len(paras)
        paras, cut = cut_references(paras, modal_size(paras))
        if cut:
            kept_notes = len(paras) - (before - len(cut))
            print("  dropped bibliography: {} paragraph(s) from p{}, "
                  "'{}' .. '{}'".format(
                      len(cut), cut[0][0], cut[0][1][:34], cut[-1][1][:34]))
            if kept_notes:
                print("      (kept {} footnote(s) typeset below it)".format(
                    kept_notes))
        else:
            print("  no bibliography heading found -- nothing dropped")

    paras, njoin = join_across_pages(paras, hyphen_log)

    chunks, seen = [], set()
    for p in paras:
        body = repair(p[1])
        if not body:
            continue
        if a.page_markers and p[0] not in seen:
            seen.add(p[0])
            chunks.append("[p. {}]".format(p[0]))
        chunks.append(body if a.keep_math_unicode else to_ascii_math(body))

    text = "\n\n".join(chunks) + "\n"
    with open(out_path, "w", encoding="utf-8", newline="\r\n") as fh:
        fh.write(text)

    print("  paragraphs {} | cross-page joins {} | de-hyphenated {}".format(
        len(paras), njoin, len(hyphen_log)))
    for h in hyphen_log:
        print("      " + h)
    print("  wrote {}  ({:,} chars, {:,} words)".format(
        out_path, len(text), len(text.split())))


if __name__ == "__main__":
    main()
