#!/usr/bin/env python
# -*- coding: utf-8 -*-
r"""
pdf2txt-ysb.py -- reflow a compiled paper to plain text, dropping the
revision notes that sit in the right margin.

Normally run from within Emacs (M-x ysb/pdf2txt), which passes the
project's master PDF as the argument.  Running it with no arguments
prints this help.

    python pdf2txt-ysb.py paper.pdf                # -> paper.txt
    python pdf2txt-ysb.py paper.pdf out.txt
    python pdf2txt-ysb.py paper.pdf 23-29          # pages 23-29 -> paper.txt
    python pdf2txt-ysb.py paper.pdf 2-10,17 out.txt
    python pdf2txt-ysb.py paper.pdf --pages 2-10,17     # the same, spelled out
    python pdf2txt-ysb.py paper.pdf --pages 12-         # p.12 to the end
    python pdf2txt-ysb.py paper.pdf --page-markers      # [p. N] at each page start
    python pdf2txt-ysb.py paper.pdf --keep-math-unicode # leave U+1D4xx italics as-is

Why not pdftotext: the pdftotext on PATH here is Xpdf, not Poppler.  It has no
crop option, and it also drops every math variable (p, q, E, H vanish), shatters
letterspaced run-in headings into single letters ("Nu d g i n g"), and renders
curly quotes as ` and '.  PyMuPDF gets all of that right.

Requires PyMuPDF:  pip install pymupdf

What it does
  * finds the text block geometrically, separately for odd and even pages
    (a two-sided book mirrors its margins), and drops the lines of a marginal
    column -- the "Added: C5(b)" / "Cont'd" revision notes -- whole.  Only a
    column that is genuinely there (many lines, many pages, one x) counts, so
    a stray folio cannot start eating body text;
  * leaves a two-column page's baselines alone, instead of weaving the two
    columns together line by line;
  * reflows wrapped lines back into paragraphs, using two independent signals
    (previous line reaches the justified right edge AND next line sits at the
    block's continuation indent) so that numbered definitions, displayed
    examples and the hanging-indent bibliography keep their shape;
  * rejoins paragraphs split across a page break, even when a footnote block
    was typeset between the two halves (with --pages, only across pages that
    are actually adjacent in the selection);
  * stops at the bibliography: the "References" heading and everything after it
    are dropped (pass --keep-refs to retain them);
  * drops running heads and folios ("Assertion 79") -- short lines at the
    very top or bottom of the page in a non-body font or carrying a number
    (--keep-heads retains them);
  * keeps a line-end hyphen that is a real one ("truth-value", not
    "truthvalue"), judged from how the document spells the word elsewhere;
  * reads the cells of a small table across OCR blocks as rows, and starts
    a new paragraph where the line spacing jumps, so a matrix does not
    swallow the paragraph after it;
  * --subs FILE applies a table of regex substitutions (pattern<TAB>
    replacement per line) for the OCR errors of one particular scan;
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
import io
import re
import sys
import unicodedata

try:
    import pymupdf
except ImportError:
    sys.exit("PyMuPDF is missing.  Install it with:  pip install pymupdf")

try:                                    # optional: only for glyph recovery
    from fontTools.agl import toUnicode as agl_to_unicode
    from fontTools.cffLib import CFFFontSet
except ImportError:
    CFFFontSet = agl_to_unicode = None

FULL_TOL = 12.0    # how near the block's right edge still counts as "wrapped"
INDENT_TOL = 4.0   # slack when matching a line to its block's continuation indent
PITCH_FACTOR = 1.4  # a line this much further down than usual starts a new paragraph
MATH_RANGE = (0x1D400, 0x1D7FF)
LETTERLIKE = {"ℎ": "h"}          # PLANCK CONSTANT, LaTeX's italic h
MATH_CLASS = "\U0001D400-\U0001D7FFℎ"

# a bare bibliography heading, with or without a section number
REF_HEADING = re.compile(
    r"^(?:\d+\.?\s+)?(references|bibliography|works\s+cited)\.?$", re.I)


# --------------------------------------------------------------------------
# glyph recovery
# --------------------------------------------------------------------------
# Glyph names TeX and MathType use that the Adobe Glyph List does not carry.
TEX_GLYPHS = {
    "llbracket": "⟦", "rrbracket": "⟧",          # stmaryrd
    "llparenthesis": "⦇", "rrparenthesis": "⦈",
    "lightning": "↯", "bigsqcup": "⨆",
    "vextendsingle": "|", "vextenddouble": "‖", "arrowvertex": "|",
    "uniondisplay": "⋃", "intersectiondisplay": "⋂",
    "summationdisplay": "∑", "productdisplay": "∏",
    "coproductdisplay": "∐", "integraldisplay": "∫",
    "logicalanddisplay": "⋀", "logicalordisplay": "⋁",
    "contintegraldisplay": "∮",
}

# "parenleftbig", "bracketleftBigg" -- a size-variant of a plain delimiter.
SIZE_SUFFIXES = ("display", "text", "Bigg", "bigg", "Big", "big", "ex")


def glyph_to_unicode(name):
    """Best-effort character for a PostScript glyph name, or None."""
    if not name or name == ".notdef":
        return None
    if name in TEX_GLYPHS:
        return TEX_GLYPHS[name]
    base = name
    while True:
        for suf in SIZE_SUFFIXES:
            if base.endswith(suf) and len(base) > len(suf):
                base = base[:-len(suf)]
                break
        else:
            break
    if base in TEX_GLYPHS:
        return TEX_GLYPHS[base]
    if agl_to_unicode is not None:
        got = agl_to_unicode(base)
        if got:
            return got
    return None


def glyph_repairs(doc):
    """
    Recover the glyphs PyMuPDF could not turn into Unicode.

    Subset fonts from TeX and MathType (stmaryrd, the CM extension fonts)
    frequently carry no usable ToUnicode table, so PyMuPDF hands back the raw
    character code -- U+0002, U+0003, ... -- and the symbol is silently lost.
    The embedded font program still records the glyph *name* for that code,
    and the name is enough to identify the character.

    Keyed by (font, code), because the same code means different things in
    different fonts: code 2 is "parenleftbig" in MathType's MTEX but
    "llbracket" in stmary10.  A document-wide substitution would corrupt one
    of the two.
    """
    if CFFFontSet is None:
        return {}, ["fontTools is not installed"]

    xrefs, notes, repairs = {}, [], {}
    for pno in range(doc.page_count):
        for font in doc.get_page_fonts(pno):
            xref, ext, _, basefont = font[0], font[1], font[2], font[3]
            xrefs.setdefault(xref, (basefont.split("+")[-1], ext))

    for xref, (short, ext) in sorted(xrefs.items()):
        if ext != "cff":
            continue
        try:
            buf = doc.extract_font(xref)[3]
            cff = CFFFontSet()
            cff.decompile(io.BytesIO(buf), None)
            encoding = cff[cff.fontNames[0]].Encoding
        except Exception as e:                      # a font we cannot read is
            notes.append("{}: {}".format(short, e))  # not a reason to abort
            continue
        # A font on one of the predefined encodings reports the *name* of that
        # encoding, not an array.  Indexing the string would silently yield
        # its letters -- "StandardEncoding"[2] == "a" -- and rewrite body text
        # into nonsense.  Such fonts map correctly without help anyway.
        if not isinstance(encoding, list):
            continue
        for code in range(32):
            if code >= len(encoding):
                break
            ch = glyph_to_unicode(encoding[code])
            if ch:
                repairs[(short, code)] = ch
    return repairs, notes


def span_text(span, repairs, log=None):
    """A span's text, with unmapped glyphs restored from its own font."""
    txt = span["text"]
    if not repairs or not any(ord(c) < 32 for c in txt):
        return txt
    font = span["font"].split("+")[-1]
    out = []
    for ch in txt:
        if ord(ch) < 32:
            rep = repairs.get((font, ord(ch)))
            if log is not None:
                log[(font, ord(ch), rep)] = log.get((font, ord(ch), rep), 0) + 1
            if rep is not None:
                out.append(rep)
                continue
        out.append(ch)
    return "".join(out)


# --------------------------------------------------------------------------
# geometry
# --------------------------------------------------------------------------
def raw_lines(doc, pages=None):
    """
    Every text line in the document, with its box, text and font size.
    ``pages`` is a set of 1-based page numbers, or None for the whole file.
    """
    out = []
    for pno, page in enumerate(doc, 1):
        if pages is not None and pno not in pages:
            continue
        for b in page.get_text("dict", sort=True)["blocks"]:
            for l in b.get("lines", []):
                txt = "".join(s["text"] for s in l["spans"])
                if txt.strip():
                    out.append((pno, l["bbox"], txt,
                                max(s["size"] for s in l["spans"])))
    return out


def page_size(doc):
    """
    The document's modal page size, as (width, height).

    Page 0 is not a safe stand-in for the document.  A cover, a plate or a
    fold-out is often a different size from the body, and one 252x381pt cover
    in front of 329 pages of 396x610pt is enough to clip every body page to
    the cover's box -- 108pt off the right of every line and 229pt off the
    bottom.  The size most pages share is the one the body text lives on.
    """
    sizes = collections.Counter(
        (round(p.rect.width, 1), round(p.rect.height, 1)) for p in doc)
    return sizes.most_common(1)[0][0]


MARGIN_GAP = 5.0      # a line this far outside the text block is not body text
MIN_COL_LINES = 10    # a marginal column needs this many such lines ...
MIN_COL_PAGES = 3     # ... on this many pages, mostly at one x, to be believed


def margin_column(cands):
    """
    Do these out-of-block lines, as (page, x0) pairs, form a real column of
    notes?  Notes are set left-aligned, so their x0 is the same line after
    line, and they recur page after page.  A folio here, an overhanging
    bracket there, does not qualify -- and such strays were enough, under a
    "clip anything outside the block" rule, to shave the first glyph off every
    line of a two-sided book.
    """
    if len(cands) < MIN_COL_LINES:
        return False
    if len({p for p, _ in cands}) < MIN_COL_PAGES:
        return False
    _, n = collections.Counter(round(x) for _, x in cands).most_common(1)[0]
    return n >= max(MIN_COL_LINES // 2, len(cands) // 2)


def detect_margins(doc):
    """
    Locate the text block and any marginal-note column, separately for odd
    and even pages.

    A two-sided book mirrors its margins, so the block sits at one x on the
    recto pages and another on the verso; measured together, one side's lines
    look as if they poke out of the other side's block.  A single-sided paper
    simply yields the same answer for both parities.

    Returns {parity: (left_cut, right_cut, body_left, body_right)}.  A line
    starting at or beyond right_cut, or ending at or before left_cut, is a
    marginal note and is dropped whole; None means nothing is dropped on that
    side.
    """
    lines = raw_lines(doc)
    width = page_size(doc)[0]
    if not lines:
        return {0: (None, None, 0.0, width), 1: (None, None, 0.0, width)}

    edges, cands = {}, {}
    for parity in (0, 1):
        mine = [ln for ln in lines if ln[0] % 2 == parity] or lines
        x0s = [round(bb[0]) for _, bb, _, _ in mine]
        x1s = [round(bb[2]) for _, bb, _, _ in mine]
        body_left = collections.Counter(x0s).most_common(1)[0][0]
        body_right = collections.Counter(x1s).most_common(1)[0][0]
        edges[parity] = (body_left, body_right)
        # Candidates carry their x as an offset from this parity's own block
        # edge, so that a column mirrored across the gutter still lines up
        # when the two parities are pooled.
        cands[parity] = (
            [(p, bb[0] - body_right) for p, bb, _, _ in mine
             if bb[0] > body_right + MARGIN_GAP],
            [(p, body_left - bb[0]) for p, bb, _, _ in mine
             if bb[2] < body_left - MARGIN_GAP])

    # A revision column is sparse -- a few notes on a few pages -- so the
    # evidence is judged over the whole document as well as per parity.
    pooled = (cands[0][0] + cands[1][0], cands[0][1] + cands[1][1])
    geo = {}
    for parity in (0, 1):
        body_left, body_right = edges[parity]
        right_col, left_col = cands[parity]
        right_cut = (body_right + MARGIN_GAP
                     if margin_column(right_col) or margin_column(pooled[0])
                     else None)
        left_cut = (body_left - MARGIN_GAP
                    if margin_column(left_col) or margin_column(pooled[1])
                    else None)
        geo[parity] = (left_cut, right_cut, body_left, body_right)
    return geo


def outside_block(x0, x1, left_cut, right_cut):
    """Is a line box entirely in a marginal column?"""
    return ((right_cut is not None and x0 >= right_cut)
            or (left_cut is not None and x1 <= left_cut))


def dropped_margin_text(doc, geo, pages=None):
    """The lines the margin rule will discard -- printed so they can be eyeballed."""
    out = []
    for pno, bb, txt, _ in raw_lines(doc, pages):
        left_cut, right_cut = geo[pno % 2][:2]
        if outside_block(bb[0], bb[2], left_cut, right_cut):
            out.append((pno, round(bb[0]), txt.strip()))
    return out


def parse_pages(spec, npages):
    """
    Turn "2-10,17" into a sorted set of 1-based page numbers.  Open ranges are
    allowed ("12-" runs to the end, "-4" from the start).  A page the document
    does not have is an error rather than a silent omission, so a typo cannot
    quietly shrink the output.
    """
    want = set()
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        m = re.match(r"^(\d*)\s*-\s*(\d*)$", part)
        if m and (m.group(1) or m.group(2)):
            lo = int(m.group(1)) if m.group(1) else 1
            hi = int(m.group(2)) if m.group(2) else npages
            if lo > hi:
                raise ValueError("page range {!r} runs backwards".format(part))
            want.update(range(lo, hi + 1))
        elif part.isdigit():
            want.add(int(part))
        else:
            raise ValueError("cannot read page spec {!r}".format(part))

    if not want:
        raise ValueError("no pages selected")
    bad = sorted(n for n in want if n < 1 or n > npages)
    if bad:
        raise ValueError("no such page: {} (document has {} page{})".format(
            ", ".join(str(n) for n in bad), npages, "" if npages == 1 else "s"))
    return want


def looks_like_pages(tok):
    """
    True for a bare page spec -- "29", "23-29", "2-10,17", "12-".

    Only digits, commas, hyphens and spaces, and at least one digit, so no
    real file name can be mistaken for one (a file literally called 23-29,
    with no extension, would be; give it as --pages' neighbour or add the
    extension).
    """
    return (any(c.isdigit() for c in tok)
            and all(c.isdigit() or c in ",- " for c in tok)
            and tok.strip(",- "))


def split_positionals(args):
    """
    Sort the bare arguments into (pdf, pagespec, out) in any order.

    So "paper.pdf 23-29" means pages 23-29 of paper.pdf written to paper.txt,
    while "paper.pdf out.txt" still means the whole file written to out.txt.
    """
    specs = [t for t in args if looks_like_pages(t)]
    names = [t for t in args if not looks_like_pages(t)]
    if len(specs) > 1:
        raise ValueError("page range given more than once: {}".format(
            ", ".join(repr(t) for t in specs)))
    if not names:
        raise ValueError("no PDF given")
    if len(names) > 2:
        raise ValueError("too many file names: {}".format(
            ", ".join(repr(t) for t in names)))
    pdf = names[0]
    out = names[1] if len(names) > 1 else None
    return pdf, (specs[0] if specs else None), out


def describe_pages(pages):
    """Render a page set back as "2-10, 17" for the run summary."""
    runs, out = [], []
    for n in sorted(pages):
        if runs and n == runs[-1][1] + 1:
            runs[-1][1] = n
        else:
            runs.append([n, n])
    for lo, hi in runs:
        out.append(str(lo) if lo == hi else "{}-{}".format(lo, hi))
    return ", ".join(out)


# --------------------------------------------------------------------------
# line assembly
# --------------------------------------------------------------------------
def visual_lines(block, repairs=None, glyph_log=None, cuts=(None, None)):
    """
    Merge lines that share a baseline.  PyMuPDF splits a footnote marker from
    its text, and an equation tag "(1)" from the definition beside it; both
    belong on one line.

    Lines in a marginal column (``cuts``) are dropped here, whole.  They used
    to be removed with a clip rectangle, which cuts through glyphs: a cut a
    few points inside the text block took the first letter off every line.
    """
    out = []
    for l in block["lines"]:
        txt = "".join(span_text(s, repairs, glyph_log) for s in l["spans"])
        if not txt.strip():
            continue
        x0, y0, x1, y1 = l["bbox"]
        if outside_block(x0, x1, cuts[0], cuts[1]):
            continue
        size = max(s["size"] for s in l["spans"])
        fonts = {s["font"].split("+")[-1] for s in l["spans"]}
        if out and same_baseline(out[-1], (x0, y0, x1, y1)):
            out[-1] = merge_line(out[-1], (x0, y0, x1, y1, txt, size, fonts))
            continue
        out.append((x0, y0, x1, y1, txt, size, fonts))
    return out


def same_baseline(a, b):
    """Two line boxes side by side on one baseline (b to the right of a)."""
    ax0, ay0, ax1, ay1 = a[:4]
    bx0, by0, bx1, by1 = b[:4]
    overlap = min(ay1, by1) - max(ay0, by0)
    return bx0 > ax1 - 1 and overlap > 0.5 * min(ay1 - ay0, by1 - by0)


def merge_line(a, b):
    ax0, ay0, ax1, ay1, atxt, asize, afonts = a
    bx0, by0, bx1, by1, btxt, bsize, bfonts = b
    return (ax0, min(ay0, by0), bx1, max(ay1, by1),
            atxt.rstrip() + " " + btxt.lstrip(), max(asize, bsize),
            afonts | bfonts)


def two_column(blocks, width):
    """
    Is this page set in two columns?  Then a left-column line and a
    right-column line share every baseline, and merging them would weave the
    two columns together word by word.  Two columns show as two populous
    line-start positions, one in each half of the page, with the left
    column's lines ending before the right column begins.
    """
    lines = [ln for vl in blocks for ln in vl]
    left = [ln for ln in lines if ln[0] < width * 0.45]
    right = [ln for ln in lines if ln[0] > width * 0.5]
    if len(left) < MIN_COL_LINES or len(right) < MIN_COL_LINES:
        return False
    rx0, n = collections.Counter(round(ln[0]) for ln in right).most_common(1)[0]
    if n < MIN_COL_LINES // 2:
        return False
    spanning = sum(1 for ln in left if ln[2] > rx0 + MARGIN_GAP)
    return spanning < len(left) // 4          # a heading or two may span both


def merge_across_blocks(blocks, width=None):
    """
    A scanned page's OCR layer puts the cells of a small table, and a running
    head and its page number, in separate blocks: "T", "F", "T" each on a line
    of their own.  Merge same-baseline lines across blocks so that a table
    row reads as a row and "Assertion" + "79" reads as one running head.
    ``blocks`` is a list of visual-line lists; lines that moved are removed
    from their own block.  A two-column page is left alone.
    """
    if width is not None and two_column(blocks, width):
        return blocks
    flat = [(bi, li, ln) for bi, vl in enumerate(blocks)
            for li, ln in enumerate(vl)]
    flat.sort(key=lambda t: (t[2][1], t[2][0]))          # by y0, then x0
    done = set()
    for i, (bi, li, ln) in enumerate(flat):
        if (bi, li) in done:
            continue
        done.add((bi, li))
        row = [(bi, li, ln)]
        for bj, lj, lm in flat[i + 1:]:
            if (bj, lj) in done or bj == bi:
                continue
            if lm[1] > ln[3]:                             # below this line
                break
            if any(same_baseline(r[2], lm) or same_baseline(lm, r[2])
                   for r in row):
                row.append((bj, lj, lm))
                done.add((bj, lj))
        if len(row) > 1:
            row.sort(key=lambda t: t[2][0])
            merged = row[0][2]
            for _, _, lm in row[1:]:
                merged = merge_line(merged, lm)
            for bj, lj, _ in row:
                blocks[bj][lj] = None
            blocks[row[0][0]][row[0][1]] = merged   # sits where the leftmost cell was
    return [[ln for ln in vl if ln is not None] for vl in blocks]


def body_font(doc, pages=None):
    """The font most body-sized text is set in (None if it cannot be told)."""
    sizes, fonts = collections.Counter(), collections.Counter()
    for pno, page in enumerate(doc, 1):
        if pages is not None and pno not in pages:
            continue
        for b in page.get_text("dict")["blocks"]:
            for l in b.get("lines", []):
                for s in l["spans"]:
                    n = len(s["text"].strip())
                    if n:
                        sizes[round(s["size"])] += n
                        fonts[(round(s["size"]), s["font"].split("+")[-1])] += n
    if not sizes:
        return None
    modal = sizes.most_common(1)[0][0]
    cands = [(n, f) for (sz, f), n in fonts.items() if sz == modal]
    return max(cands)[1] if cands else None


def is_running_head(vl, page, body_font):
    """
    A running head or folio: a short block at the very top or bottom of the
    page, set in a font other than the body's or carrying a page number.
    """
    if len(vl) != 1:
        return False
    x0, y0, x1, y1, txt, size, fonts = vl[0]
    txt = txt.strip()
    h = page.rect.height
    top, bottom = y1 < 0.10 * h, y0 > 0.92 * h
    if not (top or bottom) or len(txt) > 60:
        return False
    if re.fullmatch(r"\d+", txt):
        return True
    has_number = re.search(r"(^|\s)\d{1,4}($|\s)", txt) is not None
    other_font = body_font is not None and body_font not in fonts
    return has_number or other_font


class Hyphens:
    """
    Decide from the document itself whether a line-end hyphen is a real one.

    "truth-|value" must stay "truth-value" while "asser-|tion" must become
    "assertion".  No dictionary is needed: the document usually uses the same
    word again somewhere unbroken.  A joined form seen anywhere wins; else a
    hyphenated form seen inside a line wins; else a short list of prefixes
    that are always hyphenated; else, when both halves are common words on
    their own, the hyphen is kept (and the log marks it "?" for a look).
    """
    KEEP_PREFIXES = {"non", "self", "anti", "quasi", "pseudo", "semi", "half",
                     "well", "ill", "one", "two", "three", "four", "n"}
    WORD = re.compile(r"[A-Za-z]+")
    HYPHENATED = re.compile(r"[A-Za-z]+(?:-[A-Za-z]+)+")

    def __init__(self, doc):
        self.plain, self.joined = collections.Counter(), collections.Counter()
        for page in doc:
            lines = page.get_text("text").splitlines()
            for i, line in enumerate(lines):
                line = line.strip()
                for w in self.HYPHENATED.findall(line):
                    self.joined[w.lower()] += 1
                words = [w.lower() for w in self.WORD.findall(line)]
                # a word broken at the line end is two fragments, not words
                if line.endswith("-"):
                    words = words[:-1]
                if i and lines[i - 1].rstrip().endswith("-"):
                    words = words[1:]
                self.plain.update(words)

    def keep(self, a, b):
        """Should a-|b stay hyphenated?  Returns (keep, certain)."""
        a, b = a.lower(), b.lower()
        if self.plain[a + b]:
            return False, True
        if self.joined[a + "-" + b]:
            return True, True
        if a in self.KEEP_PREFIXES or b[:1].isdigit():
            return True, True
        if (len(a) >= 3 and len(b) >= 3
                and self.plain[a] >= 3 and self.plain[b] >= 3):
            return True, False
        return False, True


HYPHENS = None   # a Hyphens instance once the document is open


def append(prev, nxt, hyphen_log):
    """Join a wrapped line onto the paragraph so far."""
    prev = prev.rstrip()
    if prev.endswith("-"):
        stem, tail = prev[:-1], nxt.lstrip()
        ma = re.search(r"([A-Za-z]+)$", stem)
        mb = re.match(r"([A-Za-z]+)", tail)
        if ma and mb and HYPHENS is not None:
            keep, certain = HYPHENS.keep(ma.group(1), mb.group(1))
            if keep:
                hyphen_log.append("{}-|{} -> kept {}-{}{}".format(
                    stem[-14:], tail[:14], ma.group(1), mb.group(1),
                    "" if certain else "   ?"))
                return prev + tail
        hyphen_log.append("{}-|{} -> {}{}".format(
            stem[-14:], tail[:14], stem[-14:], tail[:14]))
        return stem + tail
    return prev + " " + nxt.lstrip()


def load_subs(path):
    """
    A substitution table for the OCR errors of a particular scan: one
    "pattern<TAB>replacement" per line, Python regex syntax, applied in order
    to the finished text.  Blank lines and lines starting with # are ignored.
    """
    subs = []
    with open(path, encoding="utf-8") as fh:
        for n, line in enumerate(fh, 1):
            line = line.rstrip("\r\n")
            if not line.strip() or line.lstrip().startswith("#"):
                continue
            if "\t" not in line:
                sys.exit("--subs {} line {}: no TAB between pattern and "
                         "replacement".format(path, n))
            pat, rep = line.split("\t", 1)
            try:
                subs.append((re.compile(pat, re.M), rep))
            except re.error as e:
                sys.exit("--subs {} line {}: {}".format(path, n, e))
    return subs


def page_left(blocks, fallback):
    """
    Where this page's text block starts: the most common line x0 on the page.
    Measured per page rather than once for the document, because a two-sided
    book has one left edge on the recto pages and another on the verso.
    """
    counts = collections.Counter(round(ln[0]) for vl in blocks for ln in vl)
    return counts.most_common(1)[0][0] if counts else fallback


def extract_paragraphs(doc, geo, hyphen_log, pages=None,
                       repairs=None, glyph_log=None, bfont=None, head_log=None):
    # [page, text, first_x0, last_x1, block_right, cont_indent, size, page_left]
    paras = []
    for pno, page in enumerate(doc, 1):
        if pages is not None and pno not in pages:
            continue
        left_cut, right_cut, body_left, _ = geo[pno % 2]
        blocks = [visual_lines(b, repairs, glyph_log, (left_cut, right_cut))
                  for b in page.get_text("dict", sort=True)["blocks"]
                  if "lines" in b]
        blocks = merge_across_blocks([vl for vl in blocks if vl],
                                     page.rect.width)
        pitch = line_pitch(blocks)
        pleft = page_left(blocks, body_left)
        for vl in blocks:
            if not vl:
                continue
            if is_running_head(vl, page, bfont):
                if head_log is not None:
                    head_log.append((pno, vl[0][4].strip()))
                continue
            tail = vl[1:] if len(vl) > 1 else vl      # the continuation indent
            cont = continuation_indent(tail, pleft)
            right = max(v[2] for v in vl)
            cur, prev_y0 = None, None
            for x0, y0, x1, _, txt, size, _ in vl:
                wrapped = cur is not None and cur[3] >= right - FULL_TOL
                aligned = abs(x0 - cont) <= INDENT_TOL
                # a table row or a displayed line sits further below its
                # predecessor than wrapped text does
                spaced = (pitch is not None and prev_y0 is not None
                          and y0 - prev_y0 > PITCH_FACTOR * pitch)
                if wrapped and aligned and not spaced:
                    cur[1] = append(cur[1], txt, hyphen_log)
                    cur[3] = x1
                    cur[6] = max(cur[6], size)
                else:
                    if cur:
                        paras.append(cur)
                    cur = [pno, txt.strip(), x0, x1, right, cont, size, pleft]
                prev_y0 = y0
            if cur:
                paras.append(cur)
    return paras


def continuation_indent(tail, body_left=None):
    """
    The x at which a block's wrapped lines continue: the most common x0 of
    its non-first lines.  Two footnotes in one block can tie -- the second
    note's first line against the first note's wrapped line -- and the tie
    goes to the x nearest the text block's left edge, where wrapped text
    lives.
    """
    counts = collections.Counter(round(v[0]) for v in tail)
    best = max(counts.values())
    tied = [x for x, n in counts.items() if n == best]
    if body_left is None or len(tied) == 1:
        return counts.most_common(1)[0][0]
    return min(tied, key=lambda x: abs(x - body_left))


def line_pitch(blocks):
    """The page's usual baseline-to-baseline distance, or None."""
    diffs = collections.Counter()
    for vl in blocks:
        for a, b in zip(vl, vl[1:]):
            d = round(b[1] - a[1])
            if 0 < d < 40:
                diffs[d] += 1
    return diffs.most_common(1)[0][0] if diffs else None


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
    body size, footnotes visibly smaller.

    The drop ends at the next heading -- a paragraph set larger than the body
    -- so that in a volume of chapters each chapter's bibliography goes and
    the chapter after it stays.  There may be several.  Returns (kept, dropped).
    """
    kept, refs, in_refs = [], [], False
    for p in paras:
        text, size = p[1].strip(), p[6]
        if in_refs and size > modal + 1.0:      # the next heading
            in_refs = False
        if REF_HEADING.match(text):
            in_refs = True
        if not in_refs or size < modal - 1.0:   # body, or a footnote
            kept.append(p)
        else:
            refs.append(p)
    return kept, refs


def join_across_pages(paras, hyphen_log):
    """
    Rejoin a body paragraph broken by a page break.  Body is identified by the
    document's modal font size, which keeps footnotes (smaller) out of it; the
    two halves may have footnote blocks emitted between them.  The second half
    must start at its own page's left edge (recorded on the paragraph, since
    recto and verso pages differ): a one-line block such as a table row or a
    heading trivially matches its own indent.
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
        if pa[3] >= pa[4] - FULL_TOL and abs(pb[2] - pb[7]) <= INDENT_TOL:
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
    ap.add_argument("args", nargs="*", metavar="PDF [PAGES] [OUT.TXT]",
                    help="the master PDF (normally supplied by Emacs), "
                         "optionally a page spec such as 23-29 or 2-10,17, "
                         "and optionally the output file.  Anything that looks "
                         "like a page spec is one; the other name is the "
                         "output.  Both may also be given after the options")
    ap.add_argument("--pages", metavar="SPEC", default=None,
                    help="convert only these pages, e.g. 2-10,17 (open ranges "
                         "allowed: 12- , -4).  Default: all")
    ap.add_argument("--page-markers", action="store_true",
                    help="insert a [p. N] line at the start of each page")
    ap.add_argument("--keep-math-unicode", action="store_true",
                    help="keep U+1D4xx math italics instead of folding to ASCII")
    ap.add_argument("--keep-margins", action="store_true",
                    help="do NOT clip the marginal revision notes")
    ap.add_argument("--keep-refs", action="store_true",
                    help="keep the bibliography (dropped by default)")
    ap.add_argument("--keep-heads", action="store_true",
                    help="do NOT drop running heads and page numbers")
    ap.add_argument("--subs", metavar="FILE", default=None,
                    help="apply this table of regex substitutions "
                         "(pattern<TAB>replacement per line) to the result -- "
                         "for the OCR errors of a particular scan")
    a = ap.parse_intermixed_args()   # so 'out.txt' may follow the flags

    if not a.args:
        ap.print_help()
        sys.exit(1)

    try:
        pdf, pagespec, txt = split_positionals(a.args)
    except ValueError as e:
        sys.exit("{}\n(usage: pdf2txt-ysb.py paper.pdf [23-29] [out.txt])".format(e))
    if a.pages is not None:
        if pagespec is not None:
            sys.exit("page range given twice: {!r} and --pages {!r}".format(
                pagespec, a.pages))
        pagespec = a.pages

    if sys.stdout.encoding and sys.stdout.encoding.lower() != "utf-8":
        try:
            sys.stdout.reconfigure(encoding="utf-8")
        except Exception:
            pass

    out_path = txt or re.sub(r"\.pdf$", "", pdf, flags=re.I) + ".txt"

    doc = pymupdf.open(pdf)

    pages = None
    if pagespec is not None:
        try:
            pages = parse_pages(pagespec, len(doc))
        except ValueError as e:
            sys.exit("pages: {}".format(e))

    # The margin column is a property of the whole document, so it is measured
    # over every page even when only a few are converted -- a short selection
    # (a table, a title page) would not carry enough justified lines to locate
    # the text block reliably on its own.
    pw, ph = page_size(doc)
    geo = detect_margins(doc)
    if a.keep_margins:
        geo = {k: (None, None, v[2], v[3]) for k, v in geo.items()}

    print("{}: {} pages, {:.0f}x{:.0f}pt".format(pdf, len(doc), pw, ph))
    odd = [n for n, p in enumerate(doc, 1)
           if (round(p.rect.width, 1), round(p.rect.height, 1)) != (pw, ph)]
    if odd:
        print("  note: {} page(s) are a different size ({}{}) -- geometry is "
              "taken from the other {}".format(
                  len(odd), ", ".join("p{}".format(n) for n in odd[:6]),
                  ", ..." if len(odd) > 6 else "", len(doc) - len(odd)))
    for parity, label in ((1, "odd pages"), (0, "even pages")):
        if parity == 0 and geo[0] == geo[1]:
            continue
        lc, rc, bl, br = geo[parity]
        which = "odd and even pages" if geo[0] == geo[1] else label
        cuts = ", ".join(s for s in (
            "left of x{:.0f}".format(lc) if lc is not None else "",
            "right of x{:.0f}".format(rc) if rc is not None else "") if s)
        print("  text block x {}..{}pt ({}) -> {}".format(
            bl, br, which,
            "dropping lines " + cuts if cuts else "no marginal column"))
    if pages is not None:
        print("  converting page(s) {} of {}".format(
            describe_pages(pages), len(doc)))

    dropped = dropped_margin_text(doc, geo, pages)
    if dropped:
        print("  dropped {} marginal line(s) -- check these are all notes:".format(
            len(dropped)))
        for pno, x0, txt in dropped:
            print("      p{:<3} x{:<4} {}".format(pno, x0, txt))

    repairs, notes = glyph_repairs(doc)
    for note in notes:
        print("  glyph recovery unavailable -- {}".format(note))

    global HYPHENS
    HYPHENS = Hyphens(doc)          # word statistics from the whole document

    bfont = None if a.keep_heads else body_font(doc, pages)
    if bfont:
        print("  body font: {}".format(bfont))

    hyphen_log = []
    glyph_log = {}
    head_log = [] if not a.keep_heads else None
    paras = extract_paragraphs(doc, geo, hyphen_log, pages,
                               repairs, glyph_log,
                               bfont=bfont, head_log=head_log)

    if head_log:
        print("  dropped {} running head(s) / page number(s):".format(
            len(head_log)))
        for pno, txt in head_log:
            print("      p{:<3} {}".format(pno, txt))

    if not paras:
        sys.exit("  no text found on the selected page(s) -- nothing written")

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

    if a.subs:
        hits = []
        for pat, rep in load_subs(a.subs):
            text, n = pat.subn(rep, text)
            if n:
                hits.append((pat.pattern, n))
        print("  applied {} substitution(s) from {}".format(
            sum(n for _, n in hits), a.subs))
        for pat, n in hits:
            print("      {:<40} x{}".format(pat[:40], n))

    with open(out_path, "w", encoding="utf-8", newline="\r\n") as fh:
        fh.write(text)

    print("  paragraphs {} | cross-page joins {} | de-hyphenated {}".format(
        len(paras), njoin, len(hyphen_log)))
    for h in hyphen_log:
        print("      " + h)

    if glyph_log:
        fixed = {k: n for k, n in glyph_log.items() if k[2] is not None}
        lost = {k: n for k, n in glyph_log.items() if k[2] is None}
        if fixed:
            print("  recovered {} unmapped glyph(s) from the embedded fonts:"
                  .format(sum(fixed.values())))
            for (font, code, ch), n in sorted(fixed.items(),
                                              key=lambda kv: -kv[1]):
                print("      {:12} code {:<3} -> {}  x{}".format(
                    font, code, ch, n))
        if lost:
            print("  {} glyph(s) still unmapped -- these are lost:".format(
                sum(lost.values())))
            for (font, code, _), n in sorted(lost.items(), key=lambda kv: -kv[1]):
                print("      {:12} code {:<3} x{}".format(font, code, n))
    print("  wrote {}  ({:,} chars, {:,} words)".format(
        out_path, len(text), len(text.split())))


if __name__ == "__main__":
    main()
