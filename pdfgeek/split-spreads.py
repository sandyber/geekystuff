"""
Split two-page spreads in a scanned PDF into single pages.

Scanners often capture a book opening as one double-wide sheet. This
cuts such pages in two, in reading order, so every output page is a
single book page.

Nothing is re-encoded: each half is placed via a form XObject that clips
the original page, so the image streams are copied verbatim and any OCR
text layer stays searchable. Pages that are not split are copied object
for object with insert_pdf. Bookmarks are carried over and their page
numbers shifted to account for the inserted pages.

Usage:
    python split-spreads.py book.pdf --dry-run
    python split-spreads.py book.pdf
    python split-spreads.py book.pdf --pages 1-210 --auto-gutter
    python split-spreads.py book.pdf --pages 293,306 --at 0.5
    python split-spreads.py arabic.pdf --rtl

Which pages
-----------
--pages SPEC split exactly these pages (1-based), e.g. 293,306 or 1-210
             or 40- (open-ended). Default: auto-detect, see --ratio.
--ratio R    auto-detect threshold: split every page at least R times as
             wide as the median page width (default 1.5). This finds a
             few stray spreads in an otherwise single-page file. It
             finds nothing when EVERY page is a spread, because then the
             median is the spread width - pass --pages 1-N for that.

Where to cut
------------
--at FRAC    cut at this fraction of the page width (default 0.5).
--auto-gutter
             find the gutter on each page instead: the widest band of
             blank columns near the middle, cut through its centre.
             Scans are rarely centred, and a blind 0.5 cut then shaves
             the inner margin off one page and glues it to the other.
             Falls back to --at on any page with no clear gutter.
--band LO HI search window for --auto-gutter, as fractions of the width
             (default 0.35 0.65).
--ink N      a column counts as blank if fewer than N pixels are dark
             (default 2). Raise it for speckled scans.

--auto-gutter gives each half its natural width, so output pages vary in
size by however much the scans wander. Follow with rescale-pages.py to
put them all on one page box:
    python rescale-pages.py out.pdf --factor 1.0 --box 420x612

--rtl        right-hand half first, for right-to-left books.
--dry-run    report what would be split, and where, then stop.
-o PATH      output path (default: <base>_split.pdf).

Requires: pymupdf  (pip install pymupdf)
          numpy    (only for --auto-gutter)
"""

import argparse
import os
import statistics
import sys

try:
    import pymupdf
except ImportError:
    print("[!] pymupdf not installed. Run: pip install pymupdf")
    sys.exit(1)


def fmt_mm(pts):
    return f"{pts / 72 * 25.4:.0f}"


def parse_pages(spec, n):
    """Turn a spec like 293,306 or 1-210 into a set of 0-based indices."""
    out = set()
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            a, _, b = part.partition("-")
            start = int(a) if a.strip() else 1
            end = int(b) if b.strip() else n
        else:
            start = end = int(part)
        if start < 1 or end > n or start > end:
            raise ValueError(
                f"page range out of bounds: {part} (file has {n} pages)")
        out.update(range(start - 1, end))
    return out


def detect_spreads(src, ratio):
    """Pages at least `ratio` times as wide as the median page width."""
    widths = [p.rect.width for p in src]
    median = statistics.median(widths)
    return {i for i, w in enumerate(widths) if w >= median * ratio}, median


def find_gutter(page, band, ink_threshold, dpi=60):
    """Fraction of the page width where the gutter is, or None.

    Renders the page small and grey, counts dark pixels per column, and
    takes the centre of the widest blank run inside the search band.
    """
    import numpy as np

    pix = page.get_pixmap(dpi=dpi, colorspace=pymupdf.csGRAY)
    a = np.frombuffer(pix.samples, dtype=np.uint8)
    a = a.reshape(pix.height, pix.stride)[:, :pix.width]
    ink = (a < 160).sum(axis=0)

    lo, hi = int(pix.width * band[0]), int(pix.width * band[1])
    blank = ink[lo:hi] < ink_threshold
    if not blank.any():
        return None

    best_len = best_end = 0
    run = 0
    for j, b in enumerate(blank):
        run = run + 1 if b else 0
        if run > best_len:
            best_len, best_end = run, j
    if best_len < 2:
        return None
    centre = lo + best_end - best_len / 2 + 0.5
    return centre / pix.width


def main():
    ap = argparse.ArgumentParser(
        description="Split double-wide spreads into single pages.",
        epilog=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf", help="input PDF")
    ap.add_argument("--pages", help="split exactly these pages, e.g. 1-210")
    ap.add_argument("--ratio", type=float, default=1.5,
                    help="auto-detect width threshold (default 1.5)")
    ap.add_argument("--at", type=float, default=0.5, metavar="FRAC",
                    help="cut at this fraction of the width (default 0.5)")
    ap.add_argument("--auto-gutter", action="store_true",
                    help="find the gutter on each page and cut there")
    ap.add_argument("--band", type=float, nargs=2, default=(0.35, 0.65),
                    metavar=("LO", "HI"),
                    help="gutter search window (default 0.35 0.65)")
    ap.add_argument("--ink", type=int, default=2, metavar="N",
                    help="dark pixels for a column to count as inked")
    ap.add_argument("--rtl", action="store_true",
                    help="right half first (right-to-left books)")
    ap.add_argument("--dry-run", action="store_true",
                    help="report what would be split and stop")
    ap.add_argument("-o", "--out", help="output path")

    # Bare invocation: show the help instead of an argparse error.
    if len(sys.argv) == 1:
        ap.print_help()
        return

    args = ap.parse_args()

    if not 0.05 < args.at < 0.95:
        ap.error("--at must be a fraction between 0.05 and 0.95")
    if not 0 <= args.band[0] < args.band[1] <= 1:
        ap.error("--band needs LO < HI, both within 0..1")

    if not os.path.exists(args.pdf):
        print(f"[!] File not found: {args.pdf}")
        sys.exit(1)

    base, ext = os.path.splitext(args.pdf)
    out_path = args.out or (base + "_split" + ext)
    if os.path.abspath(out_path) == os.path.abspath(args.pdf):
        print("[!] Output path equals input path; pick a different -o.")
        sys.exit(1)

    src = pymupdf.open(args.pdf)
    n = src.page_count

    if args.pages:
        try:
            todo = parse_pages(args.pages, n)
        except ValueError as e:
            print(f"[!] {e}")
            sys.exit(1)
        median = statistics.median(p.rect.width for p in src)
    else:
        todo, median = detect_spreads(src, args.ratio)

    print(f"--- {n} pages, median width {median:.0f} pt "
          f"({fmt_mm(median)} mm) ---")
    if not todo:
        print("  no spreads found; nothing to do")
        print(f"  (if EVERY page is a spread, say so: --pages 1-{n})")
        src.close()
        return

    # Where to cut each page.
    if args.auto_gutter:
        try:
            import numpy  # noqa: F401
        except ImportError:
            print("[!] --auto-gutter needs numpy. Run: pip install numpy")
            sys.exit(1)
        cut = {}
        missed = []
        for i in sorted(todo):
            f = find_gutter(src[i], args.band, args.ink)
            if f is None:
                missed.append(i + 1)
                f = args.at
            cut[i] = f
        fracs = sorted(cut.values())
        print(f"  gutter: median {statistics.median(fracs):.3f} of width, "
              f"range {fracs[0]:.3f}-{fracs[-1]:.3f}")
        if missed:
            more = " ..." if len(missed) > 12 else ""
            print(f"  no gutter found on {len(missed)} page(s), cut at "
                  f"{args.at}: {missed[:12]}{more}")
    else:
        cut = {i: args.at for i in todo}
        print(f"  cutting at {args.at:.3f} of the width")

    if len(todo) <= 12 or args.dry_run:
        for i in sorted(todo)[:60]:
            r = src[i].rect
            x = r.width * cut[i]
            print(f"  page {i+1}: {r.width:.0f}x{r.height:.0f} pt  ->  "
                  f"{x:.0f}x{r.height:.0f} + {r.width-x:.0f}x{r.height:.0f}"
                  f"  (cut at {cut[i]:.3f})")
        if args.dry_run and len(todo) > 60:
            print(f"  ... and {len(todo)-60} more")
    print(f"  {len(todo)} spread(s) -> {n + len(todo)} pages total")

    if args.dry_run:
        src.close()
        return

    dst = pymupdf.open()
    shift = {}  # old 0-based index -> new 0-based index of its first page

    i = 0
    while i < n:
        if i in todo:
            shift[i] = dst.page_count
            r = src[i].rect  # rect already accounts for page rotation
            x = r.width * cut[i]
            halves = [pymupdf.Rect(0, 0, x, r.height),
                      pymupdf.Rect(x, 0, r.width, r.height)]
            if args.rtl:
                halves.reverse()
            for clip in halves:
                page = dst.new_page(width=clip.width, height=clip.height)
                page.show_pdf_page(page.rect, src, i, clip=clip)
            i += 1
        else:
            # Copy the run of untouched pages in one go, objects verbatim.
            j = i
            while j < n and j not in todo:
                shift[j] = dst.page_count + (j - i)
                j += 1
            dst.insert_pdf(src, from_page=i, to_page=j - 1)
            i = j

    toc = src.get_toc()
    if toc:
        for entry in toc:
            old = entry[2] - 1
            if old in shift:
                entry[2] = shift[old] + 1
        dst.set_toc(toc)
        print(f"  copied {len(toc)} bookmark(s)")

    dst.set_metadata(src.metadata)
    dst.save(out_path, garbage=4, deflate=True)
    dst.close()
    src.close()

    size_mb = os.path.getsize(out_path) / 1024 / 1024
    print(f"  -> {out_path} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    main()
