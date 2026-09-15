"""
Shrink (or grow) the page boxes of a PDF without re-encoding anything.

Meant for scans whose pages were built at 1 pixel = 1 point, so a
1958x3104 px scan became a 1958x3104 pt page (691x1095 mm). The image
data is fine; only the page geometry is wrong. This script scales every
page box and places the original page content (images, OCR text layer)
into it via a form XObject, so the streams are copied verbatim: no
recompression, no quality loss, searchable text stays searchable.
Bookmarks are carried over.

Usage:
    python rescale-pages.py big.pdf --dpi 300
    python rescale-pages.py big.pdf --factor 0.24 -o fixed.pdf
    python rescale-pages.py book.pdf --pages 1-8 --match 9 --factor 2.222
    python rescale-pages.py book.pdf --pages 1-8 --match 9 --fit

--dpi N      the true resolution of the scan. Pages scale by 72/N,
             so --dpi 300 turns a 1958 pt width into 470 pt (166 mm).
--factor F   explicit scale factor instead of --dpi.
--fit        derive the factor so the content fits the target box
             (see --match / --box), preserving its aspect ratio.
-o PATH      output path (default: <base>_resized.pdf).

Rescaling only part of a file
-----------------------------
--pages SPEC rescale only these pages (1-based), e.g. 1-8 or 1-8,20 or
             3,5,7 or 40- (open-ended). Everything else is copied
             verbatim with insert_pdf, so untouched pages keep their
             objects exactly. Default: every page.

--match N    give the rescaled pages the same page box as page N of the
             input, centring the scaled content in it. This is how you
             make odd-sized pages match the rest of the book:
                 --pages 1-8 --match 9
--box WxH    explicit target box in points instead of --match.

Without --match/--box the page box is just the scaled content, which is
the original behaviour.

Note that --fit fills the target box, so it only matches the *page*. If
the odd pages were cropped differently from the rest, their aspect ratio
will differ and filling the box makes the type come out the wrong size.
In that case measure the ratio of the printed type between the two groups
(line leading, or the width of full-justified lines) and pass that as
--factor, so the type ends up the same physical size.

Requires: pymupdf  (pip install pymupdf)
"""

import argparse
import os
import sys

try:
    import pymupdf
except ImportError:
    print("[!] pymupdf not installed. Run: pip install pymupdf")
    sys.exit(1)


def fmt_mm(pts):
    return f"{pts / 72 * 25.4:.0f}"


def parse_pages(spec, n):
    """Turn a spec like 1-8,20,25- into a set of 0-based page indices."""
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


def parse_box(spec):
    """Turn a spec like 483x696 into (483.0, 696.0)."""
    a, _, b = spec.lower().partition("x")
    return float(a), float(b)


def main():
    ap = argparse.ArgumentParser(
        description="Rescale PDF page boxes without re-encoding content.",
        epilog=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf", help="input PDF")
    ap.add_argument("--dpi", type=float,
                    help="true scan resolution; pages scale by 72/dpi")
    ap.add_argument("--factor", type=float,
                    help="explicit scale factor (overrides --dpi)")
    ap.add_argument("--fit", action="store_true",
                    help="derive the factor so content fits the target box")
    ap.add_argument("--pages", help="only rescale these pages, e.g. 1-8,20")
    ap.add_argument("--match", type=int, metavar="N",
                    help="target page box = page box of input page N")
    ap.add_argument("--box", metavar="WxH",
                    help="target page box in points, e.g. 483x696")
    ap.add_argument("-o", "--out", help="output path")

    # Bare invocation: show the help instead of an argparse error.
    if len(sys.argv) == 1:
        ap.print_help()
        return

    args = ap.parse_args()

    if not (args.factor or args.dpi or args.fit):
        ap.error("give --factor F, --dpi N, or --fit")
    if args.fit and not (args.match or args.box):
        ap.error("--fit needs a target box: give --match N or --box WxH")
    if args.match and args.box:
        ap.error("give either --match or --box, not both")

    if not os.path.exists(args.pdf):
        print(f"[!] File not found: {args.pdf}")
        sys.exit(1)

    base, ext = os.path.splitext(args.pdf)
    out_path = args.out or (base + "_resized" + ext)
    if os.path.abspath(out_path) == os.path.abspath(args.pdf):
        print("[!] Output path equals input path; pick a different -o.")
        sys.exit(1)

    src = pymupdf.open(args.pdf)
    n = src.page_count

    try:
        todo = parse_pages(args.pages, n) if args.pages else set(range(n))
    except ValueError as e:
        print(f"[!] {e}")
        sys.exit(1)
    if not todo:
        print("[!] --pages selected no pages.")
        sys.exit(1)

    # Target page box for the rescaled pages, if one was requested.
    target = None
    if args.match:
        if not 1 <= args.match <= n:
            print(f"[!] --match {args.match} out of range "
                  f"(file has {n} pages)")
            sys.exit(1)
        r = src[args.match - 1].rect
        target = (r.width, r.height)
    elif args.box:
        target = parse_box(args.box)

    # Scale factor.
    if args.factor:
        factor = args.factor
    elif args.dpi:
        factor = 72.0 / args.dpi
    else:  # --fit: largest factor that keeps every selected page in the box
        tw, th = target
        factor = min(min(tw / src[i].rect.width, th / src[i].rect.height)
                     for i in todo)

    dst = pymupdf.open()

    scope = "all pages" if len(todo) == n else f"{len(todo)} of {n} pages"
    print(f"--- scaling {scope} by {factor:.4f} ---")
    if target:
        print(f"  target page box: {target[0]:.0f}x{target[1]:.0f} pt "
              f"({fmt_mm(target[0])}x{fmt_mm(target[1])} mm)")

    shown = False
    i = 0
    while i < n:
        if i in todo:
            r = src[i].rect  # rect already accounts for page rotation
            w, h = r.width * factor, r.height * factor
            pw, ph = target if target else (w, h)
            page = dst.new_page(width=pw, height=ph)
            x, y = (pw - w) / 2, (ph - h) / 2
            page.show_pdf_page(pymupdf.Rect(x, y, x + w, y + h), src, i)
            if not shown:
                print(f"  page {i+1}: {r.width:.0f}x{r.height:.0f} pt "
                      f"({fmt_mm(r.width)}x{fmt_mm(r.height)} mm)  ->  "
                      f"content {w:.0f}x{h:.0f} pt "
                      f"on a {pw:.0f}x{ph:.0f} pt page "
                      f"({fmt_mm(pw)}x{fmt_mm(ph)} mm)")
                if target and (w < pw - 1 or h < ph - 1):
                    print(f"          centred, margins {x:.1f} x {y:.1f} pt")
                shown = True
            i += 1
        else:
            # Copy the run of untouched pages in one go, objects verbatim.
            j = i
            while j < n and j not in todo:
                j += 1
            dst.insert_pdf(src, from_page=i, to_page=j - 1)
            print(f"  pages {i+1}-{j} copied verbatim")
            i = j

    toc = src.get_toc()
    if toc:
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
