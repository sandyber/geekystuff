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

--dpi N      the true resolution of the scan. Pages scale by 72/N,
             so --dpi 300 turns a 1958 pt width into 470 pt (166 mm).
--factor F   explicit scale factor instead of --dpi.
-o PATH      output path (default: <base>_resized.pdf).

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
    ap.add_argument("-o", "--out", help="output path")
    args = ap.parse_args()

    if args.factor:
        factor = args.factor
    elif args.dpi:
        factor = 72.0 / args.dpi
    else:
        ap.error("give --dpi N or --factor F")

    if not os.path.exists(args.pdf):
        print(f"[!] File not found: {args.pdf}")
        sys.exit(1)

    base, ext = os.path.splitext(args.pdf)
    out_path = args.out or (base + "_resized" + ext)
    if os.path.abspath(out_path) == os.path.abspath(args.pdf):
        print("[!] Output path equals input path; pick a different -o.")
        sys.exit(1)

    src = pymupdf.open(args.pdf)
    dst = pymupdf.open()

    r0 = src[0].rect
    print(f"--- scaling {src.page_count} pages by {factor:.4f} ---")
    print(f"  page 1: {r0.width:.0f}x{r0.height:.0f} pt "
          f"({fmt_mm(r0.width)}x{fmt_mm(r0.height)} mm)  ->  "
          f"{r0.width * factor:.0f}x{r0.height * factor:.0f} pt "
          f"({fmt_mm(r0.width * factor)}x{fmt_mm(r0.height * factor)} mm)")

    for page in src:
        r = page.rect  # rect already accounts for page rotation
        new_page = dst.new_page(width=r.width * factor,
                                height=r.height * factor)
        new_page.show_pdf_page(new_page.rect, src, page.number)

    toc = src.get_toc()
    if toc:
        dst.set_toc(toc)
        print(f"  copied {len(toc)} bookmark(s)")

    dst.save(out_path, garbage=4, deflate=True)
    dst.close()
    src.close()

    size_mb = os.path.getsize(out_path) / 1024 / 1024
    print(f"  -> {out_path} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    main()
