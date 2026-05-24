#!/usr/bin/env python3
"""
Place a signature image on a named line of a thesis approval / title page.

The script searches the PDF for a printed name (the "anchor") and drops the
signature image just above it, so placement follows that specific line even
if the page layout or paper size changes. By default it signs the advisor
line, "Yehezkel Berkovski".

The signature PNG may have a solid background; near-white pixels are made
transparent before placing so no white box appears over the page.

Examples:
    # Use the built-in defaults (approval.pdf -> approval-signed.pdf)
    python sign_approval.py

    # Spell out the files
    python sign_approval.py -i onaylanan.pdf -o onaylanan-signed.pdf -s signature.png

    # Make the signature a little bigger
    python sign_approval.py --height 40

    # Nudge placement: 12 pt right of the name, 4 pt above it
    python sign_approval.py --x-offset 12 --gap 4

Requires: pymupdf, pillow
    pip install pymupdf pillow
"""

import argparse
import io
import sys
from pathlib import Path

import fitz                      # PyMuPDF
from PIL import Image


def white_to_transparent(path, thresh):
    """Return (PNG bytes, aspect ratio) with near-white pixels made transparent.

    A pixel becomes fully transparent when each of its R, G and B channels is
    at or above `thresh` (0-255). Lower the threshold if faint light-gray
    strokes are disappearing.
    """
    im = Image.open(path).convert("RGBA")
    px = list(im.getdata())
    out = [
        (r, g, b, 0) if (r >= thresh and g >= thresh and b >= thresh)
        else (r, g, b, a)
        for r, g, b, a in px
    ]
    im.putdata(out)
    buf = io.BytesIO()
    im.save(buf, format="PNG")
    return buf.getvalue(), im.width / im.height


def parse_args(argv=None):
    p = argparse.ArgumentParser(
        description="Place a signature image just above a named line on a PDF.",
        epilog="Placement is anchored to the printed name, not fixed "
               "coordinates, so it adapts to A4 vs Letter and to layout shifts.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("-i", "--input", default="approval.pdf",
                   help="source PDF to sign")
    p.add_argument("-o", "--output", default=None,
                   help="path for the signed PDF (default: the input name with "
                        "'-signed' inserted before the extension)")
    p.add_argument("-s", "--signature", default="signature.png",
                   help="signature image (PNG)")
    p.add_argument("-a", "--anchor", default="Yehezkel Berkovski",
                   help="printed name to place the signature above")
    p.add_argument("--height", type=float, default=33.0,
                   help="signature height on the page, in points "
                        "(width follows the image aspect ratio)")
    p.add_argument("--x-offset", type=float, default=8.0,
                   help="points to the right of the name's left edge")
    p.add_argument("--gap", type=float, default=6.0,
                   help="points between the signature bottom and the name top")
    p.add_argument("--threshold", type=int, default=240,
                   help="white-to-transparent cutoff, 0-255; lower it if light "
                        "strokes vanish")

    if argv is None:
        argv = sys.argv[1:]
    if not argv:
        p.print_help()
        sys.exit(0)

    return p.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)

    if args.output is None:
        p = Path(args.input)
        args.output = str(p.with_name(f"{p.stem}-signed{p.suffix}"))

    sig_bytes, aspect = white_to_transparent(args.signature, args.threshold)

    doc = fitz.open(args.input)
    placed = False

    for page in doc:
        hits = page.search_for(args.anchor)
        if not hits:
            continue
        name_rect = hits[0]            # bounding box of the printed name

        h = args.height
        w = h * aspect
        x0 = name_rect.x0 + args.x_offset
        y1 = name_rect.y0 - args.gap   # bottom of signature, just above name
        y0 = y1 - h
        target = fitz.Rect(x0, y0, x0 + w, y1)

        page.insert_image(target, stream=sig_bytes, keep_proportion=True,
                          overlay=True)
        placed = True
        print(f"Placed signature above '{args.anchor}' on page "
              f"{page.number + 1} at {target}")
        break

    if not placed:
        sys.exit(f"Could not find '{args.anchor}' in {args.input}.")

    doc.save(args.output, deflate=True)
    print(f"Saved: {args.output}")


if __name__ == "__main__":
    main()
