"""
Auto-crop a PDF's margins by shrinking each page's CropBox to the
detected content area.

Each page is rendered at low resolution and scanned for dark pixels.
The bounding box of those pixels, plus a padding, becomes the new
CropBox. Nothing is rasterized in the output and no content is
deleted: the full page stays in the file, viewers just display the
cropped area. Any OCR text layer is left untouched. The crop is
reversible (reset the CropBox to the MediaBox to undo it).

Usage:
    python auto-crop.py book.pdf
    python auto-crop.py book.pdf --dry-run
    python auto-crop.py book.pdf --pad 10 --uniform
    python auto-crop.py book.pdf --oddeven --threshold 100
    python auto-crop.py book.pdf --pages 13-250 --out cropped.pdf

Flags:
    --pad N        padding in points around the content box (default 5)
    --threshold N  pixels darker than N (0-255) count as content
                   (default 128). If the scan background is gray and
                   no crop happens, the background is being counted as
                   content: lower N below the background's gray value.
    --dpi N        render resolution for detection (default 72). Raise
                   it if thin lines are missed.
    --from-text    take the content box from the text layer (the union
                   of the word boxes) instead of from dark pixels.
                   Needs an OCR'd or born-digital PDF, and is the right
                   choice for scans with black gutter stripes or dirty
                   edges, which pixel detection reads as content and so
                   refuses to crop. Pages with no text fall back to
                   pixel detection.
    --axis X       crop only one axis: 'x' keeps the full page height
                   and trims left/right, 'y' keeps the full width and
                   trims top/bottom, 'both' is the default. Use 'x' on
                   a scan already tight at top and bottom.
    --pages SPEC   only crop these pages, e.g. 1-10,15 (default: all)
    --uniform      one crop box for the whole file: the union of all
                   per-page boxes. Keeps every page the same size.
    --oddeven      like --uniform, but odd and even pages each get
                   their own box. For scans where recto and verso are
                   shifted relative to each other.
    --dry-run      print the detected boxes, write nothing
    --out PATH     output path (default: <base>_cropped.pdf)

Blank pages (no dark pixels) are left uncropped in per-page mode. In
--uniform / --oddeven mode they get the shared box, so page size stays
consistent.

Requires: pymupdf  (pip install pymupdf)
"""

import os
import sys
import pymupdf


def parse_pages(spec, n_pages):
    """Parse '1-10,15' into a sorted list of 1-based page numbers."""
    pages = set()
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            a, b = part.split("-", 1)
            pages.update(range(int(a), int(b) + 1))
        else:
            pages.add(int(part))
    return sorted(p for p in pages if 1 <= p <= n_pages)


def content_bbox(page, zoom, threshold):
    """Bounding box of pixels darker than threshold, in displayed page
    coordinates (the system of page.rect). None if the page is blank."""
    pix = page.get_pixmap(
        matrix=pymupdf.Matrix(zoom, zoom),
        colorspace=pymupdf.csGRAY,
        alpha=False,
    )
    w, h, stride, s = pix.width, pix.height, pix.stride, pix.samples
    # Map dark bytes to 0x01 and light bytes to 0x00, so find/rfind do
    # the per-row scanning at C speed.
    table = bytes(1 if v < threshold else 0 for v in range(256))
    r0 = r1 = None
    c0, c1 = w, -1
    for r in range(h):
        row = bytes(s[r * stride : r * stride + w]).translate(table)
        i = row.find(b"\x01")
        if i == -1:
            continue
        j = row.rfind(b"\x01")
        if r0 is None:
            r0 = r
        r1 = r
        if i < c0:
            c0 = i
        if j > c1:
            c1 = j
    if r0 is None:
        return None
    return pymupdf.Rect(
        page.rect.x0 + c0 / zoom,
        page.rect.y0 + r0 / zoom,
        page.rect.x0 + (c1 + 1) / zoom,
        page.rect.y0 + (r1 + 1) / zoom,
    )


def text_bbox(page):
    """Bounding box of the page's text, in displayed page coordinates.

    None if the page has no text layer. Immune to the black gutter
    stripes and dirty edges that pixel detection reads as content.
    """
    words = page.get_text("words")
    if not words:
        return None
    return pymupdf.Rect(
        min(w[0] for w in words),
        min(w[1] for w in words),
        max(w[2] for w in words),
        max(w[3] for w in words),
    )


def apply_crop(page, disp_rect):
    """Set the page's CropBox from a rect in displayed coordinates.

    Displayed coordinates are relative to the current CropBox and have
    the page rotation applied. set_cropbox wants unrotated coordinates
    with the CropBox offset included, so both are undone here.
    """
    r = pymupdf.Rect(disp_rect) & page.rect
    if r.is_empty:
        return False
    r = r * page.derotation_matrix
    r.normalize()
    cb = page.cropbox
    r = pymupdf.Rect(r.x0 + cb.x0, r.y0 + cb.y0, r.x1 + cb.x0, r.y1 + cb.y0)
    try:
        page.set_cropbox(r)
        return True
    except Exception as e:
        print(f"  [!] page {page.number + 1}: set_cropbox failed: {e}")
        return False


def main():
    argv = sys.argv
    if len(argv) < 2 or "-h" in argv or "--help" in argv:
        print(__doc__)
        sys.exit(0 if ("-h" in argv or "--help" in argv) else 1)

    pdf_path = argv[1]
    if not os.path.exists(pdf_path):
        print(f"[!] File not found: {pdf_path}")
        sys.exit(1)

    pad = 5.0
    threshold = 128
    dpi = 72
    pages_spec = None
    uniform = "--uniform" in argv
    oddeven = "--oddeven" in argv
    dry_run = "--dry-run" in argv
    from_text = "--from-text" in argv
    axis = "both"
    out_path = None

    flagless = {"--uniform", "--oddeven", "--dry-run", "--from-text"}
    rest = argv[2:]
    i = 0
    while i < len(rest):
        arg = rest[i]
        if arg in flagless:
            i += 1
            continue
        if arg.startswith("--"):
            if "=" in arg:
                key, _, value = arg.partition("=")
                step = 1
            else:
                key = arg
                value = rest[i + 1] if i + 1 < len(rest) else None
                step = 2
            if value is None:
                print(f"[!] {key} needs a value")
                sys.exit(1)
            if key == "--pad":
                pad = float(value)
            elif key == "--threshold":
                threshold = int(value)
            elif key == "--dpi":
                dpi = int(value)
            elif key == "--pages":
                pages_spec = value
            elif key == "--axis":
                axis = value.lower()
                if axis not in ("x", "y", "both"):
                    print(f"[!] --axis must be x, y, or both, got {value}")
                    sys.exit(1)
            elif key == "--out":
                out_path = value
            else:
                print(f"[!] Unknown flag: {key}")
                sys.exit(1)
            i += step
            continue
        print(f"[!] Unrecognized argument: {arg}")
        sys.exit(1)

    if not 0 <= threshold <= 255:
        print(f"[!] --threshold must be 0-255, got {threshold}")
        sys.exit(1)
    if uniform and oddeven:
        print("[!] Use either --uniform or --oddeven, not both.")
        sys.exit(1)

    doc = pymupdf.open(pdf_path)
    n = doc.page_count
    page_list = parse_pages(pages_spec, n) if pages_spec else list(range(1, n + 1))
    if not page_list:
        print("[!] Page selection is empty.")
        doc.close()
        sys.exit(1)

    zoom = dpi / 72.0

    # Pass 1: detect content boxes (in displayed coordinates, padded).
    source = "text layer" if from_text else f"threshold {threshold}, dpi {dpi}"
    print(f"--- detecting content on {len(page_list)} page(s) "
          f"({source}, pad {pad:g}pt, axis {axis}) ---")
    boxes = {}
    fellback = []
    for p in page_list:
        page = doc[p - 1]
        bb = text_bbox(page) if from_text else None
        if from_text and bb is None:
            fellback.append(p)
        if bb is None:
            bb = content_bbox(page, zoom, threshold)
        if bb is not None:
            bb = pymupdf.Rect(bb.x0 - pad, bb.y0 - pad, bb.x1 + pad, bb.y1 + pad)
            if axis == "x":
                bb.y0, bb.y1 = page.rect.y0, page.rect.y1
            elif axis == "y":
                bb.x0, bb.x1 = page.rect.x0, page.rect.x1
            bb = bb & page.rect
        boxes[p] = bb

    if fellback:
        more = " ..." if len(fellback) > 12 else ""
        print(f"  no text layer on {len(fellback)} page(s), detected by "
              f"pixel instead: {fellback[:12]}{more}")

    blanks = [p for p, bb in boxes.items() if bb is None]
    if len(blanks) == len(page_list):
        print("[!] No content found on any selected page. The threshold is "
              "probably below the darkness of the actual content. Try a "
              "higher --threshold or a higher --dpi.")
        doc.close()
        sys.exit(1)

    # In uniform modes, replace per-page boxes with union boxes.
    if uniform or oddeven:
        groups = {}
        for p, bb in boxes.items():
            if bb is None:
                continue
            key = (p % 2) if oddeven else 0
            groups[key] = bb if key not in groups else (groups[key] | bb)
        for p in boxes:
            boxes[p] = groups.get((p % 2) if oddeven else 0)

    # Report and apply.
    cropped = skipped = 0
    for p in page_list:
        page = doc[p - 1]
        bb = boxes[p]
        old = page.rect
        if bb is None or bb.is_empty:
            print(f"  page {p:>4}: blank, left uncropped")
            skipped += 1
            continue
        print(f"  page {p:>4}: {old.width:6.1f} x {old.height:6.1f}"
              f"  ->  {bb.width:6.1f} x {bb.height:6.1f}"
              f"   box ({bb.x0:.1f}, {bb.y0:.1f}, {bb.x1:.1f}, {bb.y1:.1f})")
        if not dry_run:
            if apply_crop(page, bb):
                cropped += 1
        else:
            cropped += 1

    if dry_run:
        print(f"\n[dry-run] would crop {cropped} page(s), skip {skipped}. "
              "Nothing written.")
        doc.close()
        return

    if cropped == 0:
        print("[!] Nothing was cropped; no output written.")
        doc.close()
        return

    if out_path is None:
        base, ext = os.path.splitext(pdf_path)
        out_path = base + "_cropped" + ext
    doc.save(out_path)
    doc.close()
    size_mb = os.path.getsize(out_path) / 1024 / 1024
    print(f"\n  -> cropped {cropped} page(s), skipped {skipped}")
    print(f"  -> wrote {out_path} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    main()
