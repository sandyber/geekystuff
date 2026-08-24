"""
Find and repair JBIG2 images that some PDF readers refuse to render.

Scanned books produced by commercial OCR shops (ABBYY, Luratech) store
the text layer as a JBIG2 bilevel image: the glyphs go in a symbol
dictionary and the page is a list of "put symbol N at x,y". It is a
wonderful format and it compresses a page of prose to about 8 kB.

It is also implemented badly almost everywhere. The usual symptom is a
single page that comes up blank, or as a grey rectangle, in one reader
while every other reader shows it fine. Apple's decoder (Preview, PDF
Expert, Books, anything on iOS/iPadOS built on PDFKit) is the one that
most often chokes; Chrome's pdfium and MuPDF are far more forgiving, so
"it works on my desktop" tells you nothing.

The classic trigger is a symbol count that is an exact power of two,
and 256 above all: the symbol ID needs ceil(log2(N)) = 8 bits, N no
longer fits in the byte a sloppy decoder kept the count in, and the
page decodes to nothing. --scan flags those pages.

The repair is to stop using JBIG2 on the offending page. The bitmap is
decoded and re-encoded as CCITT Group 4, which is the fax codec, which
every PDF reader has implemented correctly since 1993. G4 is maybe six
times bigger than JBIG2 -- call it 50 kB a page instead of 8 kB -- so
this is a per-page repair, not something to run over a whole book
unless you mean it. The pixels are identical, not merely similar: the
script decodes the result and compares it bit for bit against the
original, and refuses to write a file that fails that check.

Usage:
    python jbig2-fix.py book.pdf                     # scan, report, write nothing
    python jbig2-fix.py book.pdf --scan --verbose    # every page, not just the suspects
    python jbig2-fix.py book.pdf --fix 179
    python jbig2-fix.py book.pdf --fix 179,181,200-204 -o fixed.pdf
    python jbig2-fix.py book.pdf --fix-flagged       # every page --scan complained about
    python jbig2-fix.py book.pdf --fix all           # the whole book; check the size first

Flags:
    --scan          report only (the default when no --fix is given)
    --fix SPEC      pages to convert: 1-based, '179', '179,181,200-204',
                    or the word 'all'
    --fix-flagged   convert exactly the pages --scan flags as risky
    --verbose       with --scan, list every JBIG2 page, not just suspects
    --no-verify     skip the decode-and-compare pass (don't)
    -o PATH         output path (default: <base>_jbig2fix.pdf)

What --scan flags
-----------------
  high    the final symbol dictionary exports exactly 256 symbols, or
          any power of two >= 256. This is the byte-boundary bug and
          it is the one that actually bites.
  medium  a smaller power of two (32, 64, 128). The same class of
          off-by-one, less commonly fatal. Not converted by
          --fix-flagged unless you pass --include-medium.
  low     text region using Huffman tables or refinement coding. Rarer
          code paths, occasionally buggy. Reported, never auto-fixed.

A clean scan does not prove the file is fine -- it only means none of
the known-bad shapes are present. If a reader still drops a page, run
--fix on it anyway; the conversion is lossless and costs 40 kB.

Requires: pymupdf, pikepdf, pillow
"""

import argparse
import io
import os
import struct
import sys

import pikepdf
import pymupdf
from PIL import Image, TiffImagePlugin


# JBIG2 segment types we care to name. See ITU-T T.88 Table 34.
SEG_TYPES = {
    0: "symbol dict", 4: "text region", 6: "text region",
    7: "text region", 16: "pattern dict", 20: "halftone",
    22: "halftone", 23: "halftone", 36: "generic region",
    38: "generic region", 39: "generic region", 40: "refine region",
    42: "refine region", 43: "refine region", 48: "page info",
    49: "end of page", 50: "end of stripe", 51: "end of file",
    52: "profiles", 53: "tables", 62: "extension",
}

SYMBOL_DICT = 0
TEXT_REGIONS = (4, 6, 7)


def parse_pages(spec, n_pages):
    """Parse '1-10,15' into a sorted list of 1-based page numbers."""
    if spec.strip().lower() == "all":
        return list(range(1, n_pages + 1))
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


# --------------------------------------------------------------------------
# JBIG2 parsing. We only read segment headers and the fixed-size fields at
# the front of each segment's payload -- enough to learn how many symbols a
# page uses and which coding options it asked for. No arithmetic decoding.
# --------------------------------------------------------------------------

def parse_segments(data):
    """Walk the embedded-stream segment headers of a JBIG2 image.

    Returns [(segment_number, segment_type, payload_bytes), ...]. Stops
    quietly at the first malformed header, which is itself a finding: a
    truncated JBIG2 stream is another way to get a blank page.
    """
    segs, off = [], 0
    while off + 11 <= len(data):
        num = struct.unpack(">I", data[off:off + 4])[0]
        off += 4
        flags = data[off]
        off += 1
        stype = flags & 0x3F
        page_assoc_size = 4 if flags & 0x40 else 1

        # Referred-to segments: a 3-bit count, or a long form for >4 refs.
        rt = data[off]
        count = rt >> 5
        if count == 7:
            count = struct.unpack(">I", data[off:off + 4])[0] & 0x1FFFFFFF
            off += 4 + (count + 8) // 8
        else:
            off += 1

        # Reference size depends on this segment's own number (T.88 7.2.5).
        ref_size = 1 if num <= 256 else (2 if num <= 65536 else 4)
        off += count * ref_size
        off += page_assoc_size

        if off + 4 > len(data):
            break
        length = struct.unpack(">I", data[off:off + 4])[0]
        off += 4

        if length == 0xFFFFFFFF:      # unknown length, only legal at the end
            segs.append((num, stype, data[off:]))
            break
        segs.append((num, stype, data[off:off + length]))
        off += length
    return segs


def read_symbol_dict(payload):
    """Fixed fields at the head of a symbol dictionary segment (T.88 7.4.3)."""
    flags = struct.unpack(">H", payload[0:2])[0]
    huff = flags & 1
    refagg = (flags >> 1) & 1
    template = (flags >> 10) & 3
    rtemplate = (flags >> 12) & 1
    off = 2
    if not huff:                      # AT pixels: 8 bytes for template 0
        off += 8 if template == 0 else 2
    if refagg and rtemplate == 0:     # refinement AT pixels
        off += 4
    n_ex, n_new = struct.unpack(">II", payload[off:off + 8])
    return dict(huff=huff, refagg=refagg, template=template,
                exported=n_ex, new=n_new)


def read_text_region(payload):
    """Fixed fields at the head of a text region segment (T.88 7.4.4)."""
    w, h, x, y = struct.unpack(">IIII", payload[0:16])
    flags = struct.unpack(">H", payload[17:19])[0]
    huff = flags & 1
    refine = (flags >> 1) & 1
    rtemplate = (flags >> 15) & 1
    off = 19
    if huff:
        off += 2                      # Huffman flags
    if refine and rtemplate == 0:
        off += 4                      # refinement AT pixels
    instances = struct.unpack(">I", payload[off:off + 4])[0]
    return dict(w=w, h=h, x=x, y=y, huff=huff, refine=refine,
                instances=instances)


def describe(data):
    """Summarise one JBIG2 stream: symbol counts, coding options, oddities.

    'symbols' is the count the text region will actually index, i.e. what
    the last symbol dictionary exported -- that is the number whose bit
    width the buggy decoders get wrong.
    """
    info = dict(symbols=0, instances=0, dicts=[], huff=False, refine=False,
                refagg=False, truncated=False, segments=[])
    try:
        segs = parse_segments(data)
    except (struct.error, IndexError):
        info["truncated"] = True
        return info
    for num, stype, payload in segs:
        info["segments"].append(SEG_TYPES.get(stype, str(stype)))
        try:
            if stype == SYMBOL_DICT:
                sd = read_symbol_dict(payload)
                info["dicts"].append(sd["exported"])
                info["symbols"] = sd["exported"]
                info["huff"] |= bool(sd["huff"])
                info["refagg"] |= bool(sd["refagg"])
            elif stype in TEXT_REGIONS:
                tr = read_text_region(payload)
                info["instances"] = tr["instances"]
                info["huff"] |= bool(tr["huff"])
                info["refine"] |= bool(tr["refine"])
        except (struct.error, IndexError):
            info["truncated"] = True
            break
    return info


def is_power_of_two(n):
    return n >= 2 and (n & (n - 1)) == 0


def assess(info, house_style=frozenset()):
    """Risk level and reason for one JBIG2 stream: (level, reason) or None.

    house_style names the low-risk traits that turned out to be everywhere
    in this particular file. A coding option every page uses is the encoder's
    habit, not that page's problem, and reporting it 472 times buries the one
    finding that matters.
    """
    n = info["symbols"]
    if info["truncated"]:
        return "high", "truncated or malformed segment stream"
    if is_power_of_two(n) and n >= 256:
        return "high", f"symbol count is exactly {n} (byte-boundary bug)"
    if is_power_of_two(n):
        return "medium", f"symbol count is a power of two ({n})"
    if info["huff"] and "huff" not in house_style:
        return "low", "Huffman-coded (uncommon decoder path)"
    if info["refine"] and "refine" not in house_style:
        return "low", "refinement coding (uncommon decoder path)"
    return None


# --------------------------------------------------------------------------
# Finding the JBIG2 images
# --------------------------------------------------------------------------

def jbig2_streams(pdf):
    """Every JBIG2 image in the file, as
    [(page_number, resource_name, role, stream_object), ...].

    role is 'image' for one drawn directly and 'mask' for one serving as
    another image's stencil /Mask. In an OCR'd scan the text layer is
    almost always a mask over a JPEG or JPX background.
    """
    out = []
    for i, page in enumerate(pdf.pages, start=1):
        res = page.obj.get("/Resources")
        if res is None:
            continue
        xobjs = res.get("/XObject")
        if xobjs is None:
            continue
        for name, xo in xobjs.items():
            if xo.get("/Subtype") != pikepdf.Name("/Image"):
                continue
            if str(xo.get("/Filter", "")) == "/JBIG2Decode":
                out.append((i, str(name), "image", xo))
            mask = xo.get("/Mask")
            if mask is not None and hasattr(mask, "get") and \
                    str(mask.get("/Filter", "")) == "/JBIG2Decode":
                out.append((i, str(name), "mask", mask))
    return out


HOUSE_STYLE_SHARE = 0.2     # a trait on more than this fraction is the norm


def scan(pdf):
    """Report on every JBIG2 image. Returns (findings, rows, house_style).

    findings are the rows that assess() complained about. house_style is the
    set of low-risk traits common enough in this file to be worth ignoring.
    """
    parsed = []
    for page_no, name, role, stream in jbig2_streams(pdf):
        try:
            data = stream.read_raw_bytes()
        except Exception as exc:                       # unreadable stream
            parsed.append((page_no, name, role, None, str(exc)))
            continue
        parsed.append((page_no, name, role, describe(data), None))

    readable = [p[3] for p in parsed if p[3] is not None]
    house_style = set()
    if readable:
        for trait in ("huff", "refine"):
            share = sum(1 for i in readable if i[trait]) / len(readable)
            if share > HOUSE_STYLE_SHARE:
                house_style.add(trait)

    rows = []
    for page_no, name, role, info, err in parsed:
        verdict = ("high", err) if info is None else assess(info, house_style)
        rows.append((page_no, name, role, info, verdict))
    findings = [r for r in rows if r[4] is not None]
    return findings, rows, house_style


# --------------------------------------------------------------------------
# The repair: JBIG2 -> CCITT Group 4
# --------------------------------------------------------------------------

def decode_bilevel(doc, xref, is_mask):
    """Decode a JBIG2 image to a PIL '1' image where black means ink.

    MuPDF hands back a stencil /ImageMask as an alpha pixmap (255 where the
    stencil paints) and an ordinary bilevel image as grey (0 is black), so
    the two cases invert relative to each other. Both end up as black-is-ink
    here, which is the only convention the rest of the script knows about.
    """
    pix = pymupdf.Pixmap(doc, xref)
    if pix.n != 1:
        raise ValueError(f"expected 1 component, got {pix.n}")
    grey = Image.frombytes("L", (pix.width, pix.height), pix.samples)
    if is_mask or pix.alpha:
        # painted (255) is ink -> black
        return grey.point(lambda v: 0 if v >= 128 else 255).convert("1")
    # already grey: dark is ink
    return grey.point(lambda v: 0 if v < 128 else 255).convert("1")


def encode_g4(img):
    """CCITT Group 4 payload for a PIL '1' image, as one strip.

    Pillow splits a TIFF into strips by byte budget and each G4 strip
    restarts its reference line, so a multi-strip file cannot simply be
    concatenated into the single codestream a PDF wants. Raise the budget
    to force one strip and assert we got it.
    """
    old_budget = TiffImagePlugin.STRIP_SIZE
    TiffImagePlugin.STRIP_SIZE = 1 << 26
    try:
        buf = io.BytesIO()
        img.save(buf, format="TIFF", compression="group4")
    finally:
        TiffImagePlugin.STRIP_SIZE = old_budget

    raw_tiff = buf.getvalue()
    tif = Image.open(io.BytesIO(raw_tiff))
    offsets, counts = tif.tag_v2[273], tif.tag_v2[279]
    if len(offsets) != 1:
        raise RuntimeError(f"TIFF came back in {len(offsets)} strips")
    if tif.tobytes() != img.tobytes():
        raise RuntimeError("G4 round-trip through Pillow did not match")
    return raw_tiff[offsets[0]:offsets[0] + counts[0]]


def to_ccitt(stream, payload, width, height, black_is_1):
    """Rewrite a JBIG2 image stream in place as CCITTFaxDecode."""
    stream.write(
        payload,
        filter=pikepdf.Name("/CCITTFaxDecode"),
        decode_parms=pikepdf.Dictionary(
            K=-1, Columns=width, Rows=height, BlackIs1=black_is_1),
    )
    stream["/Type"] = pikepdf.Name("/XObject")
    stream["/Subtype"] = pikepdf.Name("/Image")
    stream["/Width"] = width
    stream["/Height"] = height


def convert(src_path, out_path, targets, black_is_1=True):
    """Convert the JBIG2 images on `targets` (1-based page numbers) to G4.

    Returns (jobs, saved_bytes_delta). Each job records what was touched so
    verify() can find it again in the output, whose object numbers will not
    match the input's.
    """
    doc = pymupdf.open(src_path)
    pdf = pikepdf.open(src_path)
    wanted = set(targets)
    jobs, before, after = [], 0, 0
    seen = set()

    for page_no, name, role, stream in jbig2_streams(pdf):
        if page_no not in wanted:
            continue
        if stream.objgen in seen:      # one image shared by several pages
            continue
        seen.add(stream.objgen)
        is_mask = role == "mask"
        width = int(stream["/Width"])
        height = int(stream["/Height"])
        xref = stream.objgen[0]
        try:
            img = decode_bilevel(doc, xref, is_mask)
        except Exception as exc:
            print(f"  [!] page {page_no} {name} ({role}): cannot decode "
                  f"the JBIG2 ({exc}); left alone")
            continue
        if img.size != (width, height):
            print(f"  [!] page {page_no} {name}: decoded {img.size}, "
                  f"dictionary says {(width, height)}; left alone")
            continue

        payload = encode_g4(img)
        before += len(stream.read_raw_bytes())
        after += len(payload)
        to_ccitt(stream, payload, width, height, black_is_1)
        jobs.append(dict(page=page_no, name=name, role=role,
                         bits=img.tobytes(), size=(width, height)))
        print(f"  page {page_no:>4}  {name} ({role})  "
              f"{width}x{height}  -> G4, {len(payload):,} bytes")

    doc.close()
    if jobs:
        pdf.save(out_path)
    pdf.close()
    return jobs, after - before


def verify(out_path, jobs):
    """Decode each converted image from the output and compare bit for bit.

    Returns (ok, inverted, mismatched). `inverted` catches a global polarity
    slip -- if every image comes back as its own negative the BlackIs1 guess
    was backwards, and convert() can simply be re-run the other way.
    """
    doc = pymupdf.open(out_path)
    ok, inverted, mismatched = [], [], []
    for job in jobs:
        page = doc[job["page"] - 1]
        xref = None
        for img in page.get_images(full=True):
            if img[7] != job["name"].lstrip("/"):
                continue
            xref = img[1] if job["role"] == "mask" else img[0]
            break
        if not xref:
            mismatched.append((job, "could not find the image again"))
            continue
        try:
            got = decode_bilevel(doc, xref, job["role"] == "mask").tobytes()
        except Exception as exc:
            mismatched.append((job, f"decode failed: {exc}"))
            continue
        if got == job["bits"]:
            ok.append(job)
        elif got == bytes(~b & 0xFF for b in job["bits"]):
            inverted.append(job)
        else:
            differing = sum(bin(a ^ b).count("1")
                            for a, b in zip(got, job["bits"]))
            mismatched.append((job, f"{differing:,} pixels differ"))
    doc.close()
    return ok, inverted, mismatched


# --------------------------------------------------------------------------

TRAIT_NAMES = {"huff": "Huffman coding", "refine": "refinement coding"}


def print_scan(findings, rows, house_style, verbose):
    total = len(rows)
    print(f"--- {total} JBIG2 image(s) on "
          f"{len({r[0] for r in rows})} page(s) ---")
    for trait in sorted(house_style):
        print(f"    note: {TRAIT_NAMES[trait]} is used throughout this file, "
              f"so it is not flagged per page.")
    if verbose:
        for page_no, name, role, info, verdict in rows:
            flag = f"  <-- {verdict[0].upper()}: {verdict[1]}" if verdict else ""
            syms = info["symbols"] if info else "?"
            inst = info["instances"] if info else "?"
            print(f"  page {page_no:>4}  {name} ({role})  "
                  f"symbols={syms:<6} instances={inst}{flag}")
    if not findings:
        print("--- nothing flagged ---")
        print("    No known-bad shape is present. That is not a promise: if a "
              "reader still drops a page, --fix it anyway.")
        return
    order = {"high": 0, "medium": 1, "low": 2}
    print(f"--- {len(findings)} flagged ---")
    for page_no, name, role, info, (level, why) in sorted(
            findings, key=lambda r: (order[r[4][0]], r[0])):
        print(f"  [{level:^6}] page {page_no:>4}  {name} ({role})  {why}")
    high = sorted({r[0] for r in findings if r[4][0] == "high"})
    if high:
        print(f"\n    fix the high ones with: --fix "
              f"{','.join(str(p) for p in high)}")


def main():
    ap = argparse.ArgumentParser(
        description="Find and repair JBIG2 images that some readers refuse "
                    "to render.",
        epilog=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf", help="input PDF")
    ap.add_argument("--scan", action="store_true",
                    help="report only (the default when no --fix is given)")
    ap.add_argument("--fix", metavar="SPEC",
                    help="pages to convert to CCITT G4: '179', "
                         "'179,181,200-204', or 'all'")
    ap.add_argument("--fix-flagged", action="store_true",
                    help="convert the pages --scan flags as high risk")
    ap.add_argument("--include-medium", action="store_true",
                    help="with --fix-flagged, take the medium ones too")
    ap.add_argument("--verbose", action="store_true",
                    help="with --scan, list every JBIG2 image")
    ap.add_argument("--no-verify", action="store_true",
                    help="skip the decode-and-compare pass (don't)")
    ap.add_argument("-o", "--out", help="output path")
    args = ap.parse_args()

    if not os.path.exists(args.pdf):
        print(f"[!] File not found: {args.pdf}")
        sys.exit(1)
    if args.scan and (args.fix or args.fix_flagged):
        ap.error("--scan means report only; drop it to actually convert")

    pdf = pikepdf.open(args.pdf)
    n_pages = len(pdf.pages)
    findings, rows, house_style = scan(pdf)
    if not rows:
        print("--- no JBIG2 images in this file ---")
        pdf.close()
        return

    if not (args.fix or args.fix_flagged):
        print_scan(findings, rows, house_style, args.verbose)
        pdf.close()
        return

    print_scan(findings, rows, house_style, args.verbose)
    print()

    if args.fix_flagged:
        levels = {"high", "medium"} if args.include_medium else {"high"}
        targets = sorted({r[0] for r in findings if r[4][0] in levels})
        if not targets:
            print("[!] Nothing flagged at that level; nothing to do.")
            pdf.close()
            return
    else:
        targets = parse_pages(args.fix, n_pages)
        if not targets:
            print("[!] --fix matched no pages in range.")
            pdf.close()
            sys.exit(1)
    pdf.close()

    base, ext = os.path.splitext(args.pdf)
    out_path = args.out or (base + "_jbig2fix" + ext)
    if os.path.abspath(out_path) == os.path.abspath(args.pdf):
        print("[!] Output path equals input path; pick a different -o.")
        sys.exit(1)

    print(f"--- converting {len(targets)} page(s) to CCITT G4 ---")
    if len(targets) > 20:
        print(f"    {len(targets)} pages of G4 will add roughly "
              f"{len(targets) * 45 // 1000} MB. Ctrl-C now if that is not "
              f"what you meant.")

    black_is_1 = True
    jobs, delta = convert(args.pdf, out_path, targets, black_is_1)
    if not jobs:
        print("[!] Nothing was converted; no file written.")
        sys.exit(1)

    if not args.no_verify:
        ok, inverted, bad = verify(out_path, jobs)
        if inverted and not ok and not bad:
            # Pillow's photometric convention moved under us. One retry with
            # the opposite polarity, then believe the result or give up.
            print("--- every image came back inverted; retrying with "
                  "BlackIs1 false ---")
            black_is_1 = False
            jobs, delta = convert(args.pdf, out_path, targets, black_is_1)
            ok, inverted, bad = verify(out_path, jobs)
        failed = [(j, why) for j, why in bad] + \
                 [(j, "inverted") for j in inverted]
        if failed:
            print(f"--- {len(failed)} image(s) FAILED verification ---")
            for job, why in failed:
                print(f"  [!] page {job['page']} {job['name']}: {why}")
            print("[!] The output does not reproduce the original pixels. "
                  "Not trusting it; delete it.")
            sys.exit(1)
        print(f"--- verified: {len(ok)} image(s) decode bit-identical to "
              f"the original ---")

    sign = "+" if delta >= 0 else ""
    print(f"\n  -> wrote {len(jobs)} converted image(s) to {out_path} "
          f"({sign}{delta / 1024:,.0f} kB)")


if __name__ == "__main__":
    main()
