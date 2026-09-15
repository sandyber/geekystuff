import sys
import pymupdf

def usage():
    print(f"Usage: {sys.argv[0]} <input.pdf> <range>:<l>,<t>,<r>,<b> [more ranges...] [output.pdf] [--swap] [--no-parity]")
    print()
    print("  <range>   1-based, inclusive: '5', '3-78', or '10-end'")
    print("  l,t,r,b   margins in points (72 pt = 1 inch)")
    print()
    print("  By default <l>/<r> apply to ODD pages (1, 3, 5, ...) and are")
    print("  swapped on even pages, for scanned books with a gutter.")
    print("  --swap        reverse the parity")
    print("  --no-parity   apply l/t/r/b literally on every page, no swapping")
    print()
    print("  Pages not covered by any range are left uncropped.")
    print("  Example:")
    print(f"    {sys.argv[0]} book.pdf 1-2:0,0,0,0 3-78:100,90,50,40 79-end:80,90,60,40")
    sys.exit(1)

args = sys.argv[1:]
swap = "--swap" in args
parity = "--no-parity" not in args
args = [a for a in args if a not in ("--swap", "--no-parity")]

if len(args) < 2:
    usage()

infile = args[0]
specs = []
outfile = None

for a in args[1:]:
    if ":" in a:
        specs.append(a)
    else:
        if outfile is not None:
            print(f"Unrecognized argument: {a}")
            usage()
        outfile = a

if not specs:
    usage()
if outfile is None:
    outfile = infile.rsplit(".", 1)[0] + "_cropped.pdf"

doc = pymupdf.open(infile)
n = len(doc)

def parse_range(s):
    # returns (first, last) as 1-based inclusive page numbers
    if "-" in s:
        a, b = s.split("-", 1)
        first = int(a)
        last = n if b.lower() == "end" else int(b)
    else:
        first = last = int(s)
    if first < 1 or last > n or first > last:
        print(f"Bad range '{s}' for a {n}-page document.")
        sys.exit(1)
    return first, last

# Build a per-page margin table. Later specs override earlier ones on overlap.
margins = [None] * (n + 1)  # index by 1-based page number
for spec in specs:
    rng, vals = spec.split(":", 1)
    parts = vals.split(",")
    if len(parts) != 4:
        print(f"Bad margins in '{spec}': need l,t,r,b")
        sys.exit(1)
    l, t, r, b = (float(x) for x in parts)
    first, last = parse_range(rng)
    for p in range(first, last + 1):
        margins[p] = (l, t, r, b)

for i, page in enumerate(doc):
    pno = i + 1  # 1-based
    if margins[pno] is None:
        continue
    l, t, r, b = margins[pno]
    if parity:
        odd = (pno % 2 == 1)
        if swap:
            odd = not odd
        if not odd:
            l, r = r, l
    box = page.mediabox
    page.set_cropbox(pymupdf.Rect(box.x0 + l, box.y0 + t, box.x1 - r, box.y1 - b))

doc.save(outfile)
print(f"Saved {outfile}")
