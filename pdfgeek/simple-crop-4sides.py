import sys
import pymupdf

def usage():
    print(f"Usage: {sys.argv[0]} <input.pdf> <margin> [output.pdf]")
    print(f"       {sys.argv[0]} <input.pdf> <left> <top> <right> <bottom> [output.pdf]")
    print("       margins in points, 72 pt = 1 inch")
    sys.exit(1)

args = sys.argv[1:]
if len(args) < 2:
    usage()

infile = args[0]
rest = args[1:]

# Split trailing output filename (a non-numeric argument) from the margins.
outfile = None
try:
    float(rest[-1])
except ValueError:
    outfile = rest[-1]
    rest = rest[:-1]

if len(rest) == 1:
    left = top = right = bottom = float(rest[0])
elif len(rest) == 4:
    left, top, right, bottom = (float(x) for x in rest)
else:
    usage()

if outfile is None:
    outfile = infile.rsplit(".", 1)[0] + "_cropped.pdf"

doc = pymupdf.open(infile)
for page in doc:
    r = page.mediabox
    page.set_cropbox(pymupdf.Rect(r.x0 + left, r.y0 + top, r.x1 - right, r.y1 - bottom))
doc.save(outfile)
print(f"Saved {outfile}")
