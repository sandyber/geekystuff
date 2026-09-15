import sys
import pymupdf

if len(sys.argv) < 3:
    print(f"Usage: {sys.argv[0]} <input.pdf> <margin_pts> [output.pdf]")
    print("       margin_pts: points to shave off each side (72 pt = 1 inch)")
    sys.exit(1)

infile = sys.argv[1]
m = float(sys.argv[2])
outfile = sys.argv[3] if len(sys.argv) > 3 else infile.rsplit(".", 1)[0] + "_cropped.pdf"

doc = pymupdf.open(infile)
for page in doc:
    r = page.mediabox
    page.set_cropbox(pymupdf.Rect(r.x0 + m, r.y0 + m, r.x1 - m, r.y1 - m))
doc.save(outfile)
print(f"Saved {outfile}")
