import pymupdf
doc = pymupdf.open("book.pdf")
m = 90  # points to shave off each side; 72 pt = 1 inch
for page in doc:
    r = page.mediabox
    page.set_cropbox(pymupdf.Rect(r.x0 + m, r.y0 + m, r.x1 - m, r.y1 - m))
doc.save("book_cropped.pdf")