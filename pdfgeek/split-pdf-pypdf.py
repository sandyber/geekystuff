import os
import sys
import re
from pypdf import PdfReader, PdfWriter

def get_help():
    # Expanded guide with no special symbols to prevent encoding crashes
    help_text = """
================================================================
PDF BOOKMARK SPLITTER - MANUAL (YSB + LLM)
================================================================
PURPOSE:
  Splits a PDF into individual files based on its bookmarks.

PREREQUISITES:
  1. Python 3.x installed.
  2. pypdf library installed (run: pip install pypdf).

COMMAND LINE USAGE:
  python split_pdf.py "your_document.pdf"

CONTROLS:
  Press CTRL + C at any time to stop the process and exit.

OUTPUT:
  - Creates a folder named after your PDF.
  - Names files: [Folder]-[Sequence]-[Bookmark Name].pdf
  - Automatically removes illegal characters (?, *, :, etc.)
    from the filenames to ensure Windows compatibility.

TROUBLESHOOTING:
  - If no files are created, ensure the PDF has bookmarks.
  - If 'pypdf' error appears, run 'pip install pypdf' again.
================================================================
    """
    return help_text

def clean_filename(title):
    # Strictly allow alphanumeric, spaces, and hyphens
    clean = re.sub(r'[^a-zA-Z0-9 \-]', '', title)
    clean = re.sub(r' +', ' ', clean)
    return clean.strip()

def process_pdf(input_path):
    if not os.path.exists(input_path):
        print("ERROR: File not found: " + input_path)
        return

    base_name = os.path.splitext(os.path.basename(input_path))[0]
    safe_folder = clean_filename(base_name)
    if not os.path.exists(safe_folder):
        os.makedirs(safe_folder)

    reader = PdfReader(input_path)
    num_pages = len(reader.pages)
    bookmarks = []

    def parse_outline(entries):
        for entry in entries:
            if isinstance(entry, list):
                parse_outline(entry)
            else:
                try:
                    page_index = reader.get_destination_page_number(entry)
                    bookmarks.append({'title': clean_filename(entry.title), 'start': page_index})
                except:
                    continue

    parse_outline(reader.outline)
    if not bookmarks:
        print("RESULT: No bookmarks found in this PDF.")
        return

    bookmarks.sort(key=lambda x: x['start'])
    print("Found " + str(len(bookmarks)) + " bookmarks. Starting extraction...")

    for i in range(len(bookmarks)):
        current = bookmarks[i]
        start_page = current['start']
        end_page = bookmarks[i+1]['start'] if i + 1 < len(bookmarks) else num_pages
        
        if end_page <= start_page:
            end_page = start_page + 1 if start_page + 1 <= num_pages else num_pages

        writer = PdfWriter()
        for page_num in range(start_page, end_page):
            writer.add_page(reader.pages[page_num])

        display_title = current['title'] if current['title'] else "Section"
        out_filename = safe_folder + "-" + str(i + 1) + "-" + display_title + ".pdf"
        out_path = os.path.join(safe_folder, out_filename)

        print("Writing (" + str(i+1) + "/" + str(len(bookmarks)) + "): " + out_filename)

        try:
            with open(out_path, "wb") as f:
                writer.write(f)
        except KeyboardInterrupt:
            raise 
        except:
            print("SKIPPING: " + out_filename + " (Write Error)")

if __name__ == "__main__":
    try:
        if len(sys.argv) < 2 or sys.argv[1] in ["-h", "--help", "/?"]:
            print(get_help())
        else:
            process_pdf(sys.argv[1])
    except KeyboardInterrupt:
        print("\n\n[HALTED] Process stopped by user. Control returned to command line.")
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)