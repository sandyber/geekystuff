import os
import subprocess
import sys
import argparse
import glob

# ... inside main() ...

# 1. Capture the full path from the command line
input_path = sys.argv[1]

# 2. Extract just the filename (e.g., "C:/Docs/Penguin.pdf" -> "Penguin.pdf")
filename_only = os.path.basename(input_path)

# 3. Strip the extension (e.g., "Penguin.pdf" -> "Penguin")
base_name_only = os.path.splitext(filename_only)[0]

# 4. Create your metadata string
metadata_title = f"{base_name_only}_Cleaned"

print(f"Cleaning: {filename_only}")
print(f"Metadata will be set to: {metadata_title}")

def run_cmd(cmd):
    """Utility to run shell commands and catch errors."""
    try:
        subprocess.run(cmd, check=True, shell=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(f"Error running command: {cmd}\n{e.stderr.decode()}")

def main():
    parser = argparse.ArgumentParser(description="Clean PDF backgrounds and OCR.")
    parser.add_argument("input_file", help="Input PDF filename")
    parser.add_argument("pages", help="Page range (e.g., 1,3,5-10)")
    parser.add_argument("--threshold", type=int, default=40, help="White threshold percentage (default: 40)")
    
    args = parser.parse_args()
    base_name = os.path.splitext(args.input_file)[0]
    output_dir = "final_pdf_pages"

    # 1. Cleanup & Setup
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    for f in glob.glob(f"{output_dir}/*.pdf"):
        os.remove(f)

    # 2. Parse Pages
    page_list = []
    for part in args.pages.split(','):
        if '-' in part:
            start, end = map(int, part.split('-'))
            page_list.extend(range(start, end + 1))
        else:
            page_list.append(int(part))

    # 3. Process Pages
    for p in page_list:
        padded = f"{p:03d}"
        print(f"Processing Page {p} (Threshold: {args.threshold}%)...")
        
        # Ghostscript: Extract page to PNG
        run_cmd(f'gswin64c -sDEVICE=pnggray -r300 -dFirstPage={p} -dLastPage={p} -o temp_page.png -q -dBATCH -dNOPAUSE "{args.input_file}"')
        
        # ImageMagick: Apply threshold
        run_cmd(f'magick temp_page.png -white-threshold {args.threshold}% -colorspace gray -type bilevel -compress fax temp_cleaned.png')
        
        # Tesseract: OCR to PDF
        run_cmd(f'tesseract temp_cleaned.png {output_dir}/page_{padded} -l eng pdf')

    # 4. Merge
    print("Merging final PDF...")
    with open("pages.txt", "w") as f:
        for pdf in sorted(glob.glob(f"{output_dir}/page_*.pdf")):
            f.write(f'"{pdf.replace("\\", "/")}"\n')

    # Create Metadata
    with open("docinfo.ps", "w") as f:
        f.write(f"[ /Title ({metadata_title}) /Author (YSB-LLM) /DOCINFO pdfmark")

    run_cmd(f'gswin64c -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dQUIET -sOutputFile="{base_name}_cleaned.pdf" @"pages.txt" docinfo.ps')

    # Final Cleanup
    for f in ["temp_page.png", "temp_cleaned.png", "pages.txt", "docinfo.ps"]:
        if os.path.exists(f): os.remove(f)
    print(f"Done! Created {base_name}_cleaned.pdf")

if __name__ == "__main__":
    main()