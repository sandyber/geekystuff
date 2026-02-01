import os
import subprocess
import sys
import argparse
import glob

def run_cmd(cmd):
    """Utility to run shell commands."""
    try:
        subprocess.run(cmd, check=True, shell=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(f"\n[!] Error: {e.stderr.decode()}")
    except KeyboardInterrupt:
        raise KeyboardInterrupt

def main():
    # Get the actual name of this script file for the usage guide
    script_name = os.path.basename(sys.argv[0])

    # --- 1. Enhanced Usage Guide & Requirements Summary ---
    if len(sys.argv) < 3 or "--help" in sys.argv or "-h" in sys.argv:
        print("\n" + "="*75)
        print("                PDF BACKGROUND REMOVER & OCR TOOL")
        print("="*75)
        print("USAGE:")
        print(f"  python {script_name} <file.pdf> <pages> [threshold:XX] [--preview]")
        print("\nEXAMPLES:")
        print(f"  python {script_name} Ethics.pdf 1-10 threshold:65")
        print(f"  python {script_name} Ethics.pdf 5 --preview")
        print("\nSYSTEM REQUIREMENTS (Must be in your PATH):")
        print("  1. Ghostscript (gswin64c) - For PDF extraction")
        print("  2. ImageMagick (magick)   - For background cleaning")
        print("  3. Tesseract (tesseract)  - For OCR text recognition")
        print("\nLIBRARIES USED (Standard Python - No pip install needed):")
        print("  os, sys, subprocess, argparse, glob")
        print("="*75 + "\n")
        return

    # --- 2. Logic Setup ---
    input_path = sys.argv[1]
    input_dir = os.path.dirname(input_path) or "."
    filename_only = os.path.basename(input_path)
    base_name_only = os.path.splitext(filename_only)[0]
    output_pdf_path = os.path.join(input_dir, f"{base_name_only}_cleaned.pdf")
    output_dir = "final_pdf_pages"

    # Argument Extraction
    raw_pages = sys.argv[2]
    threshold_val = "40"
    is_preview = "--preview" in sys.argv
    for arg in sys.argv:
        if "threshold" in arg.lower():
            threshold_val = arg.split(":")[-1].split("=")[-1]

    try:
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
        
        # Clean folder ONLY if starting a full run
        if not is_preview:
            print(f"Cleaning leftovers in /{output_dir}...")
            for f in glob.glob(os.path.join(output_dir, "*.pdf")):
                os.remove(f)

        # Parse page range
        page_list = []
        for part in raw_pages.split(','):
            if '-' in part:
                start, end = map(int, part.split('-'))
                page_list.extend(range(start, end + 1))
            else:
                page_list.append(int(part))

        if is_preview: page_list = [page_list[0]]

        print(f"--- Mode: {'PREVIEW' if is_preview else 'FULL PROCESS (Ctrl-C to interrupt)'} ---")
        print(f"Cleaning: {filename_only}")

        for p in page_list:
            padded = f"{p:03d}"
            print(f"Processing Page {p} (Threshold: {threshold_val}%)...")
            
            run_cmd(f'gswin64c -sDEVICE=pnggray -r300 -dFirstPage={p} -dLastPage={p} -o temp_page.png -q -dBATCH -dNOPAUSE "{input_path}"')
            run_cmd(f'magick temp_page.png -white-threshold {threshold_val}% -colorspace gray -type bilevel -compress fax temp_cleaned.png')
            
            if is_preview:
                print("\nOpening preview image... adjust threshold if needed!")
                os.startfile("temp_cleaned.png") 
                return 

            run_cmd(f'tesseract temp_cleaned.png {output_dir}/page_{padded} -l eng pdf')

        # Merge Phase
        print("\nMerging final PDF...")
        with open("pages.txt", "w") as f:
            for pdf in sorted(glob.glob(os.path.join(output_dir, "page_*.pdf"))):
                f.write(f'"{pdf.replace("\\", "/")}"\n')
        
        run_cmd(f'gswin64c -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dQUIET -sOutputFile="{output_pdf_path}" @"pages.txt"')
        print(f"Done! Saved to: {output_pdf_path}")

    except KeyboardInterrupt:
        print("\n\n[!] STOPPED: Ctrl+C detected. Processed pages are still in /" + output_dir)
    finally:
        # Final cleanup of small temp files
        files_to_kill = ["temp_page.png", "pages.txt", "docinfo.ps"]
        if not is_preview: files_to_kill.append("temp_cleaned.png")
        for f in files_to_kill:
            if os.path.exists(f): os.remove(f)

if __name__ == "__main__":
    main()