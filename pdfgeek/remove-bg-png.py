import os
import shutil
import subprocess
import sys
import glob
import platform
from concurrent.futures import ThreadPoolExecutor, as_completed

# Keep ImageMagick single-threaded so our worker pool isn't fighting itself.
os.environ.setdefault("MAGICK_THREAD_LIMIT", "1")

def find_binary(*candidates):
    for name in candidates:
        if shutil.which(name):
            return name
    return None

def run_cmd(cmd_list):
    """Run a command (as a list). Returns (ok, stderr_text)."""
    try:
        subprocess.run(cmd_list, check=True, capture_output=True)
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, e.stderr.decode(errors="replace") if e.stderr else ""

def clean_page(page_num, input_path, output_dir, threshold_val, dpi_val,
               gs_bin, magick_bin):
    """Extract one page, threshold it to bilevel PNG. No OCR here — ocrmypdf handles that."""
    padded = f"{page_num:04d}"
    raw_png = os.path.join(output_dir, f"_raw_{padded}.png")
    clean_png = os.path.join(output_dir, f"page_{padded}.png")

    try:
        ok, err = run_cmd([
            gs_bin, "-sDEVICE=pnggray", f"-r{dpi_val}",
            f"-dFirstPage={page_num}", f"-dLastPage={page_num}",
            "-o", raw_png, "-q", "-dBATCH", "-dNOPAUSE", input_path,
        ])
        if not ok:
            return page_num, False, f"gs failed: {err}"

        ok, err = run_cmd([
            magick_bin, raw_png,
            "-white-threshold", f"{threshold_val}%",
            "-colorspace", "gray", "-type", "bilevel",
            clean_png,
        ])
        if not ok:
            return page_num, False, f"magick failed: {err}"

        return page_num, True, ""
    finally:
        if os.path.exists(raw_png):
            try: os.remove(raw_png)
            except OSError: pass

def main():
    script_name = os.path.basename(sys.argv[0])

    if len(sys.argv) < 3 or "--help" in sys.argv or "-h" in sys.argv:
        print("\n" + "=" * 75)
        print("       PDF BACKGROUND REMOVER (clean + OCR + JBIG2 compression)")
        print("=" * 75)
        print("USAGE:")
        print(f"  python {script_name} <file.pdf> <pages> [threshold:XX] [dpi:NNN]")
        print(f"                    [workers:N] [--preview] [--keep-pngs]")
        print("\nEXAMPLES:")
        print(f"  python {script_name} Ethics.pdf 1-741")
        print(f"  python {script_name} Ethics.pdf 1-10 threshold:65")
        print(f"  python {script_name} Ethics.pdf 5 --preview")
        print(f"  python {script_name} Ethics.pdf 1-741 workers:6")
        print("\nDEFAULTS:")
        print("  threshold:40    dpi:200    workers:auto (logical CPU count)")
        print("\nPIPELINE:")
        print("  1. Ghostscript + ImageMagick (parallel) -> cleaned bilevel PNGs")
        print("  2. img2pdf -> single image-only PDF (lossless)")
        print("  3. ocrmypdf -> OCR + JBIG2 compression in one pass")
        print("\nREQUIRED (auto-detected on PATH):")
        print("  Ghostscript (gswin64c on Windows / gs elsewhere)")
        print("  ImageMagick (magick)")
        print("  ocrmypdf    (pip install ocrmypdf)")
        print("  img2pdf     (pip install img2pdf)")
        print("  jbig2.exe   (jbig2enc) - on PATH; required for compression to actually shrink")
        print("=" * 75 + "\n")
        return

    gs_bin = find_binary("gswin64c", "gs")
    magick_bin = find_binary("magick")
    ocrmypdf_bin = find_binary("ocrmypdf")

    missing = []
    if not gs_bin: missing.append("ghostscript (gswin64c/gs)")
    if not magick_bin: missing.append("imagemagick (magick)")
    if not ocrmypdf_bin: missing.append("ocrmypdf (pip install ocrmypdf)")
    if missing:
        print(f"[!] Missing required binaries: {', '.join(missing)}")
        return

    try:
        import img2pdf  # noqa: F401
    except ImportError:
        print("[!] img2pdf not installed. Run: pip install img2pdf")
        return

    input_path = sys.argv[1]
    if not os.path.exists(input_path):
        print(f"[!] File not found: {input_path}")
        return

    raw_pages = sys.argv[2]
    threshold_val = "40"
    dpi_val = "200"
    is_preview = "--preview" in sys.argv
    keep_pngs = "--keep-pngs" in sys.argv
    workers = max(1, (os.cpu_count() or 4))

    for arg in sys.argv[3:]:
        low = arg.lower()
        if low.startswith("threshold"):
            threshold_val = arg.split(":")[-1].split("=")[-1]
        elif low.startswith("dpi"):
            dpi_val = arg.split(":")[-1].split("=")[-1]
        elif low.startswith("workers"):
            try:
                workers = max(1, int(arg.split(":")[-1].split("=")[-1]))
            except ValueError:
                pass

    input_dir = os.path.dirname(input_path) or "."
    base_name = os.path.splitext(os.path.basename(input_path))[0]
    image_pdf = os.path.join(input_dir, f"{base_name}_images.pdf")
    final_path = os.path.join(input_dir, f"{base_name}_cleaned.pdf")
    output_dir = "cleaned_pages"

    try:
        os.makedirs(output_dir, exist_ok=True)

        if not is_preview:
            print(f"Cleaning leftovers in /{output_dir}...")
            for f in glob.glob(os.path.join(output_dir, "*.png")):
                os.remove(f)

        page_list = []
        for part in raw_pages.split(','):
            if '-' in part:
                start, end = map(int, part.split('-'))
                page_list.extend(range(start, end + 1))
            else:
                page_list.append(int(part))

        # --- Preview ---
        if is_preview:
            p = page_list[0]
            print(f"--- PREVIEW (page {p}, threshold {threshold_val}%, dpi {dpi_val}) ---")
            run_cmd([
                gs_bin, "-sDEVICE=pnggray", f"-r{dpi_val}",
                f"-dFirstPage={p}", f"-dLastPage={p}",
                "-o", "temp_page.png", "-q", "-dBATCH", "-dNOPAUSE", input_path,
            ])
            run_cmd([
                magick_bin, "temp_page.png",
                "-white-threshold", f"{threshold_val}%",
                "-colorspace", "gray", "-type", "bilevel",
                "-compress", "fax", "temp_cleaned.png",
            ])
            print("Opening preview... adjust threshold if needed!")
            if platform.system() == "Windows":
                os.startfile("temp_cleaned.png")
            elif platform.system() == "Darwin":
                subprocess.run(["open", "temp_cleaned.png"])
            else:
                subprocess.run(["xdg-open", "temp_cleaned.png"])
            return

        # --- Step 1: parallel clean ---
        print(f"--- STEP 1/3: cleaning {len(page_list)} pages with {workers} workers "
              f"(DPI {dpi_val}, threshold {threshold_val}%) ---")

        completed, failed = 0, []
        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(
                    clean_page, p, input_path, output_dir,
                    threshold_val, dpi_val, gs_bin, magick_bin,
                ): p for p in page_list
            }
            try:
                for fut in as_completed(futures):
                    page_num, ok, err = fut.result()
                    completed += 1
                    status = "OK" if ok else "FAIL"
                    line = f"  [{completed:>4}/{len(page_list)}] page {page_num:>4} {status}"
                    if not ok:
                        line += f"  {err.strip()[:80]}"
                        failed.append(page_num)
                    print(line)
            except KeyboardInterrupt:
                print("\n[!] Interrupt — cancelling pending pages...")
                executor.shutdown(wait=False, cancel_futures=True)
                raise

        if failed:
            print(f"\n[!] {len(failed)} page(s) failed: {failed}")
            print("    Continuing with the pages that succeeded.")

        png_files = sorted(glob.glob(os.path.join(output_dir, "page_*.png")))
        if not png_files:
            print("[!] No cleaned pages produced. Aborting.")
            return

        # --- Step 2: bundle PNGs into image-only PDF ---
        print(f"\n--- STEP 2/3: bundling {len(png_files)} pages into image PDF ---")
        import img2pdf
        with open(image_pdf, "wb") as f:
            f.write(img2pdf.convert(png_files))
        bundle_size = os.path.getsize(image_pdf) / 1024 / 1024
        print(f"  -> {image_pdf} ({bundle_size:.1f} MB)")

        # --- Step 3: ocrmypdf for OCR + JBIG2 ---
        print(f"\n--- STEP 3/3: OCR + JBIG2 compression ({workers} jobs) ---")
        ok, err = run_cmd([
            ocrmypdf_bin, "--optimize", "1",
            "--output-type", "pdf",
            "-l", "eng", "--jobs", str(workers),
            image_pdf, final_path,
        ])
        if ok and os.path.exists(final_path):
            final_size = os.path.getsize(final_path) / 1024 / 1024
            print(f"\nDone! Saved to: {final_path} ({final_size:.1f} MB)")
            os.remove(image_pdf)
            if not keep_pngs:
                for png in png_files:
                    try: os.remove(png)
                    except OSError: pass
        else:
            print(f"[!] ocrmypdf failed. Image PDF kept at: {image_pdf}")
            if err:
                # Print full stderr so the actual traceback is visible
                print("--- ocrmypdf stderr ---")
                print(err)
                print("--- end stderr ---")

    except KeyboardInterrupt:
        print(f"\n\n[!] STOPPED. Cleaned PNGs remain in /{output_dir}")
    finally:
        for f in ["temp_page.png", "temp_cleaned.png"]:
            if os.path.exists(f):
                try: os.remove(f)
                except OSError: pass

if __name__ == "__main__":
    main()
