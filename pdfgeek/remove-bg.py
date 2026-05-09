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

def _remap_toc_simple(toc, page_map):
    """Remap a simple TOC [[lvl, title, page], ...] using page_map.
    Entries whose page isn't in the map are dropped."""
    out = []
    for lvl, title, page in toc:
        new_page = page_map.get(page)
        if new_page is not None:
            out.append([lvl, title, new_page])
    return out


def _remap_toc_detailed(toc, page_map):
    """Remap a detailed TOC [[lvl, title, page, dest], ...] using page_map.
    The dest dict may carry its own 'page' (0-based) which is also remapped.
    Entries whose page isn't in the map are dropped."""
    out = []
    for lvl, title, page, dest in toc:
        new_page = page_map.get(page)
        if new_page is None:
            continue
        new_dest = dict(dest) if isinstance(dest, dict) else dest
        if isinstance(new_dest, dict) and "page" in new_dest:
            # dest 'page' is 0-based; convert to 1-based for lookup, back after.
            mapped = page_map.get(new_dest["page"] + 1)
            if mapped is None:
                continue  # dest page itself isn't preserved -> drop entry
            new_dest["page"] = mapped - 1
        out.append([lvl, title, new_page, new_dest])
    return out


def _normalize_toc_levels(toc):
    """PyMuPDF's set_toc requires each level to be at most prev_level + 1.
    When parents are dropped, surviving children can violate this. Walk the
    list and clamp each level to (prev_level + 1) to repair the hierarchy."""
    out, prev = [], 0
    for entry in toc:
        entry = list(entry)
        lvl = entry[0]
        if lvl > prev + 1:
            lvl = prev + 1
        entry[0] = lvl
        prev = lvl
        out.append(entry)
    return out


def transfer_bookmarks(source, target, output, page_map=None, simple=False):
    """Copy bookmark outline from `source` PDF onto `target`, write to `output`.

    page_map: dict mapping source page (1-based) -> output position (1-based).
              If None, an identity map for source pages 1..n is built (i.e. no
              remapping). Bookmarks pointing at source pages not in the map
              are dropped.

    Returns (ok, message).
    """
    try:
        import pymupdf  # aka fitz
    except ImportError:
        return False, "pymupdf not installed (pip install pymupdf)"

    try:
        src = pymupdf.open(source)
    except Exception as e:
        return False, f"could not open source: {e}"
    try:
        dst = pymupdf.open(target)
    except Exception as e:
        src.close()
        return False, f"could not open target: {e}"

    try:
        toc = src.get_toc(simple=simple)
        if not toc:
            return True, "source has no bookmarks; nothing to transfer"

        if page_map is None:
            page_map = {i: i for i in range(1, src.page_count + 1)}

        original_count = len(toc)
        toc = _remap_toc_simple(toc, page_map) if simple else _remap_toc_detailed(toc, page_map)
        dropped = original_count - len(toc)

        if not toc:
            return False, f"no bookmarks land on preserved pages (had {original_count})"

        # Clamp to target's actual page count and renormalize hierarchy levels
        # so set_toc doesn't error on orphaned children.
        n_pages = dst.page_count
        toc = [e for e in toc if 1 <= e[2] <= n_pages]
        toc = _normalize_toc_levels(toc)

        dst.set_toc(toc)
        # Write to a temp path then atomically replace, so we can safely
        # "transfer onto target" (output == target) without saveIncr quirks.
        if os.path.abspath(output) == os.path.abspath(target):
            tmp_out = output + ".tmp_toc"
            dst.save(tmp_out)
            dst.close()
            src.close()
            os.replace(tmp_out, output)
            msg = f"wrote {len(toc)} bookmark(s)"
            if dropped:
                msg += f"; dropped {dropped} pointing at unprocessed pages"
            return True, msg
        else:
            dst.save(output)

        msg = f"wrote {len(toc)} bookmark(s)"
        if dropped:
            msg += f"; dropped {dropped} pointing at unprocessed pages"
        return True, msg
    except Exception as e:
        return False, f"transfer failed: {e}"
    finally:
        try: src.close()
        except Exception: pass
        try: dst.close()
        except Exception: pass


def clean_page(page_num, input_path, output_dir, threshold_val, dpi_val,
               gs_bin, magick_bin):
    """Extract one page, threshold it to bilevel TIFF. No OCR here — ocrmypdf handles that.
    TIFF (Group 4) is used instead of PNG because some prebuilt jbig2enc binaries lack libpng."""
    padded = f"{page_num:04d}"
    raw_png = os.path.join(output_dir, f"_raw_{padded}.png")
    clean_tif = os.path.join(output_dir, f"page_{padded}.tif")

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
            "-compress", "Group4", clean_tif,
        ])
        if not ok:
            return page_num, False, f"magick failed: {err}"

        return page_num, True, ""
    finally:
        if os.path.exists(raw_png):
            try: os.remove(raw_png)
            except OSError: pass

def install_python_deps():
    """Pip-install the Python packages this script needs (img2pdf, ocrmypdf).

    System binaries (Ghostscript, ImageMagick, jbig2enc) are NOT pip-installable
    and must be installed via the OS package manager / installer.
    """
    packages = ["img2pdf", "ocrmypdf", "pymupdf"]

    # Skip anything already importable / on PATH so reruns are cheap.
    needed = []
    for pkg in packages:
        if pkg == "img2pdf":
            try:
                import img2pdf  # noqa: F401
                print(f"  [skip] {pkg} already installed")
            except ImportError:
                needed.append(pkg)
        elif pkg == "ocrmypdf":
            if shutil.which("ocrmypdf"):
                print(f"  [skip] {pkg} already installed")
            else:
                needed.append(pkg)
        elif pkg == "pymupdf":
            try:
                import pymupdf  # noqa: F401
                print(f"  [skip] {pkg} already installed")
            except ImportError:
                needed.append(pkg)

    if not needed:
        print("All Python dependencies already present.")
        _warn_about_system_binaries()
        return

    print(f"--- Installing Python packages: {', '.join(needed)} ---")
    base_cmd = [sys.executable, "-m", "pip", "install", "--upgrade"]
    ok, err = run_cmd(base_cmd + needed)
    if not ok:
        # Common case: system-managed Python (PEP 668). Retry with --user.
        print("  [!] First install attempt failed; retrying with --user ...")
        if err.strip():
            print(f"      reason: {err.strip().splitlines()[-1][:200]}")
        ok, err = run_cmd(base_cmd + ["--user"] + needed)

    if ok:
        print("  -> Python packages installed.")
    else:
        print("  [!] pip install failed. Stderr:")
        print(err)
        sys.exit(1)

    _warn_about_system_binaries()


def _warn_about_system_binaries():
    """Remind the user about the non-pip dependencies."""
    missing_sys = []
    if not find_binary("gswin64c", "gs"):
        missing_sys.append("Ghostscript")
    if not find_binary("magick"):
        missing_sys.append("ImageMagick")
    if not find_binary("jbig2"):
        missing_sys.append("jbig2enc (optional but needed for real compression)")
    if missing_sys:
        print("\n[note] These system binaries are NOT pip-installable and are still missing:")
        for m in missing_sys:
            print(f"        - {m}")
        print("       Install them via your OS package manager / vendor installer.")


def main():
    script_name = os.path.basename(sys.argv[0])

    # --install-deps can be run on its own (no PDF needed) to set up Python packages.
    if "--install-deps" in sys.argv:
        install_python_deps()
        # If the user *only* asked to install, exit here. Otherwise fall through
        # and continue with the normal run using the freshly installed packages.
        if len(sys.argv) < 3:
            return

    if len(sys.argv) < 3 or "--help" in sys.argv or "-h" in sys.argv:
        print("\n" + "=" * 75)
        print("       PDF BACKGROUND REMOVER (clean + OCR + JBIG2 + bookmarks)")
        print("=" * 75)
        print("USAGE:")
        print(f"  python {script_name} <file.pdf> <pages> [threshold:XX] [dpi:NNN]")
        print(f"                    [workers:N] [--preview] [--keep-tiffs]")
        print(f"                    [--no-bookmarks]")
        print(f"  python {script_name} --install-deps")
        print("\nEXAMPLES:")
        print(f"  python {script_name} Ethics.pdf 1-741")
        print(f"  python {script_name} Ethics.pdf 1-10 threshold:65")
        print(f"  python {script_name} Ethics.pdf 5 --preview")
        print(f"  python {script_name} Ethics.pdf 1-741 workers:6")
        print(f"  python {script_name} Ethics.pdf 1-741 --no-bookmarks")
        print(f"  python {script_name} --install-deps             (set up python deps)")
        print(f"  python {script_name} Ethics.pdf 1-10 --install-deps  (install + run)")
        print("\nDEFAULTS:")
        print("  threshold:40    dpi:300    workers:auto (logical CPU count)")
        print("  bookmarks: copied from source by default; --no-bookmarks to skip.")
        print("\nPIPELINE:")
        print("  1. Ghostscript + ImageMagick (parallel) -> cleaned bilevel PNGs")
        print("  2. img2pdf -> single image-only PDF (lossless)")
        print("  3. ocrmypdf -> OCR + JBIG2 compression in one pass")
        print("  4. pymupdf  -> copy bookmarks from source onto cleaned PDF")
        print("                 Maps each source page to its position in the")
        print("                 output, so partial ranges like '7-10,297-298'")
        print("                 keep bookmarks on the pages you processed and")
        print("                 drop bookmarks on pages you skipped.")
        print("\nREQUIRED (auto-detected on PATH):")
        print("  Ghostscript (gswin64c on Windows / gs elsewhere)")
        print("  ImageMagick (magick)")
        print("  ocrmypdf    (pip install ocrmypdf)")
        print("  img2pdf     (pip install img2pdf)")
        print("  pymupdf     (pip install pymupdf)  -- only needed for step 4")
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
    dpi_val = "300"
    is_preview = "--preview" in sys.argv
    keep_tiffs = "--keep-tiffs" in sys.argv
    do_bookmarks = "--no-bookmarks" not in sys.argv
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
            for pattern in ("*.png", "*.tif"):
                for f in glob.glob(os.path.join(output_dir, pattern)):
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
        print(f"--- STEP 1/4: cleaning {len(page_list)} pages with {workers} workers "
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

        page_files = sorted(glob.glob(os.path.join(output_dir, "page_*.tif")))
        if not page_files:
            print("[!] No cleaned pages produced. Aborting.")
            return

        # --- Step 2: bundle TIFFs into image-only PDF ---
        print(f"\n--- STEP 2/4: bundling {len(page_files)} pages into image PDF ---")
        import img2pdf
        with open(image_pdf, "wb") as f:
            f.write(img2pdf.convert(page_files))
        bundle_size = os.path.getsize(image_pdf) / 1024 / 1024
        print(f"  -> {image_pdf} ({bundle_size:.1f} MB)")

        # --- Step 3: ocrmypdf for OCR + JBIG2 ---
        print(f"\n--- STEP 3/4: OCR + JBIG2 compression ({workers} jobs) ---")
        ok, err = run_cmd([
            ocrmypdf_bin, "--optimize", "1",
            "--output-type", "pdf",
            "-l", "eng", "--jobs", str(workers),
            image_pdf, final_path,
        ])
        if ok and os.path.exists(final_path):
            os.remove(image_pdf)
            if not keep_tiffs:
                for f in page_files:
                    try: os.remove(f)
                    except OSError: pass

            # --- Step 4: bookmark transfer (best-effort) ---
            if do_bookmarks:
                # Build source-page -> output-position map. The cleaned PDF's
                # pages are laid out in the same order page_list was processed,
                # so source page page_list[i] becomes output position i+1.
                # This handles arbitrary partial ranges like "7-10,297-298"
                # correctly: bookmarks on pages 7-10 and 297-298 are kept,
                # bookmarks on pages in between are dropped.
                page_map = {src_p: out_p for out_p, src_p in enumerate(page_list, start=1)}
                print(f"\n--- STEP 4/4: transferring bookmarks "
                      f"({len(page_list)} pages mapped) ---")
                ok_bm, msg = transfer_bookmarks(
                    source=input_path,
                    target=final_path,
                    output=final_path,  # in-place
                    page_map=page_map,
                    simple=False,
                )
                prefix = "  ->" if ok_bm else "  [!]"
                print(f"{prefix} {msg}")
                if not ok_bm:
                    print("       (cleaned PDF is fine; just lacks bookmarks)")
            else:
                print("\n--- STEP 4/4: skipped (--no-bookmarks) ---")

            final_size = os.path.getsize(final_path) / 1024 / 1024
            print(f"\nDone! Saved to: {final_path} ({final_size:.1f} MB)")
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
        # Don't delete preview files — viewer launches asynchronously on Windows
        # and may still be loading when this runs.
        if not is_preview:
            for f in ["temp_page.png", "temp_cleaned.png"]:
                if os.path.exists(f):
                    try: os.remove(f)
                    except OSError: pass

if __name__ == "__main__":
    main()
