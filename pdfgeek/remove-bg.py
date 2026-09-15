#!/usr/bin/env python3
"""PDF background remover: clean + OCR + JBIG2 + bookmarks.

Pipeline:

  1. Ghostscript renders each page to raw PGM on a pipe (no PNG, no temp file),
     numpy thresholds it, Pillow writes a Group 4 TIFF.
  2. tesseract writes a text-only PDF per page (-c textonly_pdf=1), so no
     second rasterisation happens.
  3. jbig2enc compresses each TIFF, pikepdf assembles the image layer and
     stamps the text layer on top.
  4. pymupdf copies the source bookmarks onto the result.

Steps 1 to 3 all run per page inside one worker pool, so a page is rendered,
OCRed and compressed without waiting for the other pages to catch up.

Windows notes: keep the scratch folder off OneDrive (use --workdir C:\\temp\\pages)
and add Defender exclusions for the scratch folder, gswin64c.exe, tesseract.exe
and jbig2.exe. Both cost you a lot on a several-hundred-page book.
"""

import glob
import os
import platform
import shutil
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import numpy as np
from PIL import Image

IS_WINDOWS = platform.system() == "Windows"
# Stops a console window flashing per subprocess when run from pythonw.
CREATE_NO_WINDOW = 0x08000000 if IS_WINDOWS else 0

try:
    NO_DITHER = Image.Dither.NONE          # Pillow >= 9.1
except AttributeError:                      # pragma: no cover
    NO_DITHER = Image.NONE


# --------------------------------------------------------------------------
# small helpers
# --------------------------------------------------------------------------

_EXTRA_DIRS = None


def _extra_search_dirs():
    """Places Windows installers put things without touching PATH.

    The Tesseract installer and Ghostscript both register an install location
    and leave PATH alone, so a plain `which` misses them. Look in the same
    places the installers write to, so a working install is found even when
    the user never edited PATH.
    """
    global _EXTRA_DIRS
    if _EXTRA_DIRS is not None:
        return _EXTRA_DIRS

    dirs = []
    if IS_WINDOWS:
        try:
            import winreg

            def values(key):
                n = 0
                while True:
                    try:
                        yield winreg.EnumValue(key, n)
                    except OSError:
                        return
                    n += 1

            def subkeys(key):
                n = 0
                while True:
                    try:
                        yield winreg.EnumKey(key, n)
                    except OSError:
                        return
                    n += 1

            try:
                with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                    r"SOFTWARE\Tesseract-OCR") as k:
                    for name, val, _ in values(k):
                        if name == "InstallDir" and val:
                            dirs.append(val)
            except OSError:
                pass

            try:
                root = r"SOFTWARE\Artifex\GPL Ghostscript"
                with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, root) as k:
                    versions = sorted(subkeys(k), reverse=True)
                for ver in versions:
                    with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                        root + "\\" + ver) as k:
                        for _, val, _ in values(k):
                            if val:
                                dirs.append(os.path.join(val, "bin"))
            except OSError:
                pass
        except ImportError:
            pass

        for root in (os.environ.get("PROGRAMFILES"),
                     os.environ.get("PROGRAMFILES(X86)"),
                     os.environ.get("LOCALAPPDATA")):
            if not root:
                continue
            dirs.append(os.path.join(root, "Tesseract-OCR"))
            dirs.append(os.path.join(root, "jbig2enc"))
            dirs += sorted(glob.glob(os.path.join(root, "gs", "*", "bin")),
                           reverse=True)

    _EXTRA_DIRS = [d for d in dirs if os.path.isdir(d)]
    return _EXTRA_DIRS


def find_binary(*candidates):
    """Full path to the first candidate found, or None.

    Returns the path rather than the bare name, so a tool that only turns up
    in one of the extra directories still runs.
    """
    for name in candidates:
        found = shutil.which(name)
        if found:
            return found
    extra = _extra_search_dirs()
    if extra:
        joined = os.pathsep.join(extra)
        for name in candidates:
            found = shutil.which(name, path=joined)
            if found:
                return found
    return None


def child_env():
    """Environment for OCR children.

    tesseract uses OpenMP internally. With a worker pool running N of them at
    once, each spawning its own threads, they fight over the cores and the run
    gets slower. One thread per process, N processes, is the right shape.
    """
    env = dict(os.environ)
    env["OMP_THREAD_LIMIT"] = "1"
    return env


def run_cmd(cmd_list, env=None, stdout=None):
    """Run a command (as a list). Returns (ok, stderr_text)."""
    try:
        subprocess.run(cmd_list, check=True, capture_output=(stdout is None),
                       stdout=stdout, stderr=subprocess.PIPE if stdout else None,
                       env=env, creationflags=CREATE_NO_WINDOW)
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, e.stderr.decode(errors="replace") if e.stderr else ""


def human_mb(path):
    return os.path.getsize(path) / 1024 / 1024


# --------------------------------------------------------------------------
# stage 1: render and threshold
# --------------------------------------------------------------------------

def read_pgm(buf):
    """Parse a binary PGM (P5) into a 2-D uint8 array.

    Header is 'P5', width, height, maxval, separated by whitespace, with '#'
    comment lines allowed anywhere in it.
    """
    if not buf.startswith(b"P5"):
        raise ValueError("not a binary PGM")
    tokens, i = [], 2
    while len(tokens) < 3:
        while i < len(buf) and buf[i:i + 1].isspace():
            i += 1
        if buf[i:i + 1] == b"#":
            i = buf.index(b"\n", i) + 1
            continue
        j = i
        while j < len(buf) and not buf[j:j + 1].isspace():
            j += 1
        tokens.append(buf[i:j])
        i = j
    i += 1  # single whitespace byte after maxval
    w, h, maxval = int(tokens[0]), int(tokens[1]), int(tokens[2])
    if maxval > 255:
        raise ValueError("16-bit PGM not supported")
    return np.frombuffer(buf[i:i + w * h], dtype=np.uint8).reshape(h, w)


def render_gray(page_num, input_path, dpi_val, gs_bin, scratch_hint):
    """Render one page to a grayscale array via Ghostscript.

    Ghostscript writes raw PGM to stdout. That skips PNG compression, which
    was most of the render time, and skips the temp file entirely. If stdout
    comes back unusable (some Windows builds mangle binary stdout), fall back
    to writing a PGM file.
    """
    base = ["-sDEVICE=pgmraw", f"-r{dpi_val}",
            f"-dFirstPage={page_num}", f"-dLastPage={page_num}",
            "-q", "-dBATCH", "-dNOPAUSE", "-dSAFER",
            "-sstdout=%stderr"]  # keeps PostScript chatter out of the image
    try:
        r = subprocess.run([gs_bin, *base, "-o", "-", input_path],
                           check=True, capture_output=True,
                           creationflags=CREATE_NO_WINDOW)
        if not r.stdout:
            # gs exits 0 and prints nothing when the page doesn't exist.
            raise RuntimeError("gs produced no image (page out of range?)")
        return read_pgm(r.stdout)
    except subprocess.CalledProcessError as e:
        raise RuntimeError(
            f"gs failed: {e.stderr.decode(errors='replace')[:200]}") from e
    except ValueError:
        tmp = scratch_hint + ".pgm"
        try:
            ok, err = run_cmd([gs_bin, *base, "-o", tmp, input_path])
            if not ok:
                raise RuntimeError(f"gs failed: {err[:200]}")
            if not os.path.exists(tmp):
                raise RuntimeError("gs produced no image (page out of range?)")
            with open(tmp, "rb") as f:
                return read_pgm(f.read())
        finally:
            if os.path.exists(tmp):
                try:
                    os.remove(tmp)
                except OSError:
                    pass


def write_bilevel_tiff(arr, path, cut, dpi_val):
    """Threshold to bilevel and save as a Group 4 TIFF.

    `-white-threshold N% -type bilevel` in ImageMagick sends everything at or
    above N% of white to white and everything else to black, so one numpy
    comparison reproduces it. On a test page the two differ on 0.03% of pixels,
    all of them antialiased glyph edges, and the TIFFs come out the same size.

    The dpi tag matters: img2pdf and tesseract both size their pages from it,
    and Pillow writes no tag unless asked.
    """
    mono = np.where(arr >= cut, 255, 0).astype(np.uint8)
    Image.fromarray(mono, "L").convert("1", dither=NO_DITHER).save(
        path, compression="group4", dpi=(dpi_val, dpi_val))


# --------------------------------------------------------------------------
# stage 2: OCR to a text-only PDF
# --------------------------------------------------------------------------

def ocr_page(tif_path, out_base, tess_bin, lang, psm, oem, dpi_val):
    """Run tesseract over one TIFF, producing <out_base>.pdf with no image.

    textonly_pdf=1 gives a PDF holding only the invisible text layer, which
    then gets stamped onto the compressed image page. Handing tesseract the
    TIFF we already have avoids the second Ghostscript rasterisation that
    ocrmypdf does internally, worth about 30% of the OCR stage.
    """
    cmd = [tess_bin, tif_path, out_base, "-l", lang,
           "--psm", str(psm), "--dpi", str(dpi_val)]
    if oem is not None:
        cmd += ["--oem", str(oem)]
    cmd += ["-c", "textonly_pdf=1", "pdf"]
    ok, err = run_cmd(cmd, env=child_env())
    if not ok:
        return False, f"tesseract failed: {err.strip()[:160]}"
    if not os.path.exists(out_base + ".pdf"):
        return False, "tesseract produced no PDF"
    return True, ""


# --------------------------------------------------------------------------
# stage 3: JBIG2 compression
# --------------------------------------------------------------------------

def jbig2_generic(tif_path, out_path, jbig2_bin):
    """Lossless JBIG2 for one page. jbig2enc writes the stream to stdout.

    This is generic region coding, the same thing ocrmypdf does. Typically
    around half the size of Group 4 with no pixels changed.
    """
    try:
        with open(out_path, "wb") as f:
            subprocess.run([jbig2_bin, "--pdf", tif_path], check=True,
                           stdout=f, stderr=subprocess.PIPE,
                           creationflags=CREATE_NO_WINDOW)
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, f"jbig2 failed: {(e.stderr or b'').decode(errors='replace')[:160]}"


def jbig2_symbol_group(tif_paths, base, jbig2_bin):
    """Lossy JBIG2 over a group of pages: shared symbol dictionary.

    Much smaller than generic mode, because repeated glyphs are stored once.
    It is lossy in a specific way: glyphs that the matcher considers identical
    are drawn from one bitmap, so a bad match can swap one character for
    another. Fine for reading, risky for anything where a digit matters.

    Writes <base>.sym plus <base>.0000, <base>.0001 ... in group order.
    """
    ok, err = run_cmd([jbig2_bin, "-s", "-p", "-b", base, *tif_paths])
    if not ok:
        return False, f"jbig2 -s failed: {err.strip()[:160]}"
    return True, ""


# --------------------------------------------------------------------------
# stage 3b: assemble the PDF
# --------------------------------------------------------------------------

def build_image_pdf(records, out_path, dpi_val):
    """Build the image-only PDF from per-page records.

    Each record is a dict with 'tif' and optionally 'jb2' plus 'globals'.
    Pages carrying JBIG2 data are embedded as JBIG2Decode image XObjects;
    the rest fall back to img2pdf's lossless Group 4 embedding.
    """
    have_jbig2 = any(r.get("jb2") for r in records)
    if not have_jbig2:
        import img2pdf
        with open(out_path, "wb") as f:
            f.write(img2pdf.convert([r["tif"] for r in records]))
        return

    import pikepdf
    from pikepdf import Array, Dictionary, Name

    pdf = pikepdf.new()
    globals_cache = {}
    for rec in records:
        w, h = Image.open(rec["tif"]).size
        width_pt, height_pt = w * 72.0 / dpi_val, h * 72.0 / dpi_val

        if rec.get("jb2"):
            with open(rec["jb2"], "rb") as f:
                data = f.read()
            parms = None
            gpath = rec.get("globals")
            if gpath:
                if gpath not in globals_cache:
                    with open(gpath, "rb") as f:
                        globals_cache[gpath] = pdf.make_stream(f.read())
                parms = Dictionary(JBIG2Globals=globals_cache[gpath])
            img = pikepdf.Stream(pdf, b"")
            img.write(data, filter=Name.JBIG2Decode, decode_parms=parms)
        else:
            # This page has no JBIG2 data (jbig2enc choked on it, say). Pack the
            # bits and deflate them: bigger than Group 4, but it can't get the
            # polarity wrong, and it only ever applies to the odd stray page.
            import zlib
            with Image.open(rec["tif"]) as im:
                bits = np.asarray(im.convert("1"), dtype=bool)
            img = pikepdf.Stream(pdf, b"")
            img.write(zlib.compress(np.packbits(bits, axis=1).tobytes()),
                      filter=Name.FlateDecode)

        img.Type = Name.XObject
        img.Subtype = Name.Image
        img.Width = w
        img.Height = h
        img.ColorSpace = Name.DeviceGray
        img.BitsPerComponent = 1

        content = pdf.make_stream(
            f"q {width_pt:.4f} 0 0 {height_pt:.4f} 0 0 cm /Im0 Do Q".encode())
        page = pikepdf.Dictionary(
            Type=Name.Page,
            MediaBox=Array([0, 0, width_pt, height_pt]),
            Contents=content,
            Resources=Dictionary(XObject=Dictionary(Im0=img)))
        pdf.pages.append(pikepdf.Page(pdf.make_indirect(page)))

    pdf.save(out_path)


def overlay_text(image_pdf, text_pdfs, out_path):
    """Stamp each text-only page onto the matching image page.

    pikepdf copies the streams rather than re-encoding them, so the JBIG2
    image data passes through untouched.
    """
    import pikepdf
    stamped = 0
    with pikepdf.open(image_pdf) as pdf:
        for i, tpath in enumerate(text_pdfs):
            if not tpath or i >= len(pdf.pages):
                continue
            try:
                with pikepdf.open(tpath) as tp:
                    pdf.pages[i].add_overlay(tp.pages[0])
                stamped += 1
            except Exception:
                pass  # a page without text is still a usable page
        pdf.save(out_path)
    return stamped


# --------------------------------------------------------------------------
# stage 4: bookmarks
# --------------------------------------------------------------------------

def _remap_toc_detailed(toc, page_map):
    out = []
    for lvl, title, page, dest in toc:
        new_page = page_map.get(page)
        if new_page is None:
            continue
        new_dest = dict(dest) if isinstance(dest, dict) else dest
        if isinstance(new_dest, dict) and "page" in new_dest:
            mapped = page_map.get(new_dest["page"] + 1)
            if mapped is None:
                continue
            new_dest["page"] = mapped - 1
        out.append([lvl, title, new_page, new_dest])
    return out


def _normalize_toc_levels(toc):
    """set_toc wants each level to be at most prev + 1. Dropping parents can
    leave orphaned children, so clamp as we go."""
    out, prev = [], 0
    for entry in toc:
        entry = list(entry)
        entry[0] = min(entry[0], prev + 1)
        prev = entry[0]
        out.append(entry)
    return out


def transfer_bookmarks(source, target, page_map, copy_metadata=True):
    """Copy the source outline onto the cleaned PDF, in place."""
    try:
        import pymupdf
    except ImportError:
        return False, "pymupdf not installed (pip install pymupdf)"

    src = dst = None
    try:
        src = pymupdf.open(source)
        dst = pymupdf.open(target)

        if copy_metadata:
            meta = {k: v for k, v in (src.metadata or {}).items()
                    if k in ("title", "author", "subject", "keywords") and v}
            if meta:
                dst.set_metadata(meta)

        toc = src.get_toc(simple=False)
        if not toc:
            if copy_metadata:
                _save_in_place(dst, target)
            return True, "source has no bookmarks; nothing to transfer"

        original_count = len(toc)
        toc = _remap_toc_detailed(toc, page_map)
        dropped = original_count - len(toc)
        if not toc:
            return False, f"no bookmarks land on preserved pages (had {original_count})"

        toc = [e for e in toc if 1 <= e[2] <= dst.page_count]
        toc = _normalize_toc_levels(toc)
        dst.set_toc(toc)
        _save_in_place(dst, target)

        msg = f"wrote {len(toc)} bookmark(s)"
        if dropped:
            msg += f"; dropped {dropped} pointing at unprocessed pages"
        return True, msg
    except Exception as e:
        return False, f"transfer failed: {e}"
    finally:
        for d in (src, dst):
            try:
                d.close()
            except Exception:
                pass


def _save_in_place(doc, path):
    tmp = path + ".tmp_toc"
    doc.save(tmp)
    doc.close()
    os.replace(tmp, path)


# --------------------------------------------------------------------------
# per-page work
# --------------------------------------------------------------------------

def process_page(page_num, slot, ctx):
    """Render, threshold, OCR and compress one page.

    Everything here is independent of every other page, so the pool never has
    to wait at a stage boundary.
    """
    padded = f"{slot:04d}"
    tif = os.path.join(ctx["output_dir"], f"page_{padded}.tif")
    txt_base = os.path.join(ctx["output_dir"], f"text_{padded}")
    jb2 = os.path.join(ctx["output_dir"], f"page_{padded}.jb2")
    timings = {}

    t0 = time.time()
    try:
        arr = render_gray(page_num, ctx["input_path"], ctx["dpi_val"],
                          ctx["gs_bin"], os.path.join(ctx["output_dir"], f"_tmp_{padded}"))
        write_bilevel_tiff(arr, tif, ctx["cut"], ctx["dpi_val"])
        del arr
    except Exception as e:
        return page_num, slot, False, f"{type(e).__name__}: {e}", timings
    timings["clean"] = time.time() - t0

    if ctx["do_ocr"]:
        t0 = time.time()
        ok, err = ocr_page(tif, txt_base, ctx["tess_bin"], ctx["lang"],
                           ctx["psm"], ctx["oem"], ctx["dpi_val"])
        timings["ocr"] = time.time() - t0
        if not ok:
            return page_num, slot, False, err, timings

    if ctx["jbig2_bin"] and not ctx["jbig2_lossy"]:
        t0 = time.time()
        ok, err = jbig2_generic(tif, jb2, ctx["jbig2_bin"])
        timings["jbig2"] = time.time() - t0
        if not ok:
            return page_num, slot, True, err, timings  # keep the page, keep G4

    return page_num, slot, True, "", timings


# --------------------------------------------------------------------------
# dependencies
# --------------------------------------------------------------------------

def install_python_deps():
    packages = {"numpy": "numpy", "PIL": "pillow", "img2pdf": "img2pdf",
                "pymupdf": "pymupdf", "pikepdf": "pikepdf"}
    needed = []
    for mod, pkg in packages.items():
        try:
            __import__(mod)
            print(f"  [skip] {pkg} already installed")
        except ImportError:
            needed.append(pkg)

    if needed:
        print(f"--- Installing: {', '.join(needed)} ---")
        base = [sys.executable, "-m", "pip", "install", "--upgrade"]
        ok, err = run_cmd(base + needed)
        if not ok:
            print("  [!] first attempt failed; retrying with --user ...")
            ok, err = run_cmd(base + ["--user"] + needed)
        if not ok:
            print("  [!] pip install failed:")
            print(err)
            sys.exit(1)
        print("  -> installed.")
    else:
        print("All Python dependencies already present.")

    missing = []
    if not find_binary("gswin64c", "gs"):
        missing.append("Ghostscript")
    if not find_binary("tesseract"):
        missing.append("tesseract (OCR)")
    if not find_binary("jbig2"):
        missing.append("jbig2enc (optional: without it pages stay Group 4, "
                       "roughly twice the size)")
    if missing:
        print("\n[note] Not pip-installable, install these yourself:")
        for m in missing:
            print(f"        - {m}")


def print_help(script_name):
    print("\n" + "=" * 75)
    print("       PDF BACKGROUND REMOVER (clean + OCR + JBIG2 + bookmarks)")
    print("=" * 75)
    print("USAGE:")
    print(f"  python {script_name} <file.pdf> <pages> [options]")
    print(f"  python {script_name} --install-deps")
    print("\nEXAMPLES:")
    print(f"  python {script_name} Ethics.pdf 1-741")
    print(f"  python {script_name} Ethics.pdf 5 --preview")
    print(f"  python {script_name} Ethics.pdf 1-741 --threshold 65 --dpi 400")
    print(f"  python {script_name} Ethics.pdf 1-741 --psm 6 --workers 8")
    print(f"  python {script_name} Ethics.pdf 7-10,297-298 --jbig2-lossy")
    print("\nIMAGE FLAGS:")
    print("  --threshold N   white-threshold percent (default 40)")
    print("  --dpi N         render resolution (default 300)")
    print("  --preview       render one page and open it, for tuning")
    print("\nOCR FLAGS:")
    print("  --lang CODE     tesseract language (default eng)")
    print("  --psm N         page segmentation mode (default 3)")
    print("                  6 is ~10% faster on single-column body text but")
    print("                  hurts multi-column pages and pages with figures")
    print("  --oem N         OCR engine mode; leave unset for the default")
    print("  --no-ocr        skip OCR, produce an image-only PDF")
    print("\nCOMPRESSION FLAGS:")
    print("  --jbig2-lossy   symbol mode: much smaller, but visually identical")
    print("                  glyphs share one bitmap, so a bad match can swap a")
    print("                  character. Avoid for anything with figures in it.")
    print("  --jbig2-group N pages per symbol dictionary in lossy mode (default 10)")
    print("  --no-jbig2      leave pages as Group 4, and don't ask about it")
    print("\nOTHER FLAGS:")
    print("  --workers N     parallel workers (default: logical CPU count)")
    print("  --keep-tiffs    keep the intermediate page TIFFs")
    print("  --no-bookmarks  skip copying the source bookmark outline")
    print("  --workdir DIR   scratch folder (default: <file>_cleaned_pages)")
    print("  --shared-dir    use ./cleaned_pages instead")
    print("  --timings       print a per-stage breakdown at the end")
    print("  --install-deps  install the Python packages, then continue")
    print("\nPIPELINE:")
    print("  1. gs -> raw PGM on a pipe -> numpy threshold -> Group 4 TIFF")
    print("  2. tesseract -> text-only PDF per page (no second rasterisation)")
    print("  3. jbig2enc + pikepdf -> compressed image layer, text stamped on")
    print("  4. pymupdf -> bookmarks and metadata copied from the source")
    print("  Steps 1 to 3 run per page in one worker pool.")
    print("\nREQUIRED (auto-detected on PATH):")
    print("  Ghostscript (gswin64c on Windows, gs elsewhere)")
    print("  tesseract   (skip with --no-ocr)")
    print("  jbig2enc    (optional to run, but without it pages stay Group 4,")
    print("               roughly twice the size; the script asks before it")
    print("               continues without it. --no-jbig2 skips the question)")
    print("  python: numpy, pillow, pikepdf, img2pdf, pymupdf")
    print("=" * 75 + "\n")


# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------

def main():
    script_name = os.path.basename(sys.argv[0])

    if "--install-deps" in sys.argv:
        install_python_deps()
        if len(sys.argv) < 3:
            return

    if len(sys.argv) < 3 or "--help" in sys.argv or "-h" in sys.argv:
        print_help(script_name)
        return

    input_path = sys.argv[1]
    if not os.path.exists(input_path):
        print(f"[!] File not found: {input_path}")
        return
    raw_pages = sys.argv[2]

    # defaults
    threshold_val, dpi_val = 40.0, 300
    lang, psm, oem = "eng", 3, None
    workers = max(1, os.cpu_count() or 4)
    jbig2_group = 10
    is_preview = "--preview" in sys.argv
    keep_tiffs = "--keep-tiffs" in sys.argv
    do_bookmarks = "--no-bookmarks" not in sys.argv
    do_ocr = "--no-ocr" not in sys.argv
    no_jbig2 = "--no-jbig2" in sys.argv
    jbig2_lossy = "--jbig2-lossy" in sys.argv
    show_timings = "--timings" in sys.argv
    shared_dir = "--shared-dir" in sys.argv
    workdir = None

    flagless = {"--preview", "--keep-tiffs", "--no-bookmarks", "--no-ocr",
                "--no-jbig2", "--jbig2-lossy", "--timings", "--shared-dir",
                "--install-deps"}
    valued = {"--threshold": "threshold", "--dpi": "dpi", "--workers": "workers",
              "--workdir": "workdir", "--lang": "lang", "--psm": "psm",
              "--oem": "oem", "--jbig2-group": "jbig2_group"}

    rest = sys.argv[3:]
    i = 0
    while i < len(rest):
        arg = rest[i]
        if arg in flagless:
            i += 1
            continue
        if not arg.startswith("--"):
            print(f"[!] Unrecognized argument: {arg}")
            return
        if "=" in arg:
            key, _, value = arg.partition("=")
            step = 1
        else:
            key, value = arg, (rest[i + 1] if i + 1 < len(rest) else None)
            step = 2
        if key not in valued:
            print(f"[!] Unknown flag: {arg}")
            return
        if value is None:
            print(f"[!] {key} needs a value")
            return
        name = valued[key]
        try:
            if name == "threshold":
                threshold_val = float(value)
            elif name == "dpi":
                dpi_val = int(value)
            elif name == "workers":
                workers = max(1, int(value))
            elif name == "psm":
                psm = int(value)
            elif name == "oem":
                oem = int(value)
            elif name == "jbig2_group":
                jbig2_group = max(1, int(value))
            elif name == "lang":
                lang = value
            elif name == "workdir":
                workdir = value
        except ValueError:
            print(f"[!] {key} expects a number, got: {value!r}")
            return
        i += step

    # binaries
    gs_bin = find_binary("gswin64c", "gs")
    tess_bin = find_binary("tesseract")
    jbig2_bin = None if no_jbig2 else find_binary("jbig2")
    if not gs_bin:
        print("[!] Ghostscript not found on PATH (gswin64c / gs)")
        return
    if do_ocr and not tess_bin:
        print("[!] tesseract not found on PATH, in the registry, or under "
              "Program Files.")
        print("    Install it, add its folder to PATH, or pass --no-ocr")
        return
    # jbig2enc is optional to run but not optional to actually shrink anything.
    # Without it the pages stay as the Group 4 TIFFs from step 1, roughly twice
    # the size, and on a source that was already well compressed the output can
    # come out larger than the input. Say so before the run, not after.
    # --preview stops after one page and never compresses anything, and
    # --no-jbig2 is a deliberate choice, so neither one asks.
    if not no_jbig2 and not jbig2_bin and not is_preview:
        print("[!] jbig2enc (jbig2) not found on PATH.")
        print("    Pages will stay Group 4, roughly twice the size, and on a")
        print("    source that is already compressed the output may end up")
        print("    BIGGER than the input. Install jbig2enc for real shrinkage,")
        print("    or pass --no-jbig2 to skip this question.")
        try:
            answer = input("    Proceed anyway? [y/N] ").strip().lower()
        except EOFError:
            # No console to answer on (piped or scheduled run). The likely
            # outcome is a bloated file, so stop rather than burn the whole run.
            print("    (stdin not interactive; aborting)")
            return
        except KeyboardInterrupt:
            print("")
            print("    Aborted.")
            return
        if answer not in ("y", "yes"):
            print("    Aborted.")
            return
        print("")

    input_dir = os.path.dirname(input_path) or "."
    base_name = os.path.splitext(os.path.basename(input_path))[0]
    final_path = os.path.join(input_dir, f"{base_name}_cleaned.pdf")

    auto_dir = False
    if workdir:
        output_dir = os.path.normpath(workdir)
    elif shared_dir:
        output_dir = "cleaned_pages"
    else:
        output_dir = os.path.normpath(
            os.path.join(input_dir, f"{base_name}_cleaned_pages"))
        auto_dir = True

    cut = int(round(255 * threshold_val / 100.0))
    preview_out = os.path.join(output_dir, f"_preview_{base_name}.png")

    try:
        os.makedirs(output_dir, exist_ok=True)
        print(f"Working folder: {output_dir}")

        page_list = []
        for part in raw_pages.split(","):
            part = part.strip()
            if "-" in part:
                start, end = map(int, part.split("-"))
                page_list.extend(range(start, end + 1))
            elif part:
                page_list.append(int(part))
        if not page_list:
            print("[!] No pages requested.")
            return

        # ---- preview ----
        if is_preview:
            p = page_list[0]
            print(f"--- PREVIEW (page {p}, threshold {threshold_val}%, dpi {dpi_val}) ---")
            arr = render_gray(p, input_path, dpi_val, gs_bin,
                              os.path.join(output_dir, "_preview"))
            Image.fromarray(np.where(arr >= cut, 255, 0).astype(np.uint8),
                            "L").save(preview_out)
            print(f"Opening {preview_out} ... adjust --threshold if needed.")
            try:
                if IS_WINDOWS:
                    os.startfile(os.path.abspath(preview_out))
                elif platform.system() == "Darwin":
                    subprocess.run(["open", preview_out])
                else:
                    subprocess.run(["xdg-open", preview_out])
            except Exception:
                print("  (no viewer launched; open the file yourself)")
            return

        print(f"Cleaning leftovers in {output_dir} ...")
        for pattern in ("page_*.tif", "page_*.jb2", "text_*.pdf",
                        "group_*.sym", "group_*.[0-9][0-9][0-9][0-9]"):
            for f in glob.glob(os.path.join(output_dir, pattern)):
                os.remove(f)

        ctx = dict(input_path=input_path, output_dir=output_dir, dpi_val=dpi_val,
                   cut=cut, gs_bin=gs_bin, tess_bin=tess_bin, lang=lang,
                   psm=psm, oem=oem, do_ocr=do_ocr, jbig2_bin=jbig2_bin,
                   jbig2_lossy=jbig2_lossy)

        # ---- steps 1 to 3, per page ----
        stages = "clean" + (" + OCR" if do_ocr else "") + \
                 (" + JBIG2" if jbig2_bin and not jbig2_lossy else "")
        print(f"\n--- STEP 1/3: {stages} for {len(page_list)} pages, "
              f"{workers} workers (dpi {dpi_val}, threshold {threshold_val}%) ---")

        run_start = time.time()
        totals = {"clean": 0.0, "ocr": 0.0, "jbig2": 0.0}
        done, failed, good_slots = 0, [], []
        with ThreadPoolExecutor(max_workers=workers) as ex:
            futures = {ex.submit(process_page, p, slot, ctx): p
                       for slot, p in enumerate(page_list, start=1)}
            try:
                for fut in as_completed(futures):
                    page_num, slot, ok, err, timings = fut.result()
                    done += 1
                    for k, v in timings.items():
                        totals[k] = totals.get(k, 0.0) + v
                    line = (f"  [{done:>4}/{len(page_list)}] page {page_num:>4} "
                            f"{'OK' if ok else 'FAIL'}")
                    if err:
                        line += f"  {err.strip()[:80]}"
                    if ok:
                        good_slots.append((slot, page_num))
                    else:
                        failed.append(page_num)
                    print(line)
            except KeyboardInterrupt:
                print("\n[!] Interrupt: cancelling pending pages ...")
                ex.shutdown(wait=False, cancel_futures=True)
                raise
        step1 = time.time() - run_start

        if failed:
            print(f"\n[!] {len(failed)} page(s) failed: {failed}")
            print("    Continuing with the pages that succeeded.")
        if not good_slots:
            print("[!] No pages produced. Aborting.")
            return

        good_slots.sort()
        records = []
        for slot, page_num in good_slots:
            padded = f"{slot:04d}"
            rec = {"tif": os.path.join(output_dir, f"page_{padded}.tif"),
                   "page": page_num, "slot": slot}
            jb2 = os.path.join(output_dir, f"page_{padded}.jb2")
            if os.path.exists(jb2) and os.path.getsize(jb2) > 0:
                rec["jb2"] = jb2
            tpdf = os.path.join(output_dir, f"text_{padded}.pdf")
            rec["text"] = tpdf if os.path.exists(tpdf) else None
            records.append(rec)

        # ---- lossy JBIG2 runs over groups, so it needs its own pass ----
        if jbig2_bin and jbig2_lossy:
            groups = [records[i:i + jbig2_group]
                      for i in range(0, len(records), jbig2_group)]
            print(f"\n--- JBIG2 symbol mode: {len(groups)} group(s) of "
                  f"up to {jbig2_group} pages ---")
            t0 = time.time()

            def do_group(idx_group):
                idx, group = idx_group
                base = os.path.join(output_dir, f"group_{idx:04d}")
                return idx, group, jbig2_symbol_group(
                    [r["tif"] for r in group], base, jbig2_bin), base

            with ThreadPoolExecutor(max_workers=workers) as ex:
                for idx, group, (ok, err), base in ex.map(
                        do_group, enumerate(groups)):
                    if not ok:
                        print(f"  [!] group {idx}: {err}")
                        continue
                    for n, rec in enumerate(group):
                        part = f"{base}.{n:04d}"
                        if os.path.exists(part):
                            rec["jb2"] = part
                            rec["globals"] = base + ".sym"
            totals["jbig2"] += time.time() - t0
            print(f"  -> {time.time() - t0:.1f}s")

        # ---- assemble ----
        print(f"\n--- STEP 2/3: assembling {len(records)} pages ---")
        t0 = time.time()
        image_pdf = os.path.join(output_dir, "_image_layer.pdf")
        build_image_pdf(records, image_pdf, dpi_val)
        print(f"  image layer: {human_mb(image_pdf):.1f} MB")

        if do_ocr and any(r["text"] for r in records):
            stamped = overlay_text(image_pdf, [r["text"] for r in records],
                                   final_path)
            print(f"  text layer stamped on {stamped} page(s)")
            os.remove(image_pdf)
        else:
            os.replace(image_pdf, final_path)
        assemble_t = time.time() - t0

        # ---- bookmarks ----
        if do_bookmarks:
            page_map = {rec["page"]: n for n, rec in enumerate(records, start=1)}
            print(f"\n--- STEP 3/3: bookmarks ({len(page_map)} pages mapped) ---")
            ok_bm, msg = transfer_bookmarks(input_path, final_path, page_map)
            print(f"{'  ->' if ok_bm else '  [!]'} {msg}")
            if not ok_bm:
                print("       (the PDF is fine, it just lacks bookmarks)")
        else:
            print("\n--- STEP 3/3: bookmarks skipped (--no-bookmarks) ---")

        # ---- tidy up ----
        if not keep_tiffs:
            for pattern in ("page_*.tif", "page_*.jb2", "text_*.pdf",
                            "group_*.sym", "group_*.[0-9][0-9][0-9][0-9]"):
                for f in glob.glob(os.path.join(output_dir, pattern)):
                    try:
                        os.remove(f)
                    except OSError:
                        pass
        if auto_dir and not os.listdir(output_dir):
            try:
                os.rmdir(output_dir)
            except OSError:
                pass

        wall = time.time() - run_start
        n = len(records)
        print(f"\nDone! Saved to: {final_path} ({human_mb(final_path):.1f} MB, "
              f"{human_mb(final_path) * 1024 / max(n, 1):.0f} KB/page)")
        print(f"Wall time {wall:.1f}s for {n} pages ({wall / max(n, 1):.2f}s/page).")
        if show_timings:
            print("\nCPU time per stage (summed over workers):")
            for k in ("clean", "ocr", "jbig2"):
                if totals.get(k):
                    print(f"  {k:<6} {totals[k]:8.1f}s "
                          f"({totals[k] / max(n, 1):.2f}s/page)")
            print(f"  {'assemble':<6} {assemble_t:6.1f}s")
            print(f"  step 1 wall {step1:.1f}s with {workers} workers")

    except KeyboardInterrupt:
        print(f"\n\n[!] STOPPED. Partial pages remain in {output_dir}")


if __name__ == "__main__":
    main()
