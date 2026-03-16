import os
import sys
import re
import subprocess
from pypdf import PdfReader

# ================================================================
# PDF BOOKMARK SPLITTER - Native PDF output via pdftk / qpdf
# ================================================================
# PURPOSE:
#   Splits a PDF into individual files based on its bookmarks.
#   Pages are extracted natively (no re-encoding, no rasterisation).
#   At runtime you choose which bookmark depth level to split on;
#   pages that belong exclusively to deeper levels are skipped.
#
# PREREQUISITES:
#   1. Python 3.x
#   2. pypdf   (pip install pypdf)   -- used only to read bookmarks
#   3. pdftk   OR   qpdf             -- used for the actual splitting
#      Ubuntu/Debian : sudo apt install pdftk  /  sudo apt install qpdf
#      macOS         : brew install pdftk-java /  brew install qpdf
#      Windows       : https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
#
# USAGE:
#   python split_pdf.py "your_document.pdf"
#
# OUTPUT:
#   - A folder named after the PDF is created next to the input file.
#   - Files are named: [Folder]-[Sequence]-[Bookmark Title].pdf
# ================================================================


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

def clean_filename(title: str) -> str:
    """Keep only alphanumerics, spaces, and hyphens; collapse spaces."""
    clean = re.sub(r'[^a-zA-Z0-9 \-]', '', title)
    clean = re.sub(r' +', ' ', clean)
    return clean.strip()


def detect_tool() -> str:
    """Return 'pdftk' or 'qpdf' depending on what is installed."""
    for tool in ('pdftk', 'qpdf'):
        try:
            subprocess.run(
                [tool, '--version'],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )
            return tool
        except (FileNotFoundError, subprocess.CalledProcessError):
            continue
    return ''


def extract_pages_pdftk(input_path: str, out_path: str,
                         start: int, end: int) -> bool:
    """Extract pages [start, end] (1-based, inclusive) using pdftk."""
    cmd = ['pdftk', input_path, 'cat', f'{start}-{end}', 'output', out_path]
    result = subprocess.run(cmd, capture_output=True)
    return result.returncode == 0


def extract_pages_qpdf(input_path: str, out_path: str,
                        start: int, end: int) -> bool:
    """Extract pages [start, end] (1-based, inclusive) using qpdf."""
    cmd = ['qpdf', input_path, '--pages', '.', f'{start}-{end}', '--', out_path]
    result = subprocess.run(cmd, capture_output=True)
    return result.returncode == 0


# ------------------------------------------------------------------
# Bookmark tree parsing
# ------------------------------------------------------------------

def parse_outline_with_depth(reader: PdfReader) -> list[dict]:
    """
    Walk the PDF outline recursively and return a flat list of dicts:
        { 'title': str, 'page': int (0-based), 'depth': int (1-based) }
    The list is in document order.
    """
    entries: list[dict] = []

    def walk(nodes, depth: int) -> None:
        for node in nodes:
            if isinstance(node, list):
                # A plain list means these are children of the previous entry
                walk(node, depth + 1)
            else:
                try:
                    page = reader.get_destination_page_number(node)
                    entries.append({
                        'title': clean_filename(node.title),
                        'page':  page,
                        'depth': depth,
                    })
                except Exception:
                    pass

    walk(reader.outline, depth=1)
    return entries


def show_tree(entries: list[dict], max_examples: int = 5) -> None:
    """Print a visual preview of the bookmark hierarchy."""
    print()
    print('Bookmark structure (first few entries per level):')
    print('-' * 60)

    seen_per_depth: dict[int, int] = {}
    for e in entries:
        d = e['depth']
        seen_per_depth[d] = seen_per_depth.get(d, 0) + 1
        if seen_per_depth[d] <= max_examples:
            indent = '  ' * (d - 1)
            print(f'{indent}[depth {d}] {e["title"]}  (p.{e["page"] + 1})')
        elif seen_per_depth[d] == max_examples + 1:
            indent = '  ' * (d - 1)
            print(f'{indent}[depth {d}] ... (more)')

    print('-' * 60)
    max_depth = max(e['depth'] for e in entries)
    counts = {d: sum(1 for e in entries if e['depth'] == d)
              for d in range(1, max_depth + 1)}
    for d, n in counts.items():
        print(f'  Depth {d}: {n} bookmark(s)')
    print()


def prompt_depth(max_depth: int) -> int:
    """Ask the user which depth to split on."""
    while True:
        try:
            raw = input(f'Split at which depth? [1-{max_depth}]: ').strip()
            val = int(raw)
            if 1 <= val <= max_depth:
                return val
            print(f'  Please enter a number between 1 and {max_depth}.')
        except ValueError:
            print('  Please enter a valid integer.')
        except KeyboardInterrupt:
            raise


# ------------------------------------------------------------------
# Page-range computation: only target depth, skip everything else
# ------------------------------------------------------------------

def compute_segments(entries: list[dict],
                     target_depth: int,
                     num_pages: int) -> list[dict]:
    """
    Given the full bookmark list and a chosen depth, return segments
    corresponding only to bookmarks AT that depth.

    Pages introduced by any bookmark at a DIFFERENT depth (shallower
    or deeper) are skipped entirely -- they are not included in any
    output file.

    Each returned segment contains:
        { 'title': str, 'runs': list of (start, end) tuples (1-based, inclusive) }
    """
    # Collect only target-depth bookmarks, in order
    targets = [e for e in entries if e['depth'] == target_depth]
    if not targets:
        return []

    segments: list[dict] = []

    for i, bm in enumerate(targets):
        # Start at this bookmark's page, end just before the next target-depth
        # bookmark. All pages in between are included -- regardless of whether
        # deeper or shallower bookmarks also point into that range.
        start = bm['page']          # 0-based
        if i + 1 < len(targets):
            end = targets[i + 1]['page'] - 1    # 0-based inclusive
        else:
            end = num_pages - 1

        if end < start:
            end = start

        segments.append({
            'title': bm['title'] or 'Section',
            'runs':  [(start + 1, end + 1)],    # single contiguous run, 1-based
        })

    return segments


# ------------------------------------------------------------------
# Main processing
# ------------------------------------------------------------------

def process_pdf(input_path: str) -> None:
    if not os.path.exists(input_path):
        print(f'ERROR: File not found: {input_path}')
        return

    tool = detect_tool()
    if not tool:
        print('ERROR: Neither pdftk nor qpdf is installed.')
        print('  Ubuntu/Debian : sudo apt install pdftk   or   sudo apt install qpdf')
        print('  macOS         : brew install pdftk-java  or   brew install qpdf')
        sys.exit(1)

    print(f'Using tool : {tool}')
    print(f'Input file : {input_path}')

    reader    = PdfReader(input_path)
    num_pages = len(reader.pages)
    entries   = parse_outline_with_depth(reader)

    if not entries:
        print('RESULT: No bookmarks found in this PDF.')
        return

    # Show tree and ask which depth to use
    show_tree(entries)
    max_depth    = max(e['depth'] for e in entries)
    target_depth = prompt_depth(max_depth)

    # Compute what to extract
    segments = compute_segments(entries, target_depth, num_pages)
    if not segments:
        print(f'No bookmarks found at depth {target_depth}.')
        return

    # Prepare output folder
    base_name   = os.path.splitext(os.path.basename(input_path))[0]
    safe_folder = clean_filename(base_name) or 'output'
    output_dir  = os.path.join(os.path.dirname(os.path.abspath(input_path)),
                               safe_folder)
    os.makedirs(output_dir, exist_ok=True)

    print(f'\nExtracting {len(segments)} section(s) to: {output_dir}')
    print()

    for i, seg in enumerate(segments):
        title    = seg['title']
        runs     = seg['runs']
        out_name = f'{safe_folder}-{i + 1}-{title}.pdf'
        out_path = os.path.join(output_dir, out_name)

        pages_str = ', '.join(
            f'{s}-{e}' if s != e else str(s) for s, e in runs
        )
        label = f'({i + 1}/{len(segments)})'
        print(f'Writing {label}: {out_name}  [pages {pages_str}]')

        try:
            s, e = runs[0]
            ok = (extract_pages_pdftk(input_path, out_path, s, e)
                  if tool == 'pdftk'
                  else extract_pages_qpdf(input_path, out_path, s, e))
            if not ok:
                print(f'  SKIPPING: write error for {out_name}')

        except KeyboardInterrupt:
            raise
        except Exception as exc:
            print(f'  SKIPPING: {out_name} ({exc})')

    print('\nDone.')


# ------------------------------------------------------------------
# Entry point
# ------------------------------------------------------------------

def get_help() -> str:
    return """
================================================================
PDF BOOKMARK SPLITTER (native PDF - no rasterisation)
================================================================
USAGE:
  python split_pdf.py "your_document.pdf"

RUNTIME PROMPT:
  The script displays the bookmark tree and asks which depth
  level to split on.  Pages belonging to deeper levels are
  skipped entirely (not included in any output file).

CONTROLS:
  CTRL+C  to abort at any time.

OUTPUT:
  - Creates a folder named after the PDF.
  - Files: [Folder]-[Sequence]-[Bookmark Title].pdf
  - Pages are copied natively via pdftk or qpdf -- no image
    conversion, no quality loss.

REQUIREMENTS:
  pip install pypdf
  sudo apt install pdftk   (or qpdf as fallback)
================================================================
"""


if __name__ == '__main__':
    try:
        if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '/?'):
            print(get_help())
        else:
            process_pdf(sys.argv[1])
    except KeyboardInterrupt:
        print('\n\n[HALTED] Process stopped by user.')
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)
