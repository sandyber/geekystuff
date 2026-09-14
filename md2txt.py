#!/usr/bin/env python3
"""md2txt.py -- flatten a markdown file to plain text.

Drops the YAML front matter, empties HTML comments (including the
<!-- @key, pages --> markers left by cite2link.py), unwraps links,
images and emphasis, and removes the remaining markdown furniture:
headers, code fences, blockquote marks, reference-link definitions,
horizontal rules.

A comment that follows text on a line is removed together with the
whitespace before it, so "word. <!-- @kant -->" comes out as "word."
A line that holds nothing but a comment is dropped entirely, so it
does not leave a spurious paragraph break.

Usage:
  python3 md2txt.py input.md               # writes input.txt
  python3 md2txt.py input.md -o out.txt
  python3 md2txt.py input.md --stdout
  python3 md2txt.py input.md --reflow      # one paragraph per line
  python3 md2txt.py input.md --ascii       # fold curly quotes/dashes to ASCII
  python3 md2txt.py input.md --keep-yaml   # keep the front matter block

Fenced code blocks keep their contents verbatim (only the fence lines
are removed). Inline transformations are line-local, so emphasis or a
link split across a line break is left as-is.
"""

import argparse
import re
import sys

COMMENT_RE = re.compile(r'<!--.*?-->', re.S)
PLACEHOLDER = '\x00'

FENCE_RE = re.compile(r'^\s*(```|~~~)')
HR_RE = re.compile(r'^\s{0,3}(?:-{3,}|\*{3,}|_{3,}|={3,})\s*$')
REFDEF_RE = re.compile(r'^\s{0,3}\[[^\]]+\]:\s+\S+')
ATX_RE = re.compile(r'^\s{0,3}#{1,6}\s+')
BLOCKQUOTE_RE = re.compile(r'^\s{0,3}(?:>\s?)+')
LIST_RE = re.compile(r'^\s*(?:[-*+]|\d+[.)])\s+')

INLINE_SUBS = [
    (re.compile(r'!\[([^\]]*)\]\([^)]*\)'), r'\1'),          # images
    (re.compile(r'\[([^\]]*)\]\([^)]*\)'), r'\1'),           # inline links
    (re.compile(r'\[([^\]]+)\]\[[^\]]*\]'), r'\1'),          # reference links
    (re.compile(r'<([a-zA-Z][a-zA-Z0-9+.-]*://[^>\s]+)>'), r'\1'),  # autolinks
    (re.compile(r'\*\*(.+?)\*\*'), r'\1'),                   # bold
    (re.compile(r'__(.+?)__'), r'\1'),
    (re.compile(r'\*(?!\s)(.+?)(?<![\s*])\*'), r'\1'),       # italics
    (re.compile(r'(?<![A-Za-z0-9_])_(?!\s)(.+?)(?<![\s_])_(?![A-Za-z0-9_])'),
     r'\1'),
    (re.compile(r'`([^`]+)`'), r'\1'),                       # inline code
    (re.compile(r'\\([\\`*_{}\[\]()#+.!>-])'), r'\1'),       # escapes
]

ASCII_MAP = {
    '\u2018': "'", '\u2019': "'",
    '\u201c': '"', '\u201d': '"',
    '\u2013': '-', '\u2014': '--',
    '\u2026': '...', '\u00a0': ' ',
}


def split_front_matter(text):
    """Return (body, True) with the front matter cut off, or (text, False)."""
    if not text.startswith('---'):
        return text, False
    lines = text.split('\n')
    if lines[0].strip() != '---':
        return text, False
    for i in range(1, len(lines)):
        if lines[i].strip() in ('---', '...'):
            return '\n'.join(lines[i + 1:]), True
    return text, False


def empty_comments(text):
    """Remove every <!-- ... --> together with the whitespace before it.

    Comments are first replaced by a placeholder so that lines holding
    nothing else can be dropped whole. A comment spanning several lines
    joins its surrounding text onto one line.
    """
    text = COMMENT_RE.sub(PLACEHOLDER, text)
    out = []
    for line in text.split('\n'):
        if PLACEHOLDER in line:
            if not line.replace(PLACEHOLDER, '').strip():
                continue                     # comment-only line: drop it
            line = re.sub(r'^(?:%s[ \t]*)+' % PLACEHOLDER, '', line)
            line = re.sub(r'[ \t]*%s' % PLACEHOLDER, '', line)
        out.append(line.rstrip())
    return '\n'.join(out)


def transform_lines(text):
    """Apply block- and inline-level markdown removal, line by line."""
    out, in_fence = [], False
    for line in text.split('\n'):
        if FENCE_RE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            out.append(line)
            continue
        if REFDEF_RE.match(line) or HR_RE.match(line):
            continue
        line = ATX_RE.sub('', line)
        line = re.sub(r'\s+#+\s*$', '', line)    # trailing ATX hashes
        line = BLOCKQUOTE_RE.sub('', line)
        for rx, rep in INLINE_SUBS:
            line = rx.sub(rep, line)
        out.append(line.rstrip())
    return '\n'.join(out)


def collapse_blanks(text):
    return re.sub(r'\n{3,}', '\n\n', text).strip('\n') + '\n'


def reflow(text):
    """Join the wrapped lines of each paragraph into a single line.

    A blank line or a list marker starts a new output line.
    """
    out, cur = [], None
    for line in text.split('\n'):
        if not line.strip():
            if cur is not None:
                out.append(cur)
                cur = None
            out.append('')
        elif cur is None or LIST_RE.match(line):
            if cur is not None:
                out.append(cur)
            cur = line
        else:
            cur = cur + ' ' + line.strip()
    if cur is not None:
        out.append(cur)
    return '\n'.join(out)


def fold_ascii(text):
    for src, dst in ASCII_MAP.items():
        text = text.replace(src, dst)
    return text


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('input')
    ap.add_argument('-o', '--output', help='output file (default: INPUT.txt)')
    ap.add_argument('--stdout', action='store_true', help='print to stdout')
    ap.add_argument('--reflow', action='store_true',
                    help='join wrapped lines: one paragraph per line')
    ap.add_argument('--ascii', action='store_true',
                    help='fold curly quotes, dashes and ellipses to ASCII')
    ap.add_argument('--keep-yaml', action='store_true',
                    help='keep the YAML front matter block')
    args = ap.parse_args()

    with open(args.input, encoding='utf-8') as f:
        text = f.read()

    if not args.keep_yaml:
        text, had_fm = split_front_matter(text)
        if not had_fm:
            print('md2txt: no YAML front matter found', file=sys.stderr)

    n_comments = len(COMMENT_RE.findall(text))
    text = empty_comments(text)
    text = transform_lines(text)
    text = collapse_blanks(text)
    if args.reflow:
        text = reflow(text)
    if args.ascii:
        text = fold_ascii(text)

    if args.stdout:
        sys.stdout.write(text)
    else:
        out_path = args.output or re.sub(r'\.md$', '', args.input,
                                         flags=re.I) + '.txt'
        with open(out_path, 'w', encoding='utf-8', newline='\r\n') as f:
            f.write(text)
        print('wrote %s  (emptied %d comment%s)'
              % (out_path, n_comments, '' if n_comments == 1 else 's'))


if __name__ == '__main__':
    main()
