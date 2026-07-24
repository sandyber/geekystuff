#!/usr/bin/env python3
"""cite2link.py -- turn pandoc-style citations into markdown hyperlinks.

Input: a markdown file with a YAML front matter block containing CSL
references (each with an `id` and a `url`), and citations in the body of
the form [@key], [@key, p. 987], [@key1; @key2, pp. 221-22].

For each citation the script:
  1. removes the [@...] bracket;
  2. wraps the preceding words (default 3) -- or the preceding quoted
     phrase, if the text before the bracket ends with a quotation --
     into a markdown link [anchor](url);
  3. appends an HTML comment <!-- @key, pages --> preserving the full
     original bracket content, so the anchor can be re-chosen later
     without losing the page references.

Multi-key citations get one link per key, on adjacent trailing chunks,
each with its own comment. Citations whose key has no url in the front
matter are left untouched, with a warning.

By default the `references`, `bibliography` and `link-citations` entries
are removed from the front matter (the links now carry the URLs); use
--keep-yaml to keep the front matter verbatim.

Usage:
  python3 cite2link.py input.md                 # writes input-linked.md
  python3 cite2link.py input.md -o out.md
  python3 cite2link.py input.md --words 4
  python3 cite2link.py input.md --stdout
"""

import argparse
import re
import sys

CITE_RE = re.compile(r'([ \t]*)\[(@[^\]]+)\]')
KEY_RE = re.compile(r'@([^\s,;]+)\s*,?\s*(.*)', re.S)
# punctuation that may follow the bracket and should precede the comment
TRAIL_PUNCT_RE = re.compile(r'^[.?!,;:]+')
# straight and curly double quotes
CLOSE_QUOTES = {'"': '"', '\u201d': '\u201c'}
BOUNDARY_END = ('.', ':', ';', '?', '!', ',')


def split_front_matter(text):
    """Return (front_matter_lines_without_fences, body, True) or (None, text, False)."""
    if not text.startswith('---'):
        return None, text, False
    lines = text.split('\n')
    if lines[0].strip() != '---':
        return None, text, False
    for i in range(1, len(lines)):
        if lines[i].strip() in ('---', '...'):
            return lines[1:i], '\n'.join(lines[i + 1:]), True
    return None, text, False


def urls_from_yaml(fm_lines):
    """Extract {id: url} from the references block.

    Tries PyYAML; falls back to a line-based reader good enough for
    CSL-YAML as commonly generated (one `id:` and one `url:` per entry).
    """
    try:
        import yaml
        data = yaml.safe_load('\n'.join(fm_lines)) or {}
        out = {}
        for ref in data.get('references', []) or []:
            rid, url = ref.get('id'), ref.get('url')
            if rid and url:
                out[str(rid)] = str(url)
        return out
    except ImportError:
        pass
    out, in_refs, cur = {}, False, {}

    def flush():
        if cur.get('id') and cur.get('url'):
            out[cur['id']] = cur['url']
        cur.clear()

    for line in fm_lines:
        if re.match(r'^references\s*:', line):
            in_refs = True
            continue
        if in_refs and re.match(r'^\S', line) and not line.startswith('- '):
            flush()
            in_refs = False
        if not in_refs:
            continue
        if line.startswith('- '):
            flush()
            line = '  ' + line[2:]
        m = re.match(r'^\s*(id|url)\s*:\s*(.+?)\s*$', line)
        if m:
            cur[m.group(1)] = m.group(2).strip('"\'')
    flush()
    return out


def strip_ref_block(fm_lines):
    """Drop references/bibliography/link-citations from front matter, textually."""
    out, skipping = [], False
    for line in fm_lines:
        top = re.match(r'^(\S[^:]*):', line)
        if skipping:
            if top and not line.startswith('- '):
                skipping = False
            else:
                continue
        if top and top.group(1).strip() in ('references', 'bibliography', 'link-citations'):
            skipping = True
            continue
        out.append(line)
    return out


def take_chunk(seg, n_words):
    """Split off an anchor chunk from the end of seg.

    Returns (rest, chunk). A closing quotation takes the whole quoted
    phrase; otherwise up to n_words whitespace-separated tokens, never
    crossing a newline, an earlier link/comment, or internal sentence
    punctuation.
    """
    end = len(seg)
    while end > 0 and seg[end - 1].isspace():
        if seg[end - 1] == '\n':
            return seg, ''
        end -= 1
    if end == 0:
        return seg, ''
    last = seg[end - 1]
    if last in CLOSE_QUOTES:
        i = seg.rfind(CLOSE_QUOTES[last], 0, end - 1)
        if i != -1:
            return seg[:i], seg[i:end]
    start, pos, count = end, end, 0
    while count < n_words:
        j = pos
        while j > 0 and not seg[j - 1].isspace():
            j -= 1
        token = seg[j:pos]
        if not token:
            break
        if '](' in token or '<!--' in token or '-->' in token:
            break
        if count > 0 and token.endswith(BOUNDARY_END):
            break
        start = j
        count += 1
        pos = j
        while pos > 0 and seg[pos - 1] in (' ', '\t'):
            pos -= 1
        if pos > 0 and seg[pos - 1] == '\n':
            break
    if start == end:
        return seg, ''
    return seg[:start], seg[start:end]


def convert(body, urls, n_words, warn):
    out, pos = [], 0
    for m in CITE_RE.finditer(body):
        seg = body[pos:m.start()]
        parts = [p.strip() for p in m.group(2).split(';') if p.strip()]
        keys = []
        ok = bool(parts)
        for part in parts:
            km = KEY_RE.match(part)
            if not km or km.group(1) not in urls:
                warn('no url for citation %r -- left unchanged' % part)
                ok = False
                break
            keys.append((part, urls[km.group(1)]))
        if not ok:
            out.append(body[pos:m.end()])
            pos = m.end()
            continue
        chunks, rest = [], seg
        for _ in keys:
            rest, chunk = take_chunk(rest, n_words)
            if not chunk:
                break
            chunks.append(chunk)
        if len(chunks) < len(keys):
            warn('no anchor words before %r -- left unchanged' % m.group(2))
            out.append(body[pos:m.end()])
            pos = m.end()
            continue
        chunks.reverse()  # restore citation order
        pieces = []
        for i, ((part, url), chunk) in enumerate(zip(keys, chunks)):
            link = '[%s](%s)' % (chunk, url)
            if i < len(keys) - 1:
                pieces.append(link + ' <!-- %s -->' % part)
            else:
                pieces.append(link)
        tail = body[m.end():]
        pm = TRAIL_PUNCT_RE.match(tail)
        punct = pm.group(0) if pm else ''
        out.append(rest + ' '.join(pieces) + punct +
                   ' <!-- %s -->' % keys[-1][0])
        pos = m.end() + len(punct)
    out.append(body[pos:])
    return ''.join(out)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('input')
    ap.add_argument('-o', '--output', help='output file (default: INPUT-linked.md)')
    ap.add_argument('--stdout', action='store_true', help='print to stdout')
    ap.add_argument('--words', type=int, default=3,
                    help='words per anchor when no quotation ends the sentence (default 3)')
    ap.add_argument('--keep-yaml', action='store_true',
                    help='keep the front matter verbatim, references block included')
    args = ap.parse_args()

    def warn(msg):
        print('cite2link: ' + msg, file=sys.stderr)

    with open(args.input, encoding='utf-8') as f:
        text = f.read()

    fm_lines, body, has_fm = split_front_matter(text)
    if not has_fm:
        warn('no YAML front matter found -- nothing to link against')
        sys.exit(1)
    urls = urls_from_yaml(fm_lines)
    if not urls:
        warn('no references with urls found in front matter')
        sys.exit(1)

    new_body = convert(body, urls, max(1, args.words), warn)
    header = fm_lines if args.keep_yaml else strip_ref_block(fm_lines)
    result = '---\n' + '\n'.join(header).strip('\n') + '\n---\n' + new_body

    if args.stdout:
        sys.stdout.write(result)
    else:
        out_path = args.output or re.sub(r'(\.[^.]+)?$', '-linked.md', args.input, count=1)
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(result)
        print('wrote ' + out_path)


if __name__ == '__main__':
    main()
