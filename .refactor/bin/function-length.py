#!/usr/bin/env python3
"""Measure Elisp function length via paren-balancing, not naive '(defun ' spans.

CPS macros (defun/k, defun/done, defun/3k) are top-level definers too; a scan
keying only on '(defun ' merges runs of small CPS functions into phantom
oversized ones. This walks balanced top-level forms directly.
"""
import re
import sys
from pathlib import Path

DEFINERS = re.compile(
    r'^\((defun|defun/k|defun/done|defun/3k|defsubst)\s+([A-Za-z0-9!?_*+./:-]+)'
)


def mask(text):
    chars = list(text)
    string = escape = comment = False
    for i, c in enumerate(text):
        if comment:
            if c == '\n':
                comment = False
        elif string:
            if escape:
                escape = False
            elif c == '\\':
                escape = True
            elif c == '"':
                string = False
        elif c == ';':
            comment = True
            chars[i] = ' '
        elif c == '"':
            string = True
        elif c == '\\':
            if i + 1 < len(text) and text[i + 1] == '?':
                pass
        j = i
    return ''.join(chars)


def top_level_forms(text):
    """Yield (start_offset, end_offset) of each balanced top-level form."""
    out = []
    depth = 0
    start = None
    string = escape = comment = char_escape = False
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if comment:
            if c == '\n':
                comment = False
        elif string:
            if escape:
                escape = False
            elif c == '\\':
                escape = True
            elif c == '"':
                string = False
        elif c == ';':
            comment = True
        elif c == '"':
            string = True
            if depth == 0 and start is None:
                pass
        elif c == '?' and i + 1 < n:
            # char literal like ?a or ?\n -- skip the escaped char so its
            # paren-like content (rare) doesn't perturb depth
            i += 2
            continue
        elif c == '(':
            if depth == 0:
                start = i
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0 and start is not None:
                out.append((start, i + 1))
                start = None
        i += 1
    return out


def main():
    root = Path(sys.argv[1] if len(sys.argv) > 1 else 'src')
    results = []
    for path in sorted(root.glob('*.el')):
        text = path.read_text()
        for s, e in top_level_forms(text):
            form = text[s:e]
            m = DEFINERS.match(form)
            if not m:
                continue
            name = m.group(2)
            line_start = text.count('\n', 0, s) + 1
            line_end = text.count('\n', 0, e) + 1
            length = line_end - line_start + 1
            if length > 100:
                results.append((length, path.name, line_start, line_end, name))
    results.sort(reverse=True)
    print(f"{'lines':>6}  {'file':<28}{'start':>6}-{'end':<6}  name")
    for length, fname, ls, le, name in results:
        print(f"{length:>6}  {fname:<28}{ls:>6}-{le:<6}  {name}")
    print(f"\ntotal functions >100 lines: {len(results)}")


if __name__ == '__main__':
    main()
