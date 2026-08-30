#!/usr/bin/env python3
"""Inventory nskk-- private symbols crossing source-file boundaries."""
import argparse, re
from collections import defaultdict
from pathlib import Path

SYM = re.compile(r'\bnskk--[A-Za-z0-9!?_*+./:-]+\b')
DEF = re.compile(r'\(\s*(?:defun|defun/k|defun/done|defun/3k|defmacro|defsubst|defvar|defconst|defcustom|defvar-local|defvaralias|cl-defstruct)\s+([!?A-Za-z0-9_*+./:-]+)')

def mask(text):
    """Replace comments and strings with spaces while preserving newlines."""
    chars = list(text)
    string = escape = comment = False
    for i, c in enumerate(text):
        if comment:
            if c == '\n': comment = False
            else: chars[i] = ' '
        elif string:
            if escape: escape = False
            elif c == '\\': escape = True
            elif c == '"': string = False
            else: chars[i] = ' '
        elif c == ';': comment = True; chars[i] = ' '
        elif c == '"': string = True; chars[i] = ' '
    return ''.join(chars)

def forms(text):
    """Yield (start, end) of top-level balanced Lisp forms, ignoring strings/comments."""
    out, start, depth, string, escape, comment = [], None, 0, False, False, False
    for i, c in enumerate(text):
        if comment:
            if c == '\n': comment = False
            continue
        if string:
            if escape: escape = False
            elif c == '\\': escape = True
            elif c == '"': string = False
            continue
        if c == ';': comment = True; continue
        if c == '"': string = True; continue
        if c == '(':
            if depth == 0: start = i
            depth += 1
        elif c == ')' and depth:
            depth -= 1
            if depth == 0: out.append((start, i + 1))
    return out

def kind(line):
    if re.search(r'\b(?:setq|setf|cl-psetf|push|pop|incf|decf)\b', line): return 'mutation'
    if re.search(r'\b(?:let|let\*|cl-letf|dolist|dotimes)\b', line): return 'let-binding'
    if re.search(r'\b(?:fboundp|boundp|symbol-value|symbol-function|funcall|apply)\b', line): return 'reflection'
    return 'read'

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[2])
    ap.add_argument('--check-zero', action='store_true')
    ap.add_argument('--src-out', type=Path)
    ap.add_argument('--test-out', type=Path)
    args = ap.parse_args()
    root = args.root
    files = sorted((root / 'src').glob('*.el'))
    defs, occurrences = {}, defaultdict(list)
    for path in files:
        text = path.read_text()
        masked = mask(text)
        for a, b in forms(text):
            m = DEF.search(masked[a:b])
            if m and m.group(1).startswith('nskk--'):
                defs[m.group(1)] = path.name
        for n, line in [(m.group(), masked.count('\n', 0, m.start()) + 1) for m in SYM.finditer(masked)]:
            occurrences[n].append((path.name, line, text.splitlines()[line-1]))
    rows = []
    for sym, refs in sorted(occurrences.items()):
        if sym not in defs: continue
        for file, line, source in refs:
            if file != defs[sym]: rows.append((sym, defs[sym], file, line, kind(source)))
    test_rows = []
    for path in sorted((root / 'test').rglob('*.el')):
        text = path.read_text()
        masked = mask(text)
        for m in SYM.finditer(masked):
            sym, line = m.group(), text.count('\n', 0, m.start()) + 1
            if sym in defs: test_rows.append((sym, defs[sym], str(path.relative_to(root)), line, kind(text.splitlines()[line-1])))
    header = 'symbol\tdefinition_file\treference_file\treference_line\treference_kind\n'
    src_out = args.src_out or root / '.refactor/cross-module-symbols.tsv'
    test_out = args.test_out or root / '.refactor/cross-module-symbols-test.tsv'
    src_out.write_text(header + ''.join('\t'.join(map(str, r)) + '\n' for r in rows))
    test_out.write_text(header + ''.join('\t'.join(map(str, r)) + '\n' for r in test_rows))
    print(f'src rows: {len(rows)}; test rows: {len(test_rows)}; private definitions: {len(defs)}')
    if args.check_zero and rows: return 1
    return 0
if __name__ == '__main__': raise SystemExit(main())
