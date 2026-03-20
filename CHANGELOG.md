# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- **ja-dic candidate order**: Removed erroneous `(reverse cands)` in
  `nskk--dict-ja-dic-flatten-node`.  Candidates stored in the compiled
  `skkdic-okuri-nasi`/`skkdic-okuri-ari` trees are already in DDSKK-compatible
  order (as produced by `skkdic-extract-conversion-data`'s cons-reversal of the
  source text).  Re-reversing them caused nskk to present candidates in the
  opposite order from DDSKK.  Unit tests updated to document the correct
  pass-through semantics.

### Added

- **AZIK y-prefix youon rows**: AZIK mode now supports standard romaji y-prefix
  youon sequences (ky, ry, ny, hy, my, gy, jy, by, py) in addition to the
  existing AZIK-specific g-substitution rows (kg, rg, etc.). AZIK extension
  keys apply to y-prefix sequences: e.g. `ryp` -> りょう, `ryh` -> りゅう,
  `ryz` -> りゃん (DDSKK-compatible behavior).

### Breaking Changes

- **Cursor keys in conversion mode**: C-n/C-p and up/down arrows now commit the
  current candidate and move the cursor, instead of navigating candidates. Use
  SPC/x for candidate navigation (ddskk-compatible behavior).

  **Migration**: If you previously used C-n/C-p to navigate candidates, use
  SPC (next candidate) and x (previous candidate) instead.

### Changed

- C-n/C-p/up/down in converting mode (candidate selection): Now commits
  candidate then moves cursor (ddskk-compatible behavior)

### Code Quality (MELPA preparation)

- **`nskk--compute-phase/text-presence/mode-category` unit tests**: Added 16 unit tests
  for the three orthogonal feature-dimension helpers in `nskk-keymap.el` covering all
  return values (converting/henkan-on/idle, has-text/no-text, japanese/marker-mode/other),
  nil-state guards, priority ordering, and exhaustive-mode sweeps.
- **`nskk-prolog-trie-has-prefix-p` unit tests**: Added 6 unit tests covering prefix
  match, exact match (exact key is valid prefix), no-match, no-trie-configured guard,
  Japanese kana multi-key prefix, and empty-string root invariant.

- **Positive-first `if (not ...)` refactoring**: Converted four CPS guard sites from
  `(if (not guard) (fail) body)` to `(if guard body (fail))` across `nskk-state.el`
  (`nskk-state-set/k`), `nskk-henkan.el` (`nskk-core-search/k`), and
  `nskk-program-dictionary.el` (`nskk-program-dict-lookup/k`,
  `nskk-program-dict-builtin-lookup/k`).
- **`(require 'subr-x)` and `(require 'cl-lib)` in `nskk-henkan.el`**: Added missing
  explicit requires; `string-empty-p` used in 8 sites requires `subr-x`.
- **`(when (not ...))` → `(unless ...)` in `nskk-henkan.el`**: 1 remaining site converted.
- **`nskk-converter.el:333` string-empty-p**: Replaced `(zerop (length remaining))` with
  `(string-empty-p remaining)` — source file consistency.



- **`string-match-p` for pure boolean matches**: Replaced `(should
  (string-match ...))` with `(should (string-match-p ...))` in 7 test
  locations across `nskk-program-dictionary-test.el`, `nskk-henkan-test.el`,
  and `nskk-okurigana-e2e-test.el` -- avoids clobbering global match data.
- **`string-empty-p` consistency**: Replaced `(zerop (length ...))` with
  `string-empty-p` in `nskk-trie.el` (3 sites) and `nskk-converter.el` (2
  sites); replaced `(> (length str) 0)` with `(not (string-empty-p str))`
  across 25+ test locations in unit, integration, and E2E test files; replaced
  list `(> (length list) 0)` guards with bare truthiness checks where
  appropriate; added `(require 'subr-x)` to both source files.
- **Hankaku region test strengthened**: Replaced weak `(> (length
  (buffer-string)) 0)` assertion in `nskk-hankaku-katakana-region` test with
  exact half-width katakana string check; added 2 additional coverage cases
  (full aiueo row, ASCII passthrough).
- **Numeric conversion unit tests**: Added 53 unit tests covering all 8
  `nskk--numeric-*` functions in `nskk-henkan.el` including type dispatch
  (#0-#4, unknown), leading-ichi asymmetry in place-value kanji, and
  multi-pattern template replacement; unit suite 3834 -> 3887.
- **`nskk-show-mode` unit tests**: Added 15 unit tests for `nskk-show-mode.el`
  covering `nskk--show-mode-display-inline` overlay/timer lifecycle, exact
  indicator string content for all 5 modes (hiragana/katakana/ascii/
  jisx0208-latin/abbrev), `nskk-show-mode-display` deduplication and
  re-display logic; total suite 3887 -> 5276.
- **`(should (not (null x)))` simplification**: Simplified double-negation
  assertions `(should (not (null ...)))` to `(should ...)` in 4 locations
  across integration test files.
- **`pcase` refactoring**: Replaced `cond`/`if`-chain dispatches on string/symbol
  with `pcase` across `nskk-converter.el`, `nskk-prolog.el`, `nskk-context.el`,
  `nskk-state.el`, `nskk-henkan.el`, `nskk-input.el`, and `nskk-keymap.el`.
- **Positive-first condition style**: Refactored `(if (not cond) (fail) body)`
  to `(if cond body (fail))` in `nskk-kana.el` (2 sites), `nskk-server.el`
  (2 sites), `nskk-keymap.el` (1 site), and `nskk-input.el` (1 site).
- **`string-empty-p` over `(> (length str) 0)`**: Replaced all length-zero
  checks with `string-empty-p` in source files; added `(require 'subr-x)` to
  `nskk-search.el` and `nskk-state.el`.
- **Explicit `(require 'cl-lib)`**: Added explicit cl-lib requires to
  `nskk-context.el`, `nskk-henkan.el`, `nskk-isearch.el`, `nskk-kana.el`;
  removed spurious cl-lib from `nskk-annotation.el`.
- **`defsubst` for hot path**: Promoted `nskk--conversion-start-active-p` to
  `defsubst` for inlining in the input dispatch hot path.
- **`let*`/`let` cleanup**: Collapsed unnecessary `let*` to `let` and removed
  unused bindings across source and test files.
- **Zero byte-compile warnings**: All source and test files compile clean.
- **`:package-version` completeness**: All `defcustom`/`defface` entries carry
  `:package-version '(nskk . "0.1.0")`.
