# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0] - Unreleased

### Added

- Added a public accessor API (getter/setter function pairs) for
  previously private cross-module state in the Prolog engine, buffer-local
  editing state, converter/AZIK rule state, dictionary indices, search
  caches, show-mode overlays, and the henkan/input/keymap subsystem,
  replacing direct cross-module access to private `nskk--*` symbols
  throughout the source tree.
- Added `nskk-prolog-state-variables`, `nskk-dict-index-variables`, and
  matching reflection constants so generic snapshot/restore and test
  machinery can enumerate tracked state without naming private symbols.
- Added `nskk-dict-transaction.el`, extracting the dictionary's
  transactional load/save/rollback machinery into its own module.

- Added `nskk-display-sanitize`, which strips every text property from
  untrusted dictionary text before applying a single display face, and
  `nskk-overlay-priority-inline`, `nskk-overlay-priority-dcomp-multiple`
  and `nskk-overlay-priority-mode-indicator`, which name the overlay
  priority values the display sites previously carried as bare integers.
  The numeric values are unchanged.

### Changed

- `nskk-show-inline` now offers `horizontal` in its customize `:type`. The
  symbol was already documented in the option's docstring and accepted by
  its `:safe` predicate, but could not be chosen through customize.
- Merged the inline module's two per-style display builders into one
  style-taking function and routed it, the candidate list and the
  annotation display through `nskk-display-sanitize`. Output is identical
  including text properties.
- Eliminated cross-module references to private (`nskk--*`) symbols across
  the source tree, unifying state ownership behind the new accessor API
  and a Prolog-fact registration protocol (module-initialized flags,
  clearable-input variables, presentation actions) in place of hardcoded
  cross-module knowledge. Internal architecture only; no existing public
  API was renamed or removed.
- Decomposed `nskk--init-azik-rules` by extracting its flat AZIK rule-data
  tables into a dedicated function; other long functions were reviewed and
  left intact where splitting would relocate, not reduce, shared
  transactional or CPS-macro-sensitive state.

### Fixed

- Fixed a state-snapshot ordering bug in the tutorial dictionary-state
  guard where a Prolog fact query's side effect on the internal Prolog
  variable counter could be captured as part of the snapshot it was
  supposed to precede.

### Removed

- Removed a dead, zero-caller private helper
  (`nskk--converter-copy-prolog-state`) from the converter module.
- Removed the private per-style inline display builders
  `nskk--inline-build-horizontal` and `nskk--inline-build-vertical`,
  superseded by a single style-taking function.
- Removed the `nskk-cache-field` macro, its backing `cache-field-fn/3`
  Prolog fact table, and the CPS-style `nskk-cache-p/k` predicate from
  `nskk-cache.el`. None had callers outside `nskk-cache.el` and its test
  file.

## [0.3.0] - 2026-07-26

### Added

- Added shared E2E helpers and comprehensive regression coverage for dictionary
  parsing and registration, conversion, search and caching, skkserv, the
  embedded Prolog engine, input modes, state transitions, and tutorial flows.
- Added benchmark coverage for representative input-dispatch and end-to-end
  conversion paths.

### Changed

- Hardened dictionary, conversion, search, skkserv, program-dictionary, and
  Prolog behavior, including cache ownership and rollback semantics, malformed
  input handling, timeout accounting, and resource cleanup.
- Changed external program dictionaries to treat a non-zero process exit as a
  lookup miss instead of accepting any stdout produced before the failure.
- Strengthened release gates so byte compilation, Checkdoc, and package-lint
  failures stop the build, and package-lint checks every source module.

### Fixed

- Fixed program-dictionary subprocess output loss when a process exits before
  its final stdout chunk is delivered to the process filter.
- Fixed stale romaji classification results after Prolog classification rules
  are reinitialized.
- Fixed AZIK E2E coverage so the standalone suite is run directly rather than
  relying on incidental loading from another test file.

### Security

- Bounded stdout and stderr from program-dictionary subprocesses and skkserv
  responses, with deterministic timeout and process cleanup behavior.
- Kept program-dictionary arguments out of shell interpolation, terminated
  verified subprocess groups on failure, and restricted isolated calculation
  results to one safe, size-limited value while rejecting circular reader
  syntax.

### Performance

- Memoized the finite Prolog-backed romaji doubled-context and classification
  dispatch. Source benchmarks measured 60.0-92.6% lower classification time
  and 33.0-52.5% lower end-to-end conversion time in the tested scenarios.

## [0.2.2] - 2026-07-04

### Fixed

- **辞書登録 C-g**: ミニバッファで `C-g` が登録を抜けない問題を修正 ([#38](https://github.com/takeokunn/nskk.el/issues/38))。`reg-map` に `C-g` → `abort-recursive-edit` の束縛を追加し、`nskk-mode-map` の `nskk-handle-cancel` 経由で preedit-clear に転送される問題を回避。
- **ja-dic candidate order**: Removed erroneous `(reverse cands)` in
  `nskk--dict-ja-dic-flatten-node`.  Candidates stored in the compiled
  `skkdic-okuri-nasi`/`skkdic-okuri-ari` trees are already in DDSKK-compatible
  order (as produced by `skkdic-extract-conversion-data`'s cons-reversal of the
  source text).  Re-reversing them caused nskk to present candidates in the
  opposite order from DDSKK.  Unit tests updated to document the correct
  pass-through semantics.

### Security

- **safe-local-variable remediation**: Removed `:safe` predicates and added
  `:risky t` markers to variables that control external process execution and
  network connections.  These variables can no longer be set silently via
  `.dir-locals.el`; Emacs will now warn or block when a project-local file
  attempts to configure them.

  **Variables marked `:risky t`** (Emacs blocks even if added to
  `safe-local-variable-values`):
  - `nskk-program-dict-enable`, `nskk-program-dicts` — control arbitrary
    Emacs Lisp callbacks and external shell process execution
  - `nskk-server-enable`, `nskk-server-host`, `nskk-server-portnum` — control
    plaintext TCP connections to skkserv instances

  **Variables with `:safe` removed** (Emacs prompts before applying):
  - `nskk-server-coding-system`, `nskk-server-timeout`, `nskk-server-report-response`
  - `nskk-dict-user-dictionary-file`, `nskk-dict-system-dictionary-files`,
    `nskk-large-dictionary`, `nskk-search-learning-file`, `nskk-kakutei-jisyo`,
    `nskk-study-file`

  **Migration**: Move any of these variables from `.dir-locals.el` to your
  `init.el` using `setq`, or accept the Emacs safety prompt on each directory
  visit.

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

[Unreleased]: https://github.com/takeokunn/nskk.el/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/takeokunn/nskk.el/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/takeokunn/nskk.el/releases/tag/v0.2.2
Earlier published artifacts and release notes are available from
[GitHub Releases](https://github.com/takeokunn/nskk.el/releases).
