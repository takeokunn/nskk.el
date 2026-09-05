# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- The Prolog engine's first-solution query API (`nskk-prolog-prove-one`,
  `nskk-prolog-query-one`, `nskk-prolog-query-value`, `nskk-prolog-query-values`)
  is now defined with the project's CPS macros, so a caller using the `/k`
  form can tell "no solution" apart from a solution that binds nothing.
  The synchronous wrappers keep their names, arguments, and return values.
- Removed the `presentation-action/2` Prolog fact table. Registrations were
  written to it and nothing ever queried it; presentation-action lookup now
  reads only the alist that already served every caller.
- Split `nskk-prolog-copy-term`, `nskk-prolog-set-index`, `nskk-prolog-assert`,
  `nskk-prolog-retract` and `nskk-prolog-retract-all` into named helpers, and
  dropped the `progn` wrappers the surrounding syntax did not require.

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
  clearable-input variables) plus a presentation-action registry, in place
  of hardcoded cross-module knowledge. That change renamed and removed
  nothing; for removals see the Removed section below.
- `nskk-state-create` is now a plain function. It previously used the CPS
  definer, but its failure continuation was unreachable, so the generated
  `nskk-state-create/k` has been removed along with it.
- `nskk-state-set` documents that an invalid value for the `mode` or
  `henkan-phase` key signals an error rather than invoking the not-found
  continuation. The behaviour is unchanged; the docstring was wrong.
- Decomposed `nskk--init-azik-rules` by extracting its flat AZIK rule-data
  tables into a dedicated function; other long functions were reviewed and
  left intact where splitting would relocate, not reduce, shared
  transactional or CPS-macro-sensitive state.
- Decomposed the two `nskk-show-mode` display functions by separating each
  one's install sequence from its fail-closed handler, and factored the
  repeated error-and-quit-swallowing cleanup into a single helper. The
  module keeps `defun/done` for its one public entry point and plain
  `defun` elsewhere: the CPS transformer does not rewrite `condition-case`
  bodies, so `succeed`/`fail` placed inside one is caught by that same
  handler and yields a wrong value instead of an error.
- Renamed the customization group `nskk-show-mode` to `nskk-mode-indicator`.
  A group whose name ends in `-mode` is reserved for the group named after
  it, which this one is not. No option was renamed, so saved customizations
  are unaffected; only the group's position in the customize tree changes.
- The tutorial's quit key moved from `q` to `C-c C-q`, and `g` and `r`
  now insert when point is in an exercise input area and navigate only
  from the lesson text. `q` and `Q` are left to NSKK, which needs them
  for the katakana toggle and the numeric conversion the lessons teach.
- Folded the tutorial's private deep-copy routine into
  `nskk-prolog-copy-term`, which gained an optional caller-supplied memo
  table, removing a second implementation of the same graph copy.
- Reshaped the mode-line module: cursor-color resolution now uses the
  project's CPS found/not-found pair, the mode-line indicator consumes that
  pair's continuations directly instead of a nil test, and the all-frames
  cursor restore delegates to a per-frame helper rather than recursing
  through its own public entry point. Behavior and public signatures are
  unchanged.
- Decomposed the henkan pipeline's large cleanup and commit functions into
  named helpers, and replaced the four hand-rolled `cl-labels` cleanup
  ladders with a shared `nskk--with-cleanup-runner` macro. Each call site
  keeps its own `inhibit-quit` behaviour: commit and reset stay
  interruptible, context-clear and registration stay uninterruptible.
- Changed `nskk-reset-henkan-state` and `nskk-set-active-candidates` from
  macros to functions. Both only ever received already-evaluated arguments,
  so no call site changed.
- Changed `nskk-convert-input-to-kana-final` from a CPS function to an
  ordinary function returning the converted kana. It never signalled
  absence, so its not-found continuation was unreachable; the generated
  `nskk-convert-input-to-kana-final/k` is gone.
- Reworked the skkserv client. Connection setup, teardown and rollback are
  split into named helpers; the duplicated poll-budget normalisation is now a
  single function; and Prolog rollback uses the engine's own
  `nskk-prolog-capture-key-state` / `nskk-prolog-restore-key-state` API rather
  than a hand-rolled six-table snapshot, which also restores the cons identity
  the hand-rolled version left untouched. `nskk-server-live-p` and the private
  annotation stripper are plain functions instead of CPS definitions, so the
  generated `nskk-server-live-p/k` wrapper no longer exists.
- Changed skkserv resource cleanup to one attempt per call. A process or
  buffer whose teardown faults stays registered and is reclaimed by the next
  close or open, rather than being retried immediately within the same call.
- Replaced the skkserv unit suite. Its six `nskk-deftest-table` blocks sat
  inside `nskk-it` bodies, so their rows registered as zero runnable tests;
  the rewritten suite registers 37 table rows and covers the poll budget,
  response byte cap, fail-closed coding preflight, candidate sanitisation,
  the exactly-one-continuation invariant, resource ownership, rollback and
  post-send teardown.

### Removed

- Removed the unused public surface of `nskk-state.el`, none of which had
  any caller in `src/`: `nskk-state-get`, `nskk-state-transition`,
  `nskk-state-reset`, `nskk-state-append-input`,
  `nskk-state-delete-last-char`, `nskk-state-clear-input`,
  `nskk-state-in-henkan-mode-p`, `nskk-state-henkan-on-p`,
  `nskk-state-henkan-active-p`, the candidate-navigation functions
  `nskk-state-next-candidate`, `nskk-state-previous-candidate` and
  `nskk-state-current-candidate`, the nine generated
  `nskk-state-set-SLOT` pairs, and the metadata setters
  `nskk-state-set-remaining-romaji`, `nskk-state-set-kana-type` and
  `nskk-state-set-width-type`. Candidate navigation is implemented
  directly in `nskk-henkan.el`; struct slots are set through
  `nskk-state-set` or `setf` on the accessor.
- Removed the macros `nskk-with-candidates`, `nskk-state-slot-dispatch`
  and `nskk-ensure-marker`, which had no caller outside their own tests.
  `nskk-define-buffer-local-accessor` is replaced by
  `nskk-define-buffer-local-getter` and
  `nskk-define-buffer-local-setter`; the accessor names it generates are
  unchanged.
- Removed the Prolog predicates `valid-mode/1`, `valid-henkan-phase/1`,
  `valid-henkan-transition/2`, `henkan-mode-phase/1` and
  `state-slot-default/2`, which nothing queried. Phase and mode
  validation are Elisp-side and unchanged in behaviour.
  `mode-properties/5`, `mode-category/2` and `japanese-mode/1` remain, as
  other modules query them.
- Made the debug module's hand-written CPS continuation-pattern declaration
  visible during byte compilation, so the CPS bind forms' guard against
  binding a `defun/done` function is no longer inert while the file compiles.

### Fixed

- Fixed a state-snapshot ordering bug in the tutorial dictionary-state
  guard where a Prolog fact query's side effect on the internal Prolog
  variable counter could be captured as part of the snapshot it was
  supposed to precede.
- Fixed `nskk-show-mode` recording a mode as displayed when nothing was
  drawn. With `nskk-show-mode-style` set to `tooltip` on a frame without
  tooltip support, the display attempt returned without drawing yet still
  updated the last-shown mode, so the deduplication guard suppressed every
  later attempt. The last-shown mode is now updated only after a style
  reports that it displayed something.
- Fixed the interactive tutorial, which could not be completed. Every
  exercise's input and result region shared one end marker position, so
  answers were never graded as correct and marking one exercise correct
  erased the rest of the lesson; the cursor was placed where typing
  raised `text-read-only`; the mode inherited `special-mode-map`, whose
  bindings for `h`, `?`, `<`, `>`, `-` and the digits shadowed keys the
  lessons require as input; `g` and `r` could not be typed at all; and
  `nskk-tutorial--reset-mode` called a function removed in an earlier
  release.
- Fixed `nskk-commit-current` calling neither of its continuations when
  invoked outside an active conversion, which broke the
  exactly-one-continuation contract every other CPS function in the module
  honours. It now signals absence. Callers using the synchronous wrapper
  are unaffected, since both the old and new paths yield nil.
- Fixed a henkan unit test that registered no assertions: a table-driven
  test was nested inside another test's body, so its five rows were
  registered as a side effect after ERT had already fixed its selection
  list and never ran. Also moved `provide` and four fault-injection test
  matrices out of a `progn` they had been nested inside.
- Fixed the program dictionary command timeout, which charged a full poll
  slice per `accept-process-output` call rather than measuring elapsed
  time. Because that call returns as soon as any output arrives, a command
  that emitted its output in several chunks exhausted the budget in far
  less than `nskk-program-dict-timeout` seconds and was reported as a
  miss. Both the command and calculation paths now share one wall-clock
  deadline.
- Fixed the program dictionary cache rollback so it runs with quit
  inhibited, matching the cache module's own rollback macro. A quit
  arriving between the rollback's field assignments could previously
  leave the cache in a partially restored state.

### Removed

- Removed a dead, zero-caller private helper
  (`nskk--converter-copy-prolog-state`) from the converter module.
- Removed the private per-style inline display builders
  `nskk--inline-build-horizontal` and `nskk--inline-build-vertical`,
  superseded by a single style-taking function.
- Removed the public macro `nskk-define-mode-entry`. It ignored two of its
  four arguments, and its documented branch for passing an existing face
  symbol had no call site. The four mode-line faces it generated are now
  plain `defface` forms under the same names
  (`nskk-modeline-hiragana-face` and siblings).
- Removed four zero-caller symbols from the henkan module: the macros
  `nskk-with-conversion-context`, `nskk-when-bound` and
  `nskk-when-bound-and`, and the function `nskk-set-last-kakutei-record`.
  These carry no `nskk--` prefix but had no caller in or outside the
  module; the getter `nskk-last-kakutei-record` is unaffected.
- Removed three Prolog fact tables from `nskk-henkan-initialize` that no
  production code queried: `search-backend`, `search-result-action` and
  `should-update-overlay`. The cross-module tables `converting-phase`,
  `preedit-phase` and `disable-cleanup` are unchanged.
- Removed the `nskk-cache-field` macro, its backing `cache-field-fn/3`
  Prolog fact table, and the CPS-style `nskk-cache-p/k` predicate from
  `nskk-cache.el`. None had callers outside `nskk-cache.el` and its test
  file.
- Removed `nskk--server-prolog-state-snapshot` and
  `nskk--server-restore-prolog-state` from the skkserv client in favour of the
  Prolog engine's own per-key snapshot API.
- Removed a dead, zero-caller private helper (`nskk--debug-format`) and the
  unused `debug-category` Prolog facts and their hash index from the debug
  module.

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
