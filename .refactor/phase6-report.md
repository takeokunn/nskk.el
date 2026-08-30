# Phase 6 report (documentation)

Complete. Final phase per `EXECUTION.md`'s Task Breakdown table.

## CHANGELOG.md

Added a `## [0.4.0] - Unreleased` section (Added/Changed/Fixed/Removed),
documenting the net effect of Phases 1c-5 at the level a downstream reader
needs: the new public accessor API surface, the internal cross-module
private-symbol elimination (no existing public API renamed or removed --
verified below), the AZIK function decomposition, one production bug fix
(tutorial snapshot-ordering), and one dead-code removal.

Verified no pre-existing public symbol definition was removed or renamed
anywhere in `src/` this session, so no "breaking change" framing was
needed per the requirements doc's own instruction ("既存公開シンボルを触
る場合のみ0.4.0破壊枠を使い、CHANGELOGに記録" -- only touching an
*existing public* symbol requires the breaking-change note):

```
git diff -- src/ | grep -E "^-\(defun nskk-[a-zA-Z]|^-\(defvar nskk-[a-zA-Z]|^-\(defconst nskk-[a-zA-Z]|^-\(defcustom nskk-[a-zA-Z]|^-\(defmacro nskk-[a-zA-Z]" | grep -v "nskk--"
```
-> zero matches. Every removed/renamed definition line was a private
(`nskk--*`) symbol; every public symbol this session touched was newly
*added* (promotion of a private symbol to a new public name, or a new
accessor), which is additive/non-breaking by the doc's own definition.

The package's `;; Version:` header in `src/nskk.el` (still `0.3.0`) was
deliberately left untouched -- Phase 6's target-file list in the
requirements doc is `README.org, CHANGELOG.md, src/*ヘッダ` (the latter
meaning the per-file Layer-position headers, checked below, not the
package Version header), and bumping the released package version is a
maintainer release action outside this phase's stated scope.

## README.org

- Line 33: "All 28 source modules" -> "All 29 source modules". The prior
  count predates Phase 1b's `nskk-dict-transaction.el` extraction (commit
  `09ca1a8`, already on this branch before this session started) and was
  never updated for it. Verified against the current tree:
  `ls src/*.el | wc -l` -> 29; `grep -L "lexical-binding: t" src/*.el` ->
  no output, confirming the "all N modules use lexical-binding: t" claim
  still holds for all 29.
- "Layer separation | 7 strict layers" (line 35): unchanged, still
  accurate -- see the layer-header consistency check below.
- Test count ("5,000+ unit/integration/E2E tests", line 36): left as-is.
  The requirements doc's own Phase 0 instruction was to confirm
  order-of-magnitude alignment ("桁整合"), not exact-count freshness; the
  current authoritative count (6090, confirmed in Phase 5's full-suite
  run) is still within the "5,000+" claim.
- The "Optional Features" table (lines 264-271) was checked and needs no
  change: `nskk-dict-transaction.el` is an internal dictionary-loading
  module, not a user-facing optional feature, so it doesn't belong in that
  table.

## Layer-header consistency

Checked every `src/*.el` file's `;; Layer position:` header line against
the actual file count and the FR-011 completion gate:

```
grep -c "^;; Layer position:" src/*.el | grep -v ":1$"
```
-> only `src/nskk.el:0`. All other 28 files have exactly one header line.
`nskk.el` intentionally has none: it's the Main/entry file that requires
and orchestrates every layer, sitting above the L0-L5 numbering rather
than being one of the seven layers itself (matching its own file
docstring and the "Standalone (above Main)" layer label used by the one
file, `nskk-debug.el`, that depends on `nskk.el`). This is the pre-existing
intentional design -- no inconsistency found, nothing to fix.

Cross-checked against the authoritative machine-verifiable FR-011 gate
(zero cross-module private-symbol references, which would be impossible
to hold if the layer headers' declared dependencies didn't match the
real `require` graph):

```
python3 .refactor/bin/cross-module-symbols.py --check-zero
```
-> `src rows: 0` (unchanged from Phase 4's verification; re-run here to
confirm Phase 5's edits didn't regress it).

## Verification

- `git diff -- src/ | grep ...` (breaking-change check above) -> 0 matches.
- `ls src/*.el | wc -l` -> 29.
- `grep -L "lexical-binding: t" src/*.el` -> no output (all 29 compliant).
- `grep -c "^;; Layer position:" src/*.el | grep -v ":1$"` -> only
  `nskk.el:0`, expected and pre-existing.
- `python3 .refactor/bin/cross-module-symbols.py --check-zero` -> `src
  rows: 0`.
- No source files changed this phase, only `CHANGELOG.md`, `README.org`,
  and this report -- `make compile`/`make lint`/`make package-lint`/`make
  test` results are unchanged from Phase 5's clean run (6090/6090, 0
  unexpected) since no `.el` file was touched.

## Remaining work

- **Not committed** (same hard-rule constraint as every prior phase this
  session: git write operations require explicit user instruction in the
  current message, which has not been given).
- All six phases of `EXECUTION.md`'s Task Breakdown table are now
  complete: 1c/FR-009, 2/FR-004, 3/FR-010, 4/FR-011, 5/FR-005+FR-008,
  6/documentation. FR-007 (optional) remains explicitly deferred.
- The combined, still-uncommitted diff spans Phases 1c through 6.
