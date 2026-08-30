# Phase 1c report

## Status

Complete. FR-009 (Prolog DB snapshot/restore public API) is implemented, and its
acceptance criterion — zero `nskk--prolog-*` (private) references outside
`nskk-prolog.el`, tree-wide including `test/` — is met and grep-verified.

## Public API

`src/nskk-prolog.el` gained a state section:

- `nskk-prolog-state-snapshot` / `nskk-prolog-state-restore` — opaque 8-field
  snapshot/restore of all mutable Prolog state.
- `nskk-prolog-with-state` — dynamic full-state replacement macro.
- `nskk-prolog-with-database-fields` — dynamic replacement macro for a named
  subset of the 6 hash-table fields.
- `nskk-prolog-database` / `-database-tails` / `-index-config` /
  `-hash-indices` / `-trie-indices` / `-index-bucket-tail-cache` — zero-arg
  accessors returning the live mutable tables.
- `nskk-prolog-state-variables` — public constant listing the 8 state symbols
  in snapshot order, for callers needing symbol-level access (e.g.
  `add-variable-watcher`) rather than the opaque snapshot.
- 11 functions promoted from private to public: `nskk-prolog-clause-key`,
  `nskk-prolog-index-add`, `nskk-prolog-replace-clause-transaction`,
  `nskk-prolog-get-clauses`, `nskk-prolog-transaction-index`,
  `nskk-prolog-transaction-index-bucket`, `nskk-prolog-index-bucket-tail`,
  `nskk-prolog-transaction-set-index-bucket`,
  `nskk-prolog-index-cache-set-bucket`, `nskk-prolog-capture-key-state`,
  `nskk-prolog-prepare-key-state-index-tail`, `nskk-prolog-restore-key-state`.

`src/nskk-dict-transaction.el` gained two more public aliases discovered as a
missing gap from Phase 1b: `nskk-dict-transaction-pending-rollback`,
`nskk-dict-transaction-retry-pending-rollback` (added to the Makefile's
`package-lint--sane-prefixes` alongside the rest of that module's API).

## Migration

All external callers of the above (src/ and test/) moved off the private
names: `nskk-converter.el`, `nskk-dictionary.el`, `nskk-search.el`,
`nskk-server.el`, `nskk-study.el`, `nskk-tutorial.el`, `nskk-dict-transaction.el`,
and their corresponding test files, plus `test/nskk-test-framework.el` and
`test/nskk-e2e-helpers.el`. `test/unit/nskk-prolog-test.el` (self-module) kept
its private references except for the renamed functions, which it also
tracks. Two designs of note:

- `nskk-converter.el`'s `cl-progv`-based generic staging/publishing machinery
  now only covers non-Prolog symbols; the 6 Prolog fields are staged via
  `nskk-prolog-with-database-fields` and published/rolled back via
  `nskk-prolog-state-snapshot`/`-restore`, preserving the original
  atomic-publish-or-discard semantics.
- `nskk-tutorial.el`'s hand-rolled 10-slot save/restore vector collapsed to 3
  slots (`[prolog-snapshot user-dict-index system-dict-index]`), with
  `test/unit/nskk-tutorial-test.el`'s whitebox helpers updated to destructure
  the opaque snapshot positionally (documented as coupled to
  `nskk-prolog-state-snapshot`'s field order) rather than reading 8 named
  variables individually.
- `test/nskk-test-framework.el`'s `nskk-prolog-test-with-isolated-db`
  retained its per-symbol variable-watcher preserving save/restore (verified
  load-bearing by `test/unit/nskk-prolog-test.el`'s
  `nskk-prolog-isolation-watcher-cleanup-continues-and-resignals`), now
  sourcing its symbol list from the public `nskk-prolog-state-variables`
  constant instead of a hardcoded literal.

## Additional fixes (pre-existing gaps found during verification)

Three unrelated stale-symbol bugs left over from Phase 1b (FR-002/FR-006,
commit `09ca1a8`) were found and fixed because they blocked a clean full-suite
run, not because they were in FR-009's scope:

- `test/unit/nskk-search-test.el`, `nskk-dictionary-test.el`,
  `nskk-study-test.el`, `test/integration/nskk-dict-registration-integration-test.el`:
  stale calls to `nskk--dict-predicate-snapshot` (renamed to
  `nskk-dict-transaction-predicate-snapshot` in Phase 1b) -> fixed.
- `test/unit/nskk-dictionary-test.el`: stale calls to
  `nskk--dict-clear-pending-rollback`, `nskk--dict-rollback-and-resignal`,
  `nskk--dict-pending-rollback`, `nskk--dict-ensure-rollback-complete`,
  `nskk--dict-retry-pending-rollback` -> fixed, and the last two needed new
  public aliases in `nskk-dict-transaction.el` (see Public API above) since
  none existed yet.
- `test/unit/nskk-study-test.el`: two tests (`load-rolls-back-publication-error`,
  `load-rolls-back-before-resignaling-quit`) mocked the stale
  `nskk--dict-apply-predicate-snapshot` name, so their rollback-callback mock
  silently never fired (`rollback-called` stuck at 0) -> fixed to mock
  `nskk-dict-transaction-apply-predicate-snapshot`, the name
  `nskk-study.el` actually calls.

Also fixed two issues introduced within this phase's own work before they were
verified: a duplicate `nskk--prolog-var-counter` `defvar`, a dead duplicate
`nskk-prolog-clause-key` stub, a checkdoc-flagged lowercase error message in
`nskk-prolog-with-database-fields`, and a pre-existing (Phase 1a) missing
docstring on `nskk--prolog-ensure-presentation-actions`.

## Verification

- `make compile`: clean (byte-compile-error-on-warn, 29 files).
- `make lint`: clean (checkdoc).
- `make package-lint`: clean (only pre-existing external-package warnings, matching Phase 0 baseline).
- `make test`: 6090 selected, **6089 passed, 1 unexpected**
  (`nskk-unit-drains-exact-cap-stdout-from-short-lived-calculations`,
  `test/unit/nskk-program-dictionary-test.el`) — the same test already
  documented as a load-dependent flake in the Phase 1b report, unrelated to
  this phase's source changes (confirmed: `git diff HEAD` on that file and
  its test file is empty; failure set was non-deterministic across repeated
  runs under this session's sustained heavy system load, load average
  60-150+ from concurrent unrelated sessions on this shared machine).
- `grep -rln "nskk--prolog-" src/*.el | grep -v nskk-prolog.el` -> empty.
- `grep -rln "nskk--prolog-" test/**/*.el | grep -v nskk-prolog-test.el` -> empty.

## Remaining work

- **Not committed.** Per this session's hard rule (git write operations
  require explicit user instruction in the current message), all Phase 1c
  changes remain uncommitted in the worktree. 21 files changed, +836/-766
  (`git diff --stat`).
- Phases 2-6 (FR-004, FR-010, FR-011, FR-005/007/008, docs) are not started.
