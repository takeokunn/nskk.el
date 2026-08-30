# Phase 1b report

## Status

Complete. Phase 1b requirements are implemented; the external test-harness
flakiness observed in the full suite is recorded below.

## Public API

The new `nskk-dict-transaction` module provides:

- `nskk-dict-transaction-predicate-snapshot`
- `nskk-dict-transaction-apply-predicate-snapshot`
- `nskk-dict-transaction-ensure-rollback-complete`
- `nskk-dict-transaction-clear-pending-rollback`
- `nskk-dict-transaction-rollback-and-resignal`
- `nskk-dict-transaction-insert-file-contents-pinned`
- `nskk-dict-transaction-read-entries`

The six transaction implementation bodies were moved out of `nskk-dictionary.el`.
`nskk-search.el` and `nskk-study.el` use only the public transaction API and the
shared transactional reader. The reader parameterizes only entry parsing, while
preserving the existing loading behavior. Dictionary-to-search cache reflection
was replaced by direct function references and declarations.

## Migration and reduction

FR-006 migration removed 42 lines from `nskk-search.el` and 45 lines from
`nskk-study.el`, for 87 lines total. The new module is included in the source
build order, and the sane-prefix list contains all new public symbols. Layer
headers were updated, including the dictionary dependency in `nskk-study.el`.

## Verification

- `pgrep -f 'emacs.*batch'`: no competing batch process before the final gate.
- `make compile`: passed.
- `make test`: 6,090 tests selected on both isolated runs; each produced 6,089
  expected results and 1 unexpected result. The repeated unexpected result was
  `nskk-unit-drains-exact-cap-stdout-from-short-lived-calculations`, a
  time-dependent external harness failure unrelated to the six changed files.
  Both runs therefore exited 1; no transaction, search, or study test failed.
- `make lint`: passed (exit 0).
- `make package-lint`: passed (exit 0).
- Baseline was 6,087 tests; the three new transaction tests bring the expected
  total to 6,090.

## Remaining work

No implementation gaps remain. The only gate exception is the repeated
external stdout-drain test failure documented above.

Commit hash: `09ca1a898817f7b71b4386eff5f2f0ad0a10847a`
