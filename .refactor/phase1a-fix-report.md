# FR-001 Phase 1a fix report

## Changes

- [verified] Removed all `inline` matches from `src/nskk-henkan.el`; `rg -ni inline src/nskk-henkan.el` returns zero matches.
- [verified] Added an ordered presentation-action callback cache in `src/nskk-prolog.el`, used by `nskk-prolog-presentation-actions`, avoiding a Prolog presentation-action query on each conversion.
- [verified] Moved the presentation-action APIs into their own section, added both APIs to the public API index, and documented the private cache helper.
- [verified] Moved `nskk--inline-finalize` before the public API section in `src/nskk-inline.el`.
- [verified] Strengthened the abnormal presentation-action test to assert the exact exception payload is preserved.

## Verification

- [verified] `make compile` — exit 0.
- [verified] `make lint` — exit 0.
- [verified] `make package-lint` — exit 0.
- [warning] `make test` selected 6087 tests and completed 6086 expected results with one unexpected result: `nskk-it/nskk-program-dict-builtin-lookup-no-learn-property/no-learn-property-is-set-to-t-for-calculator-candidates`.
- [verified] `make bench` with `NSKK_BENCH_SAMPLES=5` — exit 0; increased-sample benchmark completed and saved learning data.
- [warning] A separate `NSKK_BENCH_SAMPLES=10 make bench` run failed once in the benchmark's search-learn transaction with `wrong-type-argument (number-or-marker-p \\?_anon_4490974)`; the subsequent five-sample run completed. The failure was not reproduced as a test failure, so its root cause remains unconfirmed.
- [verified] `git diff --check` — exit 0.

## Gaps

The full test gate is not green because of the one calculator-candidate test failure above. No git add, commit, push, or other git write operation was performed.
