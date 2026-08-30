# Phase 1a final report

## Result

Phase 1a FR-001 is committed on `refactor/layer-separation`.

## Fix and inspection

- `nskk-henkan.el` no longer calls the inline implementation directly. Presentation behavior is registered and dispatched through the Prolog-backed presentation-action protocol; inline finalization remains owned by `nskk-inline.el`. Verified by the source diff and the full test run.
- The prior failing no-learn-property test was caused by persisted learning data from the benchmark contaminating the test environment. The file was moved to `/tmp/nskk-phase1a-learning-backup/` before verification, and the subsequent full suite passed. Verified by the file cleanup command and test result.
- The anonymous-variable failure in the learning transaction was not reproduced with `NSKK_BENCH_SAMPLES=5 make bench`; the benchmark completed successfully. The cache and transaction paths were also exercised by the full suite. Verified by command exit status; the absence of reproduction is a verified observation, not proof that the historical defect is impossible.

## Verification

- `make test`: exit 0; 6087/6087 passed, 0 unexpected.
- `make compile && make lint && make package-lint`: exit 0.
- `git diff --check`: exit 0.
- `NSKK_BENCH_SAMPLES=5 make bench`: exit 0; output captured in `/tmp/nskk-phase1a-bench.txt`.

## Benchmark comparison

The requested E2E items did not improve against `.refactor/baseline/bench.txt`:

| Scenario | Baseline | Phase 1a | Observation |
|---|---:|---:|---|
| type `shi` | 0.0329 ms/op | 0.0465 ms/op | regression remains |
| type `nihongo` | 0.0885 ms/op | 0.1652 ms/op | regression remains |

This is a warning requiring follow-up; no benchmark gate was weakened.

## Commit

`ee9b930145cc5794f554ba1d51cc202c58798f33`

Commit subject: `refactor(henkan): invert henkan->inline dependency via presentation-action protocol (FR-001)`

Only the four requested Phase 1a paths were included in the commit. No push or PR was created. Phase 1b changes and unrelated untracked files remain untouched.
