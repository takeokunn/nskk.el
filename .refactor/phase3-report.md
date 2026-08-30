# Phase 3 report

## Status

Complete. FR-010 (module initialization/reset registration protocol) is
implemented, following the FR-001 `clearable-input-var/1` precedent
generalized to module-initialized flags. `test/nskk-test-framework.el`'s
hardcoded 7-symbol enumeration is replaced by a fact query; acceptance
criterion (zero src-internal symbol enumeration in test-framework, full
suite green) is met.

Small phase, done directly rather than delegated (7 one-line registrations
plus one macro edit), but with two real correctness issues found and fixed
during verification.

## Public API / protocol

New Prolog predicate `module-initialized-flag/1`. Each of the 7 modules
owning an `nskk--*-initialized` flag asserts its own fact unconditionally at
file-load time (not inside the module's own lazy, guarded Prolog-init
function — see design note below):

- `nskk-input.el` -> `nskk--input-initialized`
- `nskk-state.el` -> `nskk--state-prolog-initialized`
- `nskk-henkan.el` -> `nskk--henkan-initialized`
- `nskk-kana.el` -> `nskk--kana-initialized`
- `nskk-converter.el` -> `nskk--converter-initialized`
- `nskk-candidate-window.el` -> `nskk--candidate-key-facts-initialized`
- `nskk-annotation.el` -> `nskk--annotation-initialized`

`test/nskk-test-framework.el`'s `nskk-prolog-test-with-isolated-db` macro
now sources its `saved-flags` list via
`(nskk-prolog-query-all-values '(module-initialized-flag ?f) '?f)` instead
of a hardcoded quoted list of the 7 symbols.

## Design decisions and bugs found

**Registration timing**: initially placed each `(nskk-prolog-<- (module-initialized-flag ...))`
call inside the module's own guarded lazy-init function (matching where
`clearable-input-var/1` facts are asserted in `nskk-input.el`). Moved to
unconditional top-level code instead, because a lazy registration creates a
race: if `nskk-prolog-test-with-isolated-db` captures `saved-flags` before a
given module's init function has ever fired, that module's flag is silently
omitted from the enumeration -- and if the module's init happens to fire
later, inside the isolated test body, its flag would never get restored
after the test (a state leak into subsequent tests). Top-level registration
makes every fact present as soon as the file loads, independent of when (or
whether) the module's own Prolog-fact-populating init function runs.

**`test/nskk-test-framework.el` was missing two `require`s**: `nskk-annotation`
and `nskk-candidate-window` were never required by the test harness itself,
even though its own comment states the intent ("Load all NSKK modules...for
the test session"). Under the old hardcoded-list design this didn't matter
(the 7 symbol names were literal, load-independent). Under the new
fact-query design it does: a test file that doesn't separately load one of
these two modules would never see its `module-initialized-flag` fact, so
that flag would be silently dropped from `saved-flags` -- for example,
running `test/unit/nskk-prolog-test.el` standalone (not via `make test`,
which happens to load `nskk-annotation-test.el` earlier in a fixed order)
reproduced this: `nskk-prolog-isolation-restores-on-all-exits` failed
because `nskk--annotation-initialized` was left bound after the isolated-db
block instead of restored to unbound. Fixed by adding both `require`s to
`test/nskk-test-framework.el` directly, so the fact-query design is robust
to load order rather than incidentally correct under the Makefile's fixed
file list.

**A second, independent bug**: `nskk-prolog-isolation-watcher-cleanup-continues-and-resignals`
(`test/unit/nskk-prolog-test.el`) deliberately corrupts
`nskk--prolog-var-counter` to a non-numeric sentinel (`(list 'counter-before)`)
to verify that save/restore preserves arbitrary values by identity,
regardless of type. This is incompatible with the new design: computing
`saved-flags` now runs a live Prolog query (`module-initialized-flag`),
and Prolog's proving/unification internally increments
`nskk--prolog-var-counter` for variable renaming -- so a *nested*
`nskk-prolog-test-with-isolated-db` call, entered while the outer test body
had `nskk--prolog-var-counter` corrupted to a list, threw
`(wrong-type-argument number-or-marker-p (counter-before))` from inside the
query, before the test's own deliberately-injected "blocked database
restore" error could occur. Fixed by changing the test's sentinel to a
distinctive real integer (`-972837465`) -- the test's assertions only need
`eq`-identity, not non-numeric-ness, so this preserves the test's intent
without conflicting with the isolation macro's new dependency on a
numerically-valid Prolog engine state.

## Second occurrence found: `nskk-tutorial.el`, plus a real production bug

The FR-011 symbol-inventory scan (`.refactor/bin/cross-module-symbols.py`,
run at the start of Phase 4) turned up a second, independent hardcoded
7-symbol enumeration of the same flags in `nskk-tutorial--save-dict-state`
(`src/nskk-tutorial.el`), used to save/restore all module-initialized flags
around a tutorial session -- structurally identical to the
`test-framework.el` case this phase already fixed, just a different
consumer. Replaced with the same `nskk-prolog-query-all-values` call.

This surfaced a real, pre-existing-shaped bug specific to combining a live
Prolog query with a "capture an exact, faithful snapshot" contract:
`nskk-tutorial--save-dict-state`'s `let*` computed `init-flags` (the query)
*before* `original-state` (`nskk-prolog-state-snapshot`). Since Prolog's
proving/unification touches `nskk--prolog-var-counter` as a side effect
even for a trivial fact lookup (see the Phase 3 notes above), the query's
own side effect was captured *inside* the "faithful" snapshot -- the
published working graph was not actually an exact copy of the state as it
existed when the caller invoked the function. Fixed by reordering the
`let*` so the snapshot is taken first, before anything (including the new
flag-enumeration query) can perturb the live engine state. General lesson:
any function whose contract is "capture an exact snapshot of current state"
must take that snapshot before running the new fact-query, not after,
whenever the query can affect the state being captured.

`test/unit/nskk-tutorial-test.el`'s fixture (`nskk-tutorial-test--make-dict-state`
/ `--call-with-dict-state`) builds a deliberately minimal, hand-crafted
Prolog database (a single `outside/2` fact) for many other tests' hash-table
identity/count checks; seeding it with the 7 `module-initialized-flag`
facts via `nskk-prolog-assert` was tried and reverted -- it broke `hash-table-count`
assertions in other tests sharing the same fixture, and `nskk-prolog-assert`
itself also bumps `var-counter`. The correct fix was to accept that this
isolated fixture legitimately has zero registered flags, and update the one
test asserting a specific 7-pair `nskk-tutorial--saved-init-flags` alist to
expect nil instead (with a comment explaining why) -- the flag *variables*
remain protected by the fixture's own `cl-progv` dynamic-scope restoration
regardless of what the internal snapshot records.

## Verification

- `make compile`: clean.
- `make lint`: clean.
- `make package-lint`: clean.
- `make test`: **6090 selected, 6090 passed, 0 unexpected** -- fully clean (re-confirmed after the `nskk-tutorial.el` fix above).
- `grep -n "nskk--\(input\|state-prolog\|henkan\|kana\|converter\|candidate-key-facts\|annotation\)-initialized" test/nskk-test-framework.el` -> only one hit, a docstring-comment example, not an enumeration.
- Query sanity check after loading all 29 src files:
  `(nskk-prolog-query-all-values '(module-initialized-flag ?f) '?f)` returns
  exactly the same 7 symbols the old hardcoded list had.

## Remaining work

- **Not committed** (same hard-rule constraint as prior phases). Combined
  Phase 1c + 2 + 3 diff is large; not yet measured standalone since nothing
  has landed as a commit boundary.
- Phases 4-6 (FR-011, FR-005/007/008, docs) are not started.
