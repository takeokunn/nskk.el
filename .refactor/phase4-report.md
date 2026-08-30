# Phase 4 report

## Status

Complete. FR-011 (residual cross-module private symbol elimination) is
implemented. Acceptance criterion — zero references to another file's
`nskk--*` symbol anywhere in `src/` — is met and verified by the project's
own inventory script: `python3 .refactor/bin/cross-module-symbols.py --check-zero`
reports `src rows: 0`.

## Scope

Starting inventory (regenerated at the top of this phase, after Phase 3
closed two more `-initialized`-flag instances): 48 unique cross-module
private symbols, in two groups.

### Small, independent clusters (17 symbols, done directly, not delegated)

`nskk-modeline.el` (2), `nskk-show-mode.el` (1 function + 2 new
buffer-local accessor pairs), `nskk-isearch.el` (1), `nskk-search.el` (2),
`nskk-annotation.el` (1), `nskk-converter.el` (1 function + `nskk-romaji-table`
getter/setter + two style-transaction-list register/getter/setter families),
`nskk-azik.el` (1 new getter), `nskk.el` (`nskk-activation-lock-owner` --
plain public rename, not an accessor, because `nskk-tutorial.el` `defvaralias`es
it and aliasing needs a raw variable; plus `nskk-active-buffers` and
`nskk-learning-loaded` getter/setter pairs), `nskk-dictionary.el`
(`nskk-dict-user-index` getter/setter, `nskk-dict-set-system-index` setter
added alongside the pre-existing getter, plus a new `nskk-dict-index-variables`
public constant for generic reflection machinery).

### The dense henkan/input/keymap/nskk.el web (30 symbols, one delegated agent)

`src/nskk-henkan.el`, `src/nskk-input.el`, `src/nskk-keymap.el` reference
each other's (and `nskk.el`'s) private symbols extensively -- 21 functions
promoted (plain rename) and 9 buffer-local variables given accessor pairs.
Treated as one atomic task rather than parallelized, since the three files
are too interdependent to safely split across concurrent agents.

## Design notes and traps found

**The symbol scanner cannot see through forward declarations.** `.refactor/bin/cross-module-symbols.py`'s definition-finder regex matches `(defvar SYM
...)` regardless of whether SYM has a value -- a bare `(defvar SYM)` forward
declaration (this codebase's standing idiom for satisfying the byte-compiler
across files without a `require`) looks identical to a real definition to
the scanner. This produced backwards ownership data for 5 of the 30
henkan/input/keymap symbols before it was caught and manually corrected
(verified via `grep -rn "(defvar\(-local\)\? SYM [^)]"`, which requires
content after the symbol on the same line). Anyone re-running this scanner
for a future phase should re-verify ownership the same way before trusting
its `definition_file` column.

**Disposition depends on how the symbol is consumed, not just what it is:**
- Plain read/write from elsewhere -> getter/setter accessor pair, matching
  `nskk-state.el`'s established convention.
- `defvaralias` target, or fed to `add-to-list`/`cl-progv` by literal symbol
  -> cannot go through a function-based accessor at all (both need the raw
  special-variable symbol) -- either leave as a plain public `defvar` (dash
  dropped, no wrapper) or, for a *list* of such symbols consumed by generic
  reflection code, expose a small public constant naming them (e.g.
  `nskk-dict-index-variables`), matching the `nskk-prolog-state-variables`
  precedent from Phase 1c/3.
- A `let`-binding used purely for test-isolation scoping converts cleanly to
  `cl-letf` + a file-local `(gv-define-simple-setter GETTER SETTER)` --
  exact precedent already existed in `test/unit/nskk-input-test.el` from
  Phase 2, reused here for `test/nskk-test-framework.el`'s `nskk-with-mock-dict`.
  Verified empirically that `cl-letf` restores exactly like `let` would.
- Bare-symbol-argument macros (`boundp`, `bound-and-true-p`, `symbol-value`,
  `set`, `makunbound`) are not simple text substitutions -- they need the
  *symbol*, not `(getter)`. A naive text substitution over these produces
  either a `wrong-type-argument symbolp` error (`bound-and-true-p`) or
  silently corrupts a quoted symbol-data list consumed reflectively
  elsewhere. Both were found and fixed during this phase (see below).

**A production correctness bug found while simplifying `nskk--learning-loaded`:**
collapsing a `boundp`/`makunbound`-capable reflection pattern to a plain
always-bound accessor removes the *capability* to restore to "genuinely
unbound" -- one test (`nskk-search-test.el`) was deliberately exercising
that exact capability. Since the variable is unconditionally `defvar`'d
with a default (always bound on any real call path), the capability was
already unreachable in production; fixed by changing the test to use two
distinct *bound* sentinel values per fault type instead of one bound / one
unbound, preserving the test's real intent (exact-value rollback across
both fault types) without needing unbind support.

**Test-file conversion hazards found and fixed** (`fr011-henkan-input-keymap`,
verified via a purpose-built structural scanner reading every top-level form
via `read` and flagging accessor-call shapes inside `quote`):
- Quoted symbol-data lists (`'(nskk--var1 nskk--var2 ...)` consumed via
  `dolist`/`symbol-value`/`set` for reflective test setup) -- naive
  substitution corrupted every element after the first. 2 instances.
- `(bound-and-true-p VAR)` -- 30 instances across two e2e files, fixed to
  `(and (fboundp 'nskk-foo) (nskk-foo))`, matching the codebase's existing
  idiom for the equivalent `(boundp 'X)`-guarded pattern.
- `setq-local` (distinct from plain `setq`) -- 1 instance.

**The "same-module test files are exempt" rule only applies to symbols that
stay private** -- it does not exempt a renamed *function*'s old name, since
that name stops existing everywhere, including in the owning module's own
test file. An earlier instruction from this session conflated the two;
corrected mid-phase after `fr011-henkan-input-keymap`'s own reconnaissance
found 71 failures inside `nskk-henkan-test.el` itself.

## Verification

- `python3 .refactor/bin/cross-module-symbols.py --check-zero`: `src rows: 0`, exit 0.
- `make compile`: clean.
- `make lint`: clean.
- `make package-lint`: clean.
- `make test`: **6090 selected, 6090 passed, 0 unexpected** -- fully clean.

## Remaining work

- **Not committed** (same hard-rule constraint as prior phases). Combined
  Phase 1c+2+3+4 diff: 69 files, +4776/-3187 (`git diff --stat`).
- Phases 5-6 (FR-005/007/008, docs) are not started.
