# Contributing to NSKK

Thank you for contributing to NSKK.

## Development setup

NSKK requires Emacs 29.1 or later. The repository provides a Nix development
environment, or you can use a local Emacs installation:

```sh
nix develop
```

```sh
make compile
make test
```

## Pull requests

1. Create a topic branch from the current default branch.
2. Keep each change focused and add regression tests for behavioral changes.
3. Run the narrowest relevant test target, then run `make test` when practical.
4. Run `make compile`, `make lint`, and `make package-lint`. CI gates on all
   four targets. `make lint` runs checkdoc only, so a checkdoc pass says nothing
   about `package-lint`, which is a separate target with its own failures.
5. Describe the user-visible behavior, compatibility impact, and verification
   performed in the pull request.

Do not include generated `*.elc` files in commits.

CI runs the gate against Emacs 29.1, 29.4, 30.1, and snapshot. A local run
covers one version; the docstring-width rule below fails only on the 29.x legs.

## Code style

- Use the `nskk-` prefix for public symbols and `nskk--` for internal symbols.
- Enable `lexical-binding` in every Emacs Lisp source file.
- Prefer focused comments that explain non-obvious behavior rather than restating
  the code.
- Preserve DDSKK-compatible behavior unless the compatibility impact is
  explicitly documented.
- Adding a file under `src/` means adding it to `SRC` in the Makefile, in
  dependency order. `SRC` is a hand-maintained list and `make compile` sets
  `byte-compile-error-on-warn`, so a file listed before something it depends on
  fails the build. Test files need no such registration — `UNIT_SRC` is a
  wildcard.
- Renaming an internal (`nskk--`) symbol means updating `test/` in the same
  change. The suite reaches into private symbols throughout; find the sites for
  one symbol with `grep -rn '<symbol>' test/`.
- Keep docstring lines to 80 columns or fewer, continuation lines included.
  Emacs 29.1 and 29.4 reject a wider docstring under
  `byte-compile-error-on-warn`; Emacs 30 accepts it, so a local compile on
  Emacs 30 will not catch this. List every over-width line in a file with:

  ```sh
  perl -ne 'chomp; printf "%s:%d: %d cols\n", $ARGV, $., length($_) if length($_) > 80' src/FILE.el
  ```

  That reports code as well as docstrings, so check whether each hit sits
  inside a docstring. Byte-compiling under Emacs 29 is the authoritative check.

## Continuation-passing definers

`src/nskk-cps-macros.el` defines `defun/k`, `defun/done`, and `defun/3k`, which
are top-level definers alongside `defun` and account for most of the functions
in `src/`. Two consequences when working with them:

- Adding a *public* function with one of these definers requires adding its
  generated `/k` name to `package-lint--sane-prefixes` in the Makefile's
  `package-lint` target. That setting enumerates the names one by one, so
  skipping the update turns the gate red.
- Measuring function length by scanning for `(defun ` boundaries merges runs of
  adjacent CPS definitions into a single oversized span. Use a parenthesis-
  balanced parse instead.

## Reporting issues

Use the [GitHub issue tracker](https://github.com/takeokunn/nskk.el/issues) for
reproducible bugs and feature requests. Include the Emacs version, NSKK revision,
minimal configuration, reproduction steps, expected behavior, and actual
behavior.

For security-sensitive reports, avoid publishing exploit details in a public
issue. Contact the maintainer privately through the contact information in the
repository owner's GitHub profile.
