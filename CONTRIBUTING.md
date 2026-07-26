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
4. Run `make lint` and ensure changed Emacs Lisp files byte-compile cleanly.
5. Describe the user-visible behavior, compatibility impact, and verification
   performed in the pull request.

Do not include generated `*.elc` files in commits.

## Code style

- Use the `nskk-` prefix for public symbols and `nskk--` for internal symbols.
- Enable `lexical-binding` in every Emacs Lisp source file.
- Prefer focused comments that explain non-obvious behavior rather than restating
  the code.
- Preserve DDSKK-compatible behavior unless the compatibility impact is
  explicitly documented.

## Reporting issues

Use the [GitHub issue tracker](https://github.com/takeokunn/nskk.el/issues) for
reproducible bugs and feature requests. Include the Emacs version, NSKK revision,
minimal configuration, reproduction steps, expected behavior, and actual
behavior.

For security-sensitive reports, avoid publishing exploit details in a public
issue. Contact the maintainer privately through the contact information in the
repository owner's GitHub profile.
