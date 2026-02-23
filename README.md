# NSKK - Next-generation SKK for Emacs

<!-- MELPA badge (to be added after acceptance) -->
<!-- [![MELPA](https://melpa.org/packages/nskk-badge.svg)](https://melpa.org/#/nskk) -->
<!-- [![MELPA Stable](https://stable.melpa.org/packages/nskk-badge.svg)](https://stable.melpa.org/#/nskk) -->

NSKK is a modern, zero-dependency Japanese input method for Emacs based on
[SKK](https://skk-dev.github.io/ddskk/) (Simple Kana to Kanji) principles.
It provides a clean reimplementation with a modular architecture, full
lexical-binding, and aggressive performance targets.

## Features

- **Zero external dependencies** -- relies only on Emacs built-in features
- **Full lexical-binding** throughout every source file
- **Modern 7-layer architecture** with clean separation of concerns
- **High-performance design** -- targets < 1ms key response, < 10ms dictionary search
- **Native compilation support** for additional speed
- **ddskk compatibility layer** for smooth migration (`nskk-ddskk-compat.el`, `nskk-migrate.el`)
- **Extensible hook/event system** for plugins and customization

## Requirements

- Emacs 29.1 or later

## Installation

### MELPA (pending)

MELPA submission is planned. Once accepted:

```elisp
(use-package nskk
  :ensure t
  :config
  (nskk-global-mode 1))
```

### Manual

```bash
git clone https://github.com/takeokunn/nskk.el.git ~/path/to/nskk.el
```

```elisp
(add-to-list 'load-path "~/path/to/nskk.el")
(require 'nskk)
(nskk-global-mode 1)
```

## Quick Start

```elisp
(require 'nskk)
(nskk-global-mode 1)
;; Toggle with C-x C-j
```

## Key Bindings

| Key       | Command             | Description              |
|-----------|---------------------|--------------------------|
| `C-x C-j` | `nskk-toggle-mode`  | Toggle NSKK mode on/off |
| `C-j`     | `nskk-kakutei`      | Commit conversion       |

## Configuration

All options live in the `nskk` customization group. Run
`M-x customize-group RET nskk RET` to browse them interactively.

A few commonly used variables:

```elisp
;; Default input mode when NSKK is activated (ascii, hiragana, katakana)
(setq nskk-state-default-mode 'hiragana)

;; Path to user dictionary
(setq nskk-dict-user-dictionary-file (expand-file-name "~/.skk/jisyo"))

;; System dictionary files
(setq nskk-dict-system-dictionary-files
      '("/usr/share/skk/SKK-JISYO.L"))

;; How to handle 'n' -> 'ん' conversion (smart, strict, loose)
(setq nskk-converter-n-processing-mode 'smart)

;; Candidate window page size
(setq nskk-candidate-window-page-size 7)

;; Modeline format (%m = mode name, %s = state indicator)
(setq nskk-modeline-format "[%m%s]")
```

## Differences from ddskk

| Aspect | NSKK | ddskk |
|--------|------|-------|
| Dependencies | Zero (Emacs built-ins only) | ccc, cdb, etc. |
| Minimum Emacs | 29.1 | Supports older versions and XEmacs |
| Binding model | Full lexical-binding | Dynamic binding in many files |
| Architecture | Clean 7-layer modular design | Monolithic codebase |

NSKK provides a migration path for ddskk users:

- `nskk-ddskk-compat.el` -- compatibility shims for ddskk variables and functions
- `nskk-migrate.el` -- utilities to import ddskk dictionaries and settings

See [docs/how-to/migrate-from-ddskk.md](docs/how-to/migrate-from-ddskk.md)
for a step-by-step migration guide.

## Documentation

Full documentation is available under the [docs/](docs/) directory, organized
using the [Diataxis](https://diataxis.fr/) framework:

- **Tutorials** -- [docs/tutorial/](docs/tutorial/)
- **How-to guides** -- [docs/how-to/](docs/how-to/)
- **Reference** -- [docs/reference/](docs/reference/)
- **Explanations** -- [docs/explanation/](docs/explanation/)

## Status

**v0.1 -- Early development.**
Core conversion engine and infrastructure are implemented. Full end-to-end
input pipeline is under active development.

## Contributing

Contributions are welcome. Please read
[docs/how-to/contributing.md](docs/how-to/contributing.md) for guidelines on
the development process, coding conventions, and testing strategy.

## License

GPL-3.0-or-later. See [COPYING](COPYING) or
<https://www.gnu.org/licenses/gpl-3.0.html>.
