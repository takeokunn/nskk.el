# NSKK — Next-generation SKK for Emacs

[![CI](https://github.com/takeokunn/nskk.el/actions/workflows/ci.yml/badge.svg)](https://github.com/takeokunn/nskk.el/actions/workflows/ci.yml)

NSKK is a modern, zero-dependency Japanese input method for Emacs based on
[SKK](https://skk-dev.github.io/ddskk/) (Simple Kana to Kanji) principles.
It provides a clean reimplementation with a 7-layer modular architecture,
full lexical-binding, and aggressive performance targets.

## Features

- **Zero external dependencies** — relies only on Emacs built-in features
- **Full lexical-binding** throughout every source file
- **Embedded Prolog engine** — conversion and dispatch logic expressed as facts/rules
- **High-performance design** — targets < 1ms key response, < 10ms dictionary search
- **ddskk-compatible** key bindings and input behavior
- **AZIK extended romaji** support (optional, via `nskk-azik.el`)
- **Tested on Emacs 29.1, 29.4, 30.1, and snapshot** via CI

## Requirements

- Emacs 29.1 or later

## Installation

### use-package (built-in, Emacs 29+)

Clone the repository first, then load via `:load-path`:

```bash
git clone https://github.com/takeokunn/nskk.el.git ~/path/to/nskk.el
```

```elisp
(use-package nskk
  :load-path "~/path/to/nskk.el"
  :config
  (nskk-global-mode 1))
```

With optional AZIK and custom dictionary:

```elisp
(use-package nskk
  :load-path "~/path/to/nskk.el"
  :custom
  (nskk-state-default-mode 'hiragana)
  (nskk-dict-system-dictionary-files '("/usr/share/skk/SKK-JISYO.L"))
  (nskk-converter-romaji-style 'azik)
  :config
  (nskk-global-mode 1))
```

### leaf

```bash
git clone https://github.com/takeokunn/nskk.el.git ~/path/to/nskk.el
```

```elisp
(leaf nskk
  :load-path "~/path/to/nskk.el"
  :require t
  :config
  (nskk-global-mode 1))
```

With optional AZIK and custom dictionary:

```elisp
(leaf nskk
  :load-path "~/path/to/nskk.el"
  :require t
  :custom
  (nskk-state-default-mode . 'hiragana)
  (nskk-dict-system-dictionary-files . '("/usr/share/skk/SKK-JISYO.L"))
  (nskk-converter-romaji-style . 'azik)
  :config
  (nskk-global-mode 1))
```

### straight.el

```elisp
(straight-use-package
 '(nskk :type git :host github :repo "takeokunn/nskk.el"))
```

With `use-package` integration:

```elisp
(use-package nskk
  :straight (nskk :type git :host github :repo "takeokunn/nskk.el")
  :config
  (nskk-global-mode 1))
```

With `leaf` + `straight-leaf` integration:

```elisp
(leaf nskk
  :straight (nskk :type git :host github :repo "takeokunn/nskk.el")
  :require t
  :config
  (nskk-global-mode 1))
```

### elpaca

```elisp
(elpaca (nskk :host github :repo "takeokunn/nskk.el")
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
;; Toggle in current buffer with C-x C-j
```

To use a system SKK dictionary (e.g. `skktools` on Debian/Ubuntu or `skk-jisyo` on NixOS):

```elisp
(setq nskk-dict-system-dictionary-files '("/usr/share/skk/SKK-JISYO.L"))
(nskk-global-mode 1)
```

NSKK also auto-detects dictionaries from Nix profiles and common system locations
when `nskk-dict-system-dictionary-files` is `nil`.

## Key Bindings

All bindings are active inside `nskk-mode`. They follow ddskk conventions.

### Mode Control

| Key         | Function              | Description                                                  |
|-------------|-----------------------|--------------------------------------------------------------|
| `C-x C-j`   | `nskk-toggle-mode`    | Toggle NSKK on/off in current buffer                         |
| `C-j`       | `nskk-kakutei`        | Commit / enter hiragana from ASCII / newline in Japanese mode |
| `q`         | `nskk-handle-q`       | Toggle hiragana/katakana                                     |
| `l`         | `nskk-handle-l`       | Switch to ASCII (latin) mode                                 |
| `L`         | `nskk-handle-upper-l` | Switch to full-width latin (JIS X 0208) mode                 |
| `/`         | `nskk-handle-slash`   | Enter abbrev mode                                            |

### Conversion (henkan)

| Key         | Function                     | Description                              |
|-------------|------------------------------|------------------------------------------|
| `SPC`       | `nskk-handle-space`          | Start conversion (▽→▼) / next candidate |
| `x`         | `nskk-handle-x`              | Previous candidate                       |
| `RET`       | `nskk-handle-return`         | Commit candidate and insert newline      |
| `C-g`       | `nskk-handle-cancel`         | Cancel conversion or preedit             |
| `a`–`l`     | (candidate list keys)        | Select candidate directly in list mode   |

### Input Modes

| Mode             | Display | Description                       |
|------------------|---------|-----------------------------------|
| `hiragana`       | `かな`  | Default Japanese input mode       |
| `katakana`       | `カナ`  | Katakana input mode               |
| `ascii`          | `SKK`   | Direct ASCII (pass-through) mode  |
| `latin`          | `SKK`   | Direct ASCII mode (same as ascii) |
| `jisx0208-latin` | `全英`  | Full-width latin (JIS X 0208)     |
| `abbrev`         | `aA`    | Abbreviation mode                 |

## Architecture

NSKK is organized into 7 layers with 16 modules. Higher layers depend only
on lower layers; there are no circular dependencies.

```
L6 Entry
  nskk.el                    -- Minor mode, global mode, bootstrap

L5 Presentation
  nskk-keymap.el             -- State-aware key dispatch
  nskk-modeline.el           -- Mode-line indicator and cursor color
  nskk-candidate-window.el   -- Echo area candidate display

L4 Input
  nskk-input.el              -- Character routing, mode switching, romaji accumulation

L3 Application
  nskk-henkan.el             -- Conversion pipeline, candidate nav, dict registration
  nskk-azik.el               -- AZIK extended romaji (optional)

L2 Domain
  nskk-state.el              -- Per-buffer state struct and mode transition graph
  nskk-converter.el          -- Romaji-to-kana table-driven conversion engine
  nskk-search.el             -- Dictionary search algorithms (exact/prefix/regex)

L1 Core Engine
  nskk-kana.el               -- Kana character classification and conversion
  nskk-dictionary.el         -- Dictionary loading, lookup, registration, persistence
  nskk-cache.el              -- LRU/TTL cache for search results

L0 Foundation
  nskk-prolog.el             -- Embedded Prolog engine (unification, backtracking)
  nskk-custom.el             -- Centralized defcustom/defgroup definitions
  nskk-debug.el              -- Debug logging to *NSKK Debug* buffer
```

**Embedded Prolog engine.** Decision logic across all layers is expressed as
Prolog facts and rules queried at runtime. This eliminates nested `cond`
chains and allows hot-swappable dispatch tables. Hash and trie indexes
provide O(1) and O(k) lookup for the most common patterns.

## Configuration Reference

Run `M-x customize-group RET nskk RET` to browse all options interactively.
The table below covers the most commonly adjusted variables.

### Input behavior

| Variable                            | Default    | Description                                     |
|-------------------------------------|------------|-------------------------------------------------|
| `nskk-state-default-mode`           | `ascii`    | Initial mode when NSKK is activated             |
| `nskk-converter-romaji-style`       | `standard` | Romaji style: `standard` or `azik`             |
| `nskk-converter-auto-start-henkan`  | `t`        | Uppercase letter auto-starts conversion         |
| `nskk-converter-n-processing-mode`  | `smart`    | ん detection: `smart`, `strict`, or `loose`    |
| `nskk-converter-use-sokuon`         | `t`        | Enable っ doubling from repeated consonants     |

### Dictionary

| Variable                             | Default          | Description                          |
|--------------------------------------|------------------|--------------------------------------|
| `nskk-dict-user-dictionary-file`     | `~/.skk/jisyo`   | Path to the user dictionary file     |
| `nskk-dict-system-dictionary-files`  | `nil`            | List of system dictionary file paths |
| `nskk-dict-cache-enabled`            | `t`              | Enable in-memory dictionary cache    |

### Candidate display

| Variable                                   | Default                   | Description                                |
|--------------------------------------------|---------------------------|--------------------------------------------|
| `nskk-henkan-show-candidates-nth`          | `5`                       | Show candidate list after Nth SPC press    |
| `nskk-henkan-number-to-display-candidates` | `7`                       | Candidates shown per page in echo area     |
| `nskk-henkan-show-candidates-keys`         | `(?a ?s ?d ?f ?j ?k ?l)` | Home-row selection keys for candidate list |

### Display

| Variable                | Default   | Description                                   |
|-------------------------|-----------|-----------------------------------------------|
| `nskk-modeline-format`  | `" %m"`  | Mode-line format (`%m` = mode display string) |
| `nskk-use-color-cursor` | `t`       | Change cursor color to reflect current mode   |

### Example configuration

```elisp
;; Start in hiragana mode instead of ASCII
(setq nskk-state-default-mode 'hiragana)

;; Point to a system dictionary (SKK-JISYO.L from skktools)
(setq nskk-dict-system-dictionary-files
      '("/usr/share/skk/SKK-JISYO.L"))

;; Use AZIK extended romaji
(setq nskk-converter-romaji-style 'azik)

;; Show candidate list on first SPC (aggressive)
(setq nskk-henkan-show-candidates-nth 1)

;; Suppress cursor color changes
(setq nskk-use-color-cursor nil)
```

## Differences from ddskk

| Aspect             | NSKK                                | ddskk                                |
|--------------------|-------------------------------------|--------------------------------------|
| Dependencies       | Zero (Emacs built-ins only)         | ccc, cdb, etc.                       |
| Minimum Emacs      | 29.1                                | Supports older versions and XEmacs   |
| Binding model      | Full lexical-binding everywhere     | Dynamic binding in many files        |
| Architecture       | 7-layer modular, 16 modules         | Monolithic codebase                  |
| Decision logic     | Embedded Prolog engine              | Imperative conditionals              |

## Development

### Requirements

- GNU Make
- Emacs 29.1 or later

Alternatively, use the provided Nix development shell (includes Emacs and
`SKK-JISYO.L`):

```bash
nix develop
```

### Common tasks

```bash
make compile      # Byte-compile all .el files (errors on any warning)
make test         # Run unit and integration tests
make test-unit    # Run unit tests only
make lint         # Run checkdoc on all source files
make package-lint # Run package-lint against nskk.el
make clean        # Remove compiled .elc files
```

To run the full CI check suite in a Nix sandbox (compile + test + lint):

```bash
nix flake check
```

### Adding a new romaji rule

```elisp
;; One-off rule (takes effect immediately):
(nskk-converter-add-rule "thi" "てぃ")

;; Or define a new style with a batch of rules:
(nskk-converter-define-style my-custom-style
  "My custom romaji style."
  ("thi" "てぃ")
  ("dhi" "でぃ"))

(setq nskk-converter-romaji-style 'my-custom-style)
```

## Status

**v0.1 — Early development.**
Core conversion engine and infrastructure are implemented. The full end-to-end
input pipeline is functional. Dictionary registration and AZIK support are
complete.

## License

GPL-3.0-or-later. See [COPYING](COPYING) or
<https://www.gnu.org/licenses/gpl-3.0.html>.
