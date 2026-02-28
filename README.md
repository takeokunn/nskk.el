# NSKK - Next-generation SKK for Emacs

<!-- MELPA badge (to be added after acceptance) -->
<!-- [![MELPA](https://melpa.org/packages/nskk-badge.svg)](https://melpa.org/#/nskk) -->
<!-- [![MELPA Stable](https://stable.melpa.org/packages/nskk-badge.svg)](https://stable.melpa.org/#/nskk) -->

NSKK is a modern, zero-dependency Japanese input method for Emacs based on
[SKK](https://skk-dev.github.io/ddskk/) (Simple Kana to Kanji) principles.
It provides a clean reimplementation with a 7-layer modular architecture,
full lexical-binding, and aggressive performance targets.

## Features

- **Zero external dependencies** -- relies only on Emacs built-in features
- **Full lexical-binding** throughout every source file
- **Embedded Prolog engine** -- conversion and dispatch logic expressed as facts/rules
- **High-performance design** -- targets < 1ms key response, < 10ms dictionary search
- **ddskk-compatible** key bindings and input behavior
- **AZIK extended romaji** support (optional, via `nskk-azik.el`)

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

With optional AZIK and custom dictionary:

```elisp
(use-package nskk
  :ensure t
  :custom
  (nskk-state-default-mode 'hiragana)
  (nskk-dict-system-dictionary-files '("/usr/share/skk/SKK-JISYO.L"))
  (nskk-converter-romaji-style 'azik)
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
;; Toggle in current buffer with C-x C-j
```

## Key Bindings

All bindings are active inside `nskk-mode`. They follow ddskk conventions.

### Mode Control

| Key         | Function              | Description                                   |
|-------------|-----------------------|-----------------------------------------------|
| `C-x C-j`   | `nskk-toggle-mode`    | Toggle NSKK on/off in current buffer          |
| `C-j`       | `nskk-kakutei`        | Commit / enter hiragana from ASCII mode       |
| `q`         | `nskk-handle-q`       | Toggle hiragana/katakana                       |
| `l`         | `nskk-handle-l`       | Switch to ASCII (latin) mode                  |
| `L`         | `nskk-handle-upper-l` | Switch to full-width latin (JIS X 0208) mode  |
| `/`         | `nskk-handle-slash`   | Enter abbrev mode                             |

### Conversion (henkan)

| Key         | Function                     | Description                              |
|-------------|------------------------------|------------------------------------------|
| `SPC`       | `nskk-handle-space`          | Start conversion (▽→▼) / next candidate |
| `x`         | `nskk-handle-x`              | Previous candidate                       |
| `RET`       | `nskk-handle-return`         | Commit candidate and insert newline      |
| `C-g`       | `nskk-handle-cancel`         | Cancel conversion or preedit             |
| `a`-`l`     | (candidate list keys)        | Select candidate directly in list mode   |

### Input Modes

| Mode           | Display | Description                       |
|----------------|---------|-----------------------------------|
| `hiragana`     | `かな`  | Default Japanese input mode       |
| `katakana`     | `カナ`  | Katakana input mode               |
| `ascii`        | `SKK`   | Direct ASCII (pass-through) mode  |
| `latin`        | `SKK`   | Direct ASCII mode (same as ascii) |
| `jisx0208-latin` | `全英` | Full-width latin (JIS X 0208)    |
| `abbrev`       | `aA`    | Abbreviation mode                 |

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

| Variable                            | Default       | Description                                      |
|-------------------------------------|---------------|--------------------------------------------------|
| `nskk-state-default-mode`           | `ascii`       | Initial mode when NSKK is activated              |
| `nskk-converter-romaji-style`       | `standard`    | Romaji style: `standard` or `azik`              |
| `nskk-converter-auto-start-henkan`  | `t`           | Uppercase letter auto-starts conversion          |
| `nskk-converter-n-processing-mode`  | `smart`       | ん detection: `smart`, `strict`, or `loose`     |
| `nskk-converter-use-sokuon`         | `t`           | Enable っ doubling from repeated consonants      |

### Dictionary

| Variable                             | Default                     | Description                            |
|--------------------------------------|-----------------------------|----------------------------------------|
| `nskk-dict-user-dictionary-file`     | `~/.skk/jisyo`              | Path to the user dictionary file       |
| `nskk-dict-system-dictionary-files`  | `nil`                       | List of system dictionary file paths   |
| `nskk-dict-cache-enabled`            | `t`                         | Enable in-memory dictionary cache      |

### Candidate display

| Variable                               | Default                       | Description                                   |
|----------------------------------------|-------------------------------|-----------------------------------------------|
| `nskk-henkan-show-candidates-nth`      | `5`                           | Show candidate list after Nth SPC press       |
| `nskk-henkan-number-to-display-candidates` | `7`                      | Candidates shown per page in echo area        |
| `nskk-henkan-show-candidates-keys`     | `(?a ?s ?d ?f ?j ?k ?l)`     | Home-row selection keys for candidate list    |

### Display

| Variable                   | Default    | Description                                   |
|----------------------------|------------|-----------------------------------------------|
| `nskk-modeline-format`     | `"[%m]"`  | Mode-line format (`%m` = mode display string) |
| `nskk-use-color-cursor`    | `t`        | Change cursor color to reflect current mode   |

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

## Development Setup

### Running tests

Tests use the built-in `ert` framework. From the project root:

```bash
emacs --batch -L . -L test \
  --eval "(require 'nskk)" \
  -l test/unit/nskk-prolog-test.el \
  -f ert-run-tests-batch-and-exit
```

To run all unit tests at once:

```bash
emacs --batch -L . -L test \
  $(find test/unit -name '*.el' | sort | perl -ne 'chomp; print "-l $_ "') \
  -f ert-run-tests-batch-and-exit
```

### Byte-compilation check

```bash
emacs --batch -L . \
  -f batch-byte-compile \
  nskk-custom.el nskk-prolog.el nskk-cache.el nskk-kana.el \
  nskk-dictionary.el nskk-state.el nskk-converter.el nskk-search.el \
  nskk-henkan.el nskk-azik.el nskk-input.el \
  nskk-keymap.el nskk-modeline.el nskk-candidate-window.el nskk-debug.el \
  nskk.el
```

Byte-compilation must complete with zero errors and zero warnings before any
pull request is merged.

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

**v0.1 -- Early development.**
Core conversion engine and infrastructure are implemented. Full end-to-end
input pipeline is functional. Dictionary registration and AZIK support are
complete. MELPA submission is pending.

## License

GPL-3.0-or-later. See [COPYING](COPYING) or
<https://www.gnu.org/licenses/gpl-3.0.html>.
