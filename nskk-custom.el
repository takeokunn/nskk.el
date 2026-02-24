;;; nskk-custom.el --- NSKK customization variables -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Customization variables (defgroup/defcustom) for NSKK.
;; All user-configurable options are defined here.

;;; Code:

(require 'cl-lib)

(defgroup nskk nil
  "NSKK - Next-generation SKK with zero dependencies and extreme performance."
  :prefix "nskk-"
  :group 'i18n
  :link '(url-link :tag "GitHub" "https://github.com/takeokunn/nskk.el"))

(defgroup nskk-core nil
  "Core conversion and state management settings."
  :prefix "nskk-"
  :group 'nskk)

(defgroup nskk-converter nil
  "Romaji to Kana conversion settings."
  :prefix "nskk-converter-"
  :group 'nskk-core)

(defgroup nskk-state nil
  "State management settings."
  :prefix "nskk-state-"
  :group 'nskk-core)

(defgroup nskk-dictionary nil
  "Dictionary and search settings."
  :prefix "nskk-dict-"
  :group 'nskk)

(defgroup nskk-cache nil
  "Cache settings."
  :prefix "nskk-cache-"
  :group 'nskk)

(defgroup nskk-ui nil
  "UI components settings."
  :prefix "nskk-"
  :group 'nskk)

;; Converter settings
(defcustom nskk-converter-use-sokuon t
  "Whether to enable automatic sokuon (small tsu) conversion."
  :type 'boolean
  :group 'nskk-converter)

(defcustom nskk-converter-n-processing-mode 'smart
  "How to process \\='n' for \\='ん' (hiragana) or \\='ン' (katakana).
\\='smart means auto-detect based on context.
\\='strict means \\='nn' is required.
\\='loose means single \\='n' is sufficient."
  :type '(choice (const :tag "Smart (auto)" smart)
                 (const :tag "Strict (nn required)" strict)
                 (const :tag "Loose (single n ok)" loose))
  :group 'nskk-converter)

(defcustom nskk-converter-auto-start-henkan t
  "Whether to automatically start conversion on uppercase input."
  :type 'boolean
  :group 'nskk-converter)

(defcustom nskk-converter-romaji-style 'standard
  "Romaji input style for Japanese conversion.
\\='standard - Standard SKK romaji (default)
\\='azik     - AZIK extended romaji with efficiency shortcuts"
  :type '(choice (const :tag "Standard SKK" standard)
                 (const :tag "AZIK" azik))
  :group 'nskk-converter)

(defgroup nskk-azik nil
  "AZIK extended romaji input settings."
  :prefix "nskk-azik-"
  :group 'nskk-converter)

(defcustom nskk-azik-q-behavior 'context-aware
  "Behavior of q key when AZIK style is active.
\\='context-aware - Produce ん when there is pending romaji input,
                 otherwise toggle hiragana/katakana mode (recommended)
\\='always-n      - Always produce ん (AZIK purist)
\\='toggle-only   - Keep standard SKK toggle behavior (use nn for ん)"
  :type '(choice (const :tag "Context-aware (Recommended)" context-aware)
                 (const :tag "Always produce ん" always-n)
                 (const :tag "Toggle only" toggle-only))
  :group 'nskk-azik)

(defcustom nskk-azik-keyboard-type 'jp106
  "Keyboard layout for AZIK mappings.
Affects key position-based shortcuts.
\\='jp106 - Japanese 106-key layout (default)
\\='us101 - US 101-key layout"
  :type '(choice (const :tag "Japanese 106-key" jp106)
                 (const :tag "US 101-key" us101))
  :group 'nskk-azik)

;; State settings
(defcustom nskk-state-default-mode 'ascii
  "Default input mode when NSKK is activated."
  :type '(choice (const :tag "ASCII" ascii)
                 (const :tag "Hiragana" hiragana)
                 (const :tag "Katakana" katakana))
  :group 'nskk-state)

(defcustom nskk-state-undo-limit 100
  "Maximum number of undo operations to keep in history."
  :type 'integer
  :group 'nskk-state)

;; Dictionary settings
(defcustom nskk-dict-user-dictionary-file
  (expand-file-name "~/.skk/jisyo")
  "Path to the user dictionary file."
  :type 'file
  :group 'nskk-dictionary)

(defcustom nskk-dict-system-dictionary-files
  (list (expand-file-name "/usr/share/skk/SKK-JISYO.L"))
  "List of system dictionary files to load."
  :type '(repeat file)
  :group 'nskk-dictionary)

(defcustom nskk-dict-cache-enabled t
  "Whether to enable dictionary caching."
  :type 'boolean
  :group 'nskk-dictionary)

;; Cache settings
(defcustom nskk-cache-default-capacity 1000
  "Default cache capacity for LRU/LFU caches."
  :type 'integer
  :group 'nskk-cache)

(defcustom nskk-cache-strategy 'lru
  "Cache eviction strategy.
\\='lru means Least Recently Used.
\\='lfu means Least Frequently Used."
  :type '(choice (const :tag "LRU" lru)
                 (const :tag "LFU" lfu))
  :group 'nskk-cache)

;; UI settings
(defcustom nskk-candidate-window-page-size 7
  "Number of candidates to display per page in the candidate window."
  :type 'integer
  :group 'nskk-ui)

(defcustom nskk-candidate-window-use-annotation t
  "Whether to show annotations in the candidate window."
  :type 'boolean
  :group 'nskk-ui)

(defcustom nskk-candidate-window-position 'bottom
  "Where to display the candidate window.
\\='bottom means below the current line.
\\='top means above the current line.
\\='inline means inline with the text."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Top" top)
                 (const :tag "Inline" inline))
  :group 'nskk-ui)

(defcustom nskk-minibuffer-show-inline-candidate t
  "Whether to show inline candidate preview in minibuffer."
  :type 'boolean
  :group 'nskk-ui)

(defcustom nskk-modeline-format "[%m%s]"
  "Modeline format string.
%m is replaced with the mode name.
%s is replaced with the state indicator."
  :type 'string
  :group 'nskk-ui)

(defcustom nskk-modeline-mode-names
  '((ascii . "A")
    (hiragana . "あ")
    (katakana . "ア")
    (abbrev . "aA")
    (latin . "L"))
  "Mode name display mapping for modeline."
  :type 'alist
  :group 'nskk-ui)

;; Dictionary path variables
(defcustom nskk-large-dictionary nil
  "Path to large SKK dictionary file."
  :type '(choice file (const nil))
  :group 'nskk-dictionary)

(provide 'nskk-custom)

;;; nskk-custom.el ends here
