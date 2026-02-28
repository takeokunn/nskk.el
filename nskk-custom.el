;;; nskk-custom.el --- Customization groups and variables for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
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

;; Centralized customization variables for NSKK (Layer 0: Foundation).
;;
;; Layer position: L0 (Foundation) -- no dependencies on other NSKK modules.
;;
;; All `defgroup' and `defcustom' forms for the entire package are defined
;; here so that:
;; - The customization group hierarchy is established in a single place.
;; - Individual modules `(require \'nskk-custom)' to access their variables
;;   without needing to declare groups themselves.
;;
;; No Prolog predicates are maintained by this module.
;;
;; Group hierarchy:
;;   nskk
;;   ├── nskk-state      (default mode, undo limit)
;;   ├── nskk-converter  (romaji style, sokuon, n-processing)
;;   ├── nskk-search     (sort method, fuzzy threshold, learning file)
;;   ├── nskk-cache      (strategy, capacity)  [defined in nskk-cache.el]
;;   ├── nskk-kana       (kana classification settings) [defined in nskk-kana.el]
;;   ├── nskk-dictionary (dict files, cache enable) [defined in nskk-dictionary.el]
;;   ├── nskk-azik       (q behavior, keyboard type) [defined in nskk-azik.el]
;;   └── nskk-ui
;;       ├── nskk-modeline       (format string)
;;       ├── nskk-henkan         (show-nth, page size, selection keys)
;;       ├── nskk-candidate-window
;;       └── nskk-debug          (enabled, max-entries)
;;
;; Cursor color variables (defined in this module):
;; - `nskk-use-color-cursor'
;; - `nskk-cursor-hiragana-color'
;; - `nskk-cursor-katakana-color'
;; - `nskk-cursor-latin-color'
;; - `nskk-cursor-jisx0208-latin-color'
;; - `nskk-cursor-abbrev-color'

;;; Code:

;;;; Top-Level Groups

(defgroup nskk nil
  "NSKK - Next-generation SKK with zero dependencies and extreme performance."
  :prefix "nskk-"
  :group 'i18n
  :link '(url-link :tag "GitHub" "https://github.com/takeokunn/nskk.el"))

(defgroup nskk-ui nil
  "UI components settings."
  :prefix "nskk-"
  :group 'nskk)

;;;; State Settings

(defgroup nskk-state nil
  "State management settings."
  :prefix "nskk-state-"
  :group 'nskk)

(defcustom nskk-state-default-mode 'ascii
  "Default input mode when NSKK is activated."
  :type '(choice (const :tag "ASCII" ascii)
                 (const :tag "Hiragana" hiragana)
                 (const :tag "Katakana" katakana)
                 (const :tag "Full-width Latin" jisx0208-latin))
  :group 'nskk-state)

(defcustom nskk-state-undo-limit 100
  "Maximum number of undo operations to keep in history."
  :type 'integer
  :group 'nskk-state)

;;;; Converter Settings
;;
;; Note: nskk-kana group is defined in nskk-kana.el.

(defgroup nskk-converter nil
  "Romaji to Kana conversion settings."
  :prefix "nskk-converter-"
  :group 'nskk)

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

;;;; Search Settings
;;
;; Note: nskk-dictionary group is defined in nskk-dictionary.el.

(defgroup nskk-search nil
  "SKK dictionary search customization."
  :group 'nskk
  :prefix "nskk-search-")

(defcustom nskk-search-sort-method 'frequency
  "Sort method for search results."
  :type '(choice (const :tag "Frequency order" frequency)
                 (const :tag "Kana order" kana)
                 (const :tag "No sorting" none))
  :group 'nskk-search)

(defcustom nskk-search-fuzzy-threshold 3
  "Maximum Levenshtein distance threshold for fuzzy search."
  :type 'integer
  :group 'nskk-search)

(defcustom nskk-search-enable-cache t
  "Enable search result caching when non-nil."
  :type 'boolean
  :group 'nskk-search)

(defcustom nskk-search-learning-file "~/.emacs.d/nskk/learning.dat"
  "File path for persisting learning data."
  :type 'file
  :group 'nskk-search)

(defcustom nskk-search-auto-save t
  "When non-nil, automatically save learning data periodically."
  :type 'boolean
  :group 'nskk-search)

(defcustom nskk-search-auto-save-interval 300
  "Auto-save interval in seconds for learning data."
  :type 'integer
  :group 'nskk-search)

;;;; UI / Modeline Settings

(defgroup nskk-modeline nil
  "Mode line indicator settings for NSKK."
  :prefix "nskk-modeline-"
  :group 'nskk-ui)

(defcustom nskk-modeline-format "[%m]"
  "Modeline format string.
%m is replaced with the mode name."
  :type 'string
  :group 'nskk-modeline)

(defcustom nskk-use-color-cursor t
  "Whether to change cursor color based on input mode."
  :type 'boolean
  :group 'nskk-ui)

(defcustom nskk-cursor-hiragana-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "coral4" "pink")
  "Cursor color for hiragana mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-katakana-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "forestgreen" "green")
  "Cursor color for katakana mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-latin-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "ivory4" "gray")
  "Cursor color for ASCII/latin mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-jisx0208-latin-color "gold"
  "Cursor color for full-width latin mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-abbrev-color "royalblue"
  "Cursor color for abbrev mode."
  :type 'color
  :group 'nskk-ui)

;;;; Henkan (Conversion) Settings

(defgroup nskk-henkan nil
  "Conversion (henkan) pipeline settings."
  :prefix "nskk-henkan-"
  :group 'nskk-ui)

(defcustom nskk-henkan-show-candidates-nth 5
  "Number of SPC presses before showing candidate list.
After this many candidates shown one-by-one, switch to echo area
candidate list display with selection keys."
  :type 'integer
  :group 'nskk-henkan)

(defcustom nskk-henkan-number-to-display-candidates 7
  "Number of candidates to display per page in candidate list."
  :type 'integer
  :group 'nskk-henkan)

(defcustom nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "Selection keys for candidate list display.
These keys allow direct candidate selection in the echo area list."
  :type '(repeat character)
  :group 'nskk-henkan)

;;;; Candidate Window Settings

(defgroup nskk-candidate-window nil
  "Candidate display UI for NSKK."
  :prefix "nskk-candidate-"
  :group 'nskk-ui)

;;;; Debug Settings

(defgroup nskk-debug nil
  "NSKK debugging configuration."
  :prefix "nskk-debug-"
  :group 'nskk)

(defcustom nskk-debug-enabled nil
  "Whether NSKK debug mode is enabled.
When enabled, debug messages are logged to the *NSKK Debug* buffer."
  :type 'boolean
  :group 'nskk-debug)

(defcustom nskk-debug-max-entries 1000
  "Maximum number of log entries before trimming the debug buffer."
  :type 'integer
  :group 'nskk-debug)

(provide 'nskk-custom)

;;; nskk-custom.el ends here
