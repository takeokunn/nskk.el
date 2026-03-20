;;; nskk-custom.el --- Customization groups and variables for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
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
;; Core `defgroup' and `defcustom' forms are defined here so that:
;; - The customization group hierarchy is established in a single place.
;; - Individual modules `(require \'nskk-custom)' to access their variables
;;   without needing to declare groups themselves.
;; Optional-module customizations (nskk-server.el, nskk-azik.el,
;; nskk-cache.el, nskk-kana.el, nskk-dictionary.el) define their own
;; `defcustom' and `defgroup' forms near their implementation for
;; self-containment; those groups are listed in the hierarchy below.
;;
;; No Prolog predicates are maintained by this module.
;;
;; Group hierarchy:
;;   nskk
;;   ├── nskk-state      (default mode)
;;   ├── nskk-converter  (auto-start henkan, romaji style)
;;   ├── nskk-search     (sort method, fuzzy threshold, learning file)
;;   ├── nskk-server     (host, port, coding-system, timeout) [defined in nskk-server.el]
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
;; Cursor color faces (defined in this module):
;; - `nskk-use-color-cursor'
;; - `nskk-cursor-hiragana'
;; - `nskk-cursor-katakana'
;; - `nskk-cursor-latin'
;; - `nskk-cursor-jisx0208-latin'
;; - `nskk-cursor-abbrev'

;;; Code:

(require 'cl-lib)

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
  :safe #'symbolp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-state)

;;;; Converter Settings
;;
;; Note: nskk-kana group is defined in nskk-kana.el.

(defgroup nskk-converter nil
  "Romaji to Kana conversion settings."
  :prefix "nskk-converter-"
  :group 'nskk)

(defcustom nskk-converter-auto-start-henkan t
  "Whether to automatically start conversion on uppercase input."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-converter)

(defcustom nskk-converter-romaji-style 'standard
  "Romaji input style for Japanese conversion.
\\='standard - Standard SKK romaji (default)
\\='azik     - AZIK extended romaji with efficiency shortcuts"
  :type '(choice (const :tag "Standard SKK" standard)
                 (const :tag "AZIK" azik))
  :safe #'symbolp
  :package-version '(nskk . "0.1.0")
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
  :safe #'symbolp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defcustom nskk-search-fuzzy-threshold 3
  "Maximum Levenshtein distance threshold for fuzzy search.
Zero disables fuzzy matching."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defcustom nskk-search-learning-file "~/.emacs.d/nskk/learning.dat"
  "File path for persisting learning data."
  :type 'file
  :safe #'stringp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

;;;; UI / Modeline Settings

(defgroup nskk-modeline nil
  "Mode line indicator settings for NSKK."
  :prefix "nskk-modeline-"
  :group 'nskk-ui)

(defcustom nskk-modeline-format " %m"
  "NSKK lighter format string.
\\=%m\\= is an NSKK-specific placeholder replaced with the current input-mode
indicator string (e.g. \"あ\", \"ア\", \"_\").  This placeholder is expanded
internally by NSKK and is distinct from the standard Emacs mode-line \\=%m\\=
construct (which expands to the major-mode name).
The leading space follows the Emacs minor-mode lighter convention,
separating the indicator from adjacent mode indicators in the mode line."
  :type 'string
  :safe #'stringp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-modeline)

(defcustom nskk-use-color-cursor t
  "Whether to change cursor color based on input mode."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-hiragana
  '((((background dark)) (:background "coral4"))
    (t (:background "pink")))
  "Cursor color face for hiragana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-katakana
  '((((background dark)) (:background "forestgreen"))
    (t (:background "green")))
  "Cursor color face for katakana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-latin
  '((((background dark)) (:background "ivory4"))
    (t (:background "gray")))
  "Cursor color face for ASCII/latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-jisx0208-latin
  '((t (:background "gold")))
  "Cursor color face for full-width latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-abbrev
  '((t (:background "royalblue")))
  "Cursor color face for abbrev mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

;;;; Henkan (Conversion) Settings

(defgroup nskk-henkan nil
  "Conversion (henkan) pipeline settings."
  :prefix "nskk-henkan-"
  :group 'nskk-ui)

(defcustom nskk-henkan-show-candidates-nth 5
  "Number of SPC presses before showing candidate list.
After this many candidates shown one-by-one, switch to overlay
candidate list display below the conversion region.
Zero means always show the list immediately."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-henkan)

(defcustom nskk-henkan-number-to-display-candidates 7
  "Number of candidates to display per page in candidate list.
Must be at least 1; should match the number of selection keys in
`nskk-henkan-show-candidates-keys'."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-henkan)

(defcustom nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "Selection keys for candidate list display.
These keys allow direct candidate selection in the overlay candidate list."
  :type '(repeat character)
  :safe (lambda (v) (and (listp v) (cl-every #'characterp v)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-henkan)

(defcustom nskk-max-registration-depth 3
  "Maximum nesting depth for recursive word registration.
When `nskk--registration-depth' reaches this value, further
registration attempts are silently ignored."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-henkan)

;;;; Candidate Window Settings

(defgroup nskk-candidate-window nil
  "Candidate display UI for NSKK."
  :prefix "nskk-candidate-"
  :group 'nskk-ui)

(defcustom nskk-show-tooltip nil
  "When non-nil, display conversion candidates using Emacs tooltip.
Only works in GUI Emacs (not terminal).  When both `nskk-show-inline' and
`nskk-show-tooltip' are non-nil, `nskk-show-inline' takes precedence."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-candidate-window)

;;;; Dynamic Completion Multiple Display Settings

(defgroup nskk-dcomp nil
  "Dynamic completion (dcomp) settings for NSKK."
  :prefix "nskk-dcomp-"
  :group 'nskk-ui)

(defcustom nskk-dcomp-multiple-activate nil
  "When non-nil, display multiple dynamic completion candidates inline.
When enabled, pressing TAB in preedit (\u25bd) mode shows a list of matching
completion candidates directly below the preedit text.  Use TAB or
SHIFT+TAB to cycle through the candidates.
When nil, only the current completion is shown (default behavior)."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

(defcustom nskk-dcomp-multiple-rows 7
  "Maximum number of candidates to display in the dcomp multiple view.
When `nskk-dcomp-multiple-activate' is non-nil, this controls how many
completion candidates appear in the inline list below the preedit."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

(defface nskk-dcomp-face
  '((t (:foreground "DarkKhaki")))
  "Face for the dynamically completed part of the reading (inline suffix)."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

(defface nskk-dcomp-multiple-face
  '((t (:inherit default)))
  "Face for candidate readings in the dcomp multiple display."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

(defface nskk-dcomp-multiple-trailing-face
  '((t (:foreground "DarkKhaki")))
  "Face for trailing part of candidate readings in dcomp multiple display."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

(defface nskk-dcomp-multiple-selected-face
  '((t (:inherit highlight :weight bold)))
  "Face for the currently selected candidate in dcomp multiple display."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dcomp)

;;;; Kakutei Dictionary Settings

(defgroup nskk-kakutei-jisyo nil
  "Confirmed dictionary settings for NSKK."
  :prefix "nskk-kakutei-"
  :group 'nskk)

(defcustom nskk-kakutei-jisyo nil
  "Path to the confirmed (kakutei) dictionary file, or nil to disable.
The confirmed dictionary contains entries that are committed immediately
without showing a candidate selection menu.  When a reading matches an
entry in this dictionary, the single candidate is inserted directly.
The file format is the same as the standard SKK dictionary format."
  :type '(choice file (const nil))
  :safe (lambda (v) (or (null v) (stringp v)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-kakutei-jisyo)

;;;; Debug Settings

(defgroup nskk-debug nil
  "NSKK debugging configuration."
  :prefix "nskk-debug-"
  :group 'nskk)

(defcustom nskk-debug-enabled nil
  "Whether NSKK debug mode is enabled.
When enabled, debug messages are logged to the *NSKK Debug* buffer.
Note: enabling this records individual keystrokes and dictionary query
terms in the debug buffer for diagnostic purposes."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-debug)

(defcustom nskk-debug-max-entries 1000
  "Maximum number of log entries before trimming the debug buffer.
A value of zero causes `nskk--debug-trim' to clear the buffer on
every append (effectively disabling the buffer log)."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-debug)

(provide 'nskk-custom)

;;; nskk-custom.el ends here
