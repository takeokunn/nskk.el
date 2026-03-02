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
  :group 'nskk-state)

(defcustom nskk-state-undo-limit 100
  "Maximum number of undo operations to keep in history."
  :type 'integer
  :safe #'integerp
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
  :safe #'booleanp
  :group 'nskk-converter)

(defcustom nskk-converter-n-processing-mode 'smart
  "How to process \\='n' for \\='ん' (hiragana) or \\='ン' (katakana).
\\='smart means auto-detect based on context.
\\='strict means \\='nn' is required.
\\='loose means single \\='n' is sufficient."
  :type '(choice (const :tag "Smart (auto)" smart)
                 (const :tag "Strict (nn required)" strict)
                 (const :tag "Loose (single n ok)" loose))
  :safe #'symbolp
  :group 'nskk-converter)

(defcustom nskk-converter-auto-start-henkan t
  "Whether to automatically start conversion on uppercase input."
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-converter)

(defcustom nskk-converter-romaji-style 'standard
  "Romaji input style for Japanese conversion.
\\='standard - Standard SKK romaji (default)
\\='azik     - AZIK extended romaji with efficiency shortcuts"
  :type '(choice (const :tag "Standard SKK" standard)
                 (const :tag "AZIK" azik))
  :safe #'symbolp
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
  :group 'nskk-search)

(defcustom nskk-search-fuzzy-threshold 3
  "Maximum Levenshtein distance threshold for fuzzy search."
  :type 'integer
  :safe #'integerp
  :group 'nskk-search)

(defcustom nskk-search-enable-cache t
  "Enable search result caching when non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-search)

(defcustom nskk-search-learning-file "~/.emacs.d/nskk/learning.dat"
  "File path for persisting learning data."
  :type 'file
  :safe #'stringp
  :group 'nskk-search)

(defcustom nskk-search-auto-save t
  "When non-nil, automatically save learning data periodically."
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-search)

(defcustom nskk-search-auto-save-interval 300
  "Auto-save interval in seconds for learning data."
  :type 'integer
  :safe #'integerp
  :group 'nskk-search)

;;;; Multiple Jisyo (Dictionary Files) Settings

(defcustom nskk-jisyo-files nil
  "List of SKK dictionary file paths to load, in priority order.
When non-nil, these files are loaded as system dictionaries in addition
to any auto-detected dictionaries.  Each file should be a valid path to
an SKK-format dictionary file (e.g. SKK-JISYO.L).

Example configuration:
  (setq nskk-jisyo-files
        (list \"/path/to/SKK-JISYO.L\"
              \"/path/to/SKK-JISYO.jinmei\"))

DDSKK equivalent: skk-search-prog-list with multiple jisyo entries."
  :type '(repeat file)
  :group 'nskk-search)

;;;; UI / Modeline Settings

(defgroup nskk-modeline nil
  "Mode line indicator settings for NSKK."
  :prefix "nskk-modeline-"
  :group 'nskk-ui)

(defcustom nskk-modeline-format " %m"
  "Modeline format string.
%m is replaced with the mode name.
The leading space follows the Emacs minor-mode lighter convention,
separating the indicator from adjacent mode indicators in the mode line."
  :type 'string
  :safe #'stringp
  :group 'nskk-modeline)

(defcustom nskk-use-color-cursor t
  "Whether to change cursor color based on input mode."
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-ui)

(defface nskk-cursor-hiragana
  '((((background dark)) (:background "coral4"))
    (t (:background "pink")))
  "Cursor color face for hiragana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :group 'nskk-ui)

(defface nskk-cursor-katakana
  '((((background dark)) (:background "forestgreen"))
    (t (:background "green")))
  "Cursor color face for katakana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :group 'nskk-ui)

(defface nskk-cursor-latin
  '((((background dark)) (:background "ivory4"))
    (t (:background "gray")))
  "Cursor color face for ASCII/latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :group 'nskk-ui)

(defface nskk-cursor-jisx0208-latin
  '((t (:background "gold")))
  "Cursor color face for full-width latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :group 'nskk-ui)

(defface nskk-cursor-abbrev
  '((t (:background "royalblue")))
  "Cursor color face for abbrev mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :group 'nskk-ui)

;;;; Henkan (Conversion) Settings

(defgroup nskk-henkan nil
  "Conversion (henkan) pipeline settings."
  :prefix "nskk-henkan-"
  :group 'nskk-ui)

(defcustom nskk-henkan-show-candidates-nth 5
  "Number of SPC presses before showing candidate list.
After this many candidates shown one-by-one, switch to overlay
candidate list display below the conversion region."
  :type 'integer
  :safe #'integerp
  :group 'nskk-henkan)

(defcustom nskk-henkan-number-to-display-candidates 7
  "Number of candidates to display per page in candidate list."
  :type 'integer
  :safe #'integerp
  :group 'nskk-henkan)

(defcustom nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "Selection keys for candidate list display.
These keys allow direct candidate selection in the overlay candidate list."
  :type '(repeat character)
  :safe (lambda (v) (and (listp v) (cl-every #'characterp v)))
  :group 'nskk-henkan)

(defcustom nskk-max-registration-depth 3
  "Maximum nesting depth for recursive word registration.
When `nskk--registration-depth' reaches this value, further
registration attempts are silently ignored."
  :type 'integer
  :safe #'natnump
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
When enabled, debug messages are logged to the *NSKK Debug* buffer.
Note: enabling this records individual keystrokes and dictionary query
terms in the debug buffer for diagnostic purposes."
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-debug)

(defcustom nskk-debug-max-entries 1000
  "Maximum number of log entries before trimming the debug buffer."
  :type 'integer
  :safe #'integerp
  :group 'nskk-debug)

(provide 'nskk-custom)

;;; nskk-custom.el ends here
