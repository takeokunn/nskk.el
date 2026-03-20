;;; nskk-region.el --- Region operation commands for NSKK -*- lexical-binding: t; -*-

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

;; Region-based text conversion commands for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-kana and nskk-cps-macros.
;;
;; Provides M-x commands to convert text in the active region between
;; different Japanese character scripts.  These are the nskk.el equivalents
;; of ddskk's region conversion commands (skk-hiragana-region, etc.).
;;
;; Commands:
;; - `nskk-hiragana-region'        -- Convert katakana in region to hiragana
;; - `nskk-katakana-region'        -- Convert hiragana in region to katakana
;; - `nskk-hankaku-katakana-region' -- Convert zenkaku katakana to hankaku
;; - `nskk-zenkaku-katakana-region' -- Convert hankaku katakana to zenkaku
;; - `nskk-jisx0208-latin-region'  -- Convert ASCII in region to full-width latin
;; - `nskk-latin-region'           -- Convert full-width latin in region to ASCII
;;
;; All commands operate on the current region (mark..point) and replace it
;; with the converted text.  If the region is not active, the commands do
;; nothing and show a message.

;;; Code:

(require 'nskk-kana)
(require 'nskk-cps-macros)

;;;; Internal Helpers

(defun nskk--region-convert (beg end converter)
  "Convert text from BEG to END using CONVERTER function.
CONVERTER takes a string and returns the converted string.
Replaces the region text with the converted result."
  (let* ((text (buffer-substring-no-properties beg end))
         (converted (funcall converter text)))
    (when converted
      (delete-region beg end)
      (goto-char beg)
      (insert converted))))

(defun nskk--ascii-char-to-zenkaku (char)
  "Convert ASCII CHAR (integer) to full-width Unicode equivalent.
Only converts printable ASCII (0x20-0x7E).
Returns a string of the converted character."
  (cond
   ;; Space (0x20) -> Ideographic space (U+3000)
   ((= char #x20) "\u3000")
   ;; Printable ASCII (0x21-0x7E) -> Full-width (0xFF01-0xFF5E)
   ((and (>= char #x21) (<= char #x7E))
    (char-to-string (+ char #xFEE0)))
   ;; Other chars pass through
   (t (char-to-string char))))

(defun nskk--string-ascii-to-zenkaku (str)
  "Convert all ASCII printable characters in STR to full-width equivalents."
  (mapconcat #'nskk--ascii-char-to-zenkaku str ""))

(defun nskk--zenkaku-char-to-ascii (char)
  "Convert full-width Unicode CHAR to ASCII equivalent.
Converts Ideographic space (U+3000) to ASCII space, and full-width
ASCII variants (0xFF01-0xFF5E) to basic ASCII (0x21-0x7E).
Returns a string of the converted character."
  (cond
   ;; Ideographic space (U+3000) -> ASCII space
   ((= char #x3000) " ")
   ;; Full-width ASCII variants (0xFF01-0xFF5E) -> basic ASCII
   ((and (>= char #xFF01) (<= char #xFF5E))
    (char-to-string (- char #xFEE0)))
   ;; Other chars pass through
   (t (char-to-string char))))

(defun nskk--string-zenkaku-to-ascii (str)
  "Convert all full-width ASCII variants in STR to basic ASCII equivalents."
  (mapconcat #'nskk--zenkaku-char-to-ascii str ""))

;;;; Public Commands

;;;###autoload
(defun nskk-hiragana-region (beg end)
  "Convert katakana characters in region BEG to END to hiragana.
Operates on the active region when called interactively.
This is the nskk.el equivalent of ddskk's `skk-hiragana-region'."
  (interactive "r")
  (nskk--region-convert beg end #'nskk-kana-string-katakana-to-hiragana))

;;;###autoload
(defun nskk-katakana-region (beg end)
  "Convert hiragana characters in region BEG to END to katakana.
Operates on the active region when called interactively.
This is the nskk.el equivalent of ddskk's `skk-katakana-region'."
  (interactive "r")
  (nskk--region-convert beg end #'nskk-kana-string-hiragana-to-katakana))

;;;###autoload
(defun nskk-hankaku-katakana-region (beg end)
  "Convert full-width katakana in region BEG to END to half-width katakana.
Operates on the active region when called interactively."
  (interactive "r")
  (nskk--region-convert beg end #'nskk-kana-zenkaku-to-hankaku))

;;;###autoload
(defun nskk-zenkaku-katakana-region (beg end)
  "Convert half-width katakana in region BEG to END to full-width katakana.
Operates on the active region when called interactively."
  (interactive "r")
  (nskk--region-convert beg end #'nskk-kana-hankaku-to-zenkaku))

;;;###autoload
(defun nskk-jisx0208-latin-region (beg end)
  "Convert ASCII in region BEG to END to full-width (JIS X 0208) equivalents.
Converts printable ASCII (0x20-0x7E) to full-width Unicode variants.
Equivalent of ddskk's `skk-jisx0208-latin-region'."
  (interactive "r")
  (nskk--region-convert beg end #'nskk--string-ascii-to-zenkaku))

;;;###autoload
(defun nskk-latin-region (beg end)
  "Convert full-width latin in region BEG to END to ASCII equivalents.
Converts full-width ASCII variants (U+FF01-U+FF5E) and ideographic
space (U+3000) to basic ASCII.  Equivalent of ddskk's `skk-latin-region'."
  (interactive "r")
  (nskk--region-convert beg end #'nskk--string-zenkaku-to-ascii))

(provide 'nskk-region)

;;; nskk-region.el ends here
