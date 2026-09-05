;;; nskk-region.el --- Region operation commands for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

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

;; Region operation commands for NSKK.

;;; Code:

(require 'nskk-kana)

;;;; Internal Helpers

(defun nskk--region-bounds ()
  "Return normalized bounds of the active region for interactive commands."
  (unless (use-region-p)
    (user-error "No active region"))
  (list (region-beginning) (region-end)))

(defun nskk--region-convert (beg end converter)
  "Convert text from BEG to END using CONVERTER function.
CONVERTER takes a string and must return a string.
Replaces the region text with the converted result.
On failure, restore point, mark, and mark activation exactly."
  (let ((saved-point (point))
        (saved-mark (mark t))
        (saved-mark-active mark-active))
    (condition-case err
        (atomic-change-group
          (let* ((text (buffer-substring-no-properties beg end))
                 (converted (funcall converter text)))
            (delete-region beg end)
            (goto-char beg)
            (insert converted)))
      ((error quit)
       (goto-char saved-point)
       (set-marker (mark-marker) saved-mark)
       (setq mark-active saved-mark-active)
       (signal (car err) (cdr err))))))

(defun nskk--ascii-char-to-zenkaku (char)
  "Convert ASCII CHAR (integer) to full-width Unicode equivalent.
Only converts printable ASCII (0x20-0x7E).
Returns a string of the converted character."
  (char-to-string (nskk-jisx0208-latin-char char)))

(defun nskk--string-ascii-to-zenkaku (str)
  "Convert all ASCII printable characters in STR to full-width equivalents."
  (mapconcat #'nskk--ascii-char-to-zenkaku str ""))

(defun nskk--zenkaku-char-to-ascii (char)
  "Convert full-width Unicode CHAR to ASCII equivalent.
Converts Ideographic space (U+3000) to ASCII space, and full-width
ASCII variants (0xFF01-0xFF5E) to basic ASCII (0x21-0x7E).
Returns a string of the converted character."
  (cond
   ((= char #x3000) " ")
   ((and (>= char #xFF01) (<= char #xFF5E))
    (char-to-string (- char #xFEE0)))
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
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk-kana-string-katakana-to-hiragana))

;;;###autoload
(defun nskk-katakana-region (beg end)
  "Convert hiragana characters in region BEG to END to katakana.
Operates on the active region when called interactively.
This is the nskk.el equivalent of ddskk's `skk-katakana-region'."
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk-kana-string-hiragana-to-katakana))

;;;###autoload
(defun nskk-hankaku-katakana-region (beg end)
  "Convert full-width katakana in region BEG to END to half-width katakana.
Operates on the active region when called interactively."
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk-kana-zenkaku-to-hankaku))

;;;###autoload
(defun nskk-zenkaku-katakana-region (beg end)
  "Convert half-width katakana in region BEG to END to full-width katakana.
Operates on the active region when called interactively."
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk-kana-hankaku-to-zenkaku))

;;;###autoload
(defun nskk-jisx0208-latin-region (beg end)
  "Convert ASCII in region BEG to END to full-width (JIS X 0208) equivalents.
Converts printable ASCII (0x20-0x7E) to full-width Unicode variants.
Equivalent of ddskk's `skk-jisx0208-latin-region'."
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk--string-ascii-to-zenkaku))

;;;###autoload
(defun nskk-latin-region (beg end)
  "Convert full-width latin in region BEG to END to ASCII equivalents.
Converts full-width ASCII variants (U+FF01-U+FF5E) and ideographic
space (U+3000) to basic ASCII.  Equivalent of ddskk's `skk-latin-region'."
  (interactive (nskk--region-bounds))
  (nskk--region-convert beg end #'nskk--string-zenkaku-to-ascii))

(provide 'nskk-region)

;;; nskk-region.el ends here
