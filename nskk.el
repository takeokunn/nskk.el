;;; nskk.el --- Nerima SKK(Simple Kana to Kanji conversion program) -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Takeo Obara

;; Author: Takeo Obara <bararararatty@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29"))
;; Homepage: https://github.com/takeokunn/nskk.el
;; Keywords: japanese, input method
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
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

;; Nerima SKK(Simple Kana to Kanji conversion program)

;;; Code:

(define-minor-mode nskk-mode
  "Minor mode for nskk-mode"
  :lighter "nskk"
  :group 'nskk)

(provide 'nskk)
;;; ob-phpstan.el ends here
