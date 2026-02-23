;;; nskk-dict-errors.el --- Dictionary error handling -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: Takeshi Umeda <takeokunn@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is part of NSKK (Next-generation SKK).
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Dictionary error handling and recovery module.
;; Provides automatic recovery from dictionary errors.

;;; Code:

(require 'cl-lib)

(defgroup nskk-dict-errors nil
  "Dictionary error handling settings."
  :group 'nskk
  :prefix "nskk-dict-errors-")

(define-error 'nskk-dict-error "Dictionary error")

(provide 'nskk-dict-errors)

;;; nskk-dict-errors.el ends here
