;;; nskk-input-routing-pbt-test.el --- PBT for input routing by mode -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Contributors
;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based
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
;;; Code:
(require 'ert)

(require 'nskk-test-framework)

(require 'nskk-test-macros)

(require 'nskk-pbt-generators)

(require 'nskk-pbt-shrink)

(require 'nskk-state)

(require 'nskk-input)

;;;;
;;;; Helper Macros
;;;;
(defmacro nskk-input-routing-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk-input-routing-test--saved-romaji-buffer
          (nskk-state-romaji-buffer)))
     (nskk-state-set-romaji-buffer "")
     (unwind-protect
         (progn
           ,@body)
       (nskk-state-set-romaji-buffer
        nskk-input-routing-test--saved-romaji-buffer))))

;;;;
;;;; Generator for direct-insert modes
;;;;
(nskk-register-generator 'direct-insert-mode
                         (lambda (&rest _args)
                           (nskk--pbt-random-choice '(ascii latin))))

;;;;
;;;; Property 1: Direct-Insert Modes Insert Characters Directly
;;;;
(nskk-property-test nskk-pbt-ascii-mode-direct-insert
                    ((char lowercase-char))
                    (let ((char-val (string-to-char char)))
                      (nskk-input-routing-test-with-state 'ascii
                                                          (with-temp-buffer
                                                           (setq last-command-event char-val)
                                                           (nskk-self-insert 1)
                                                           (should
                                                            (string=
                                                             (buffer-string)
                                                             char)))))
                    50)

(nskk-property-test nskk-pbt-latin-mode-direct-insert
                    ((char lowercase-char))
                    (let ((char-val (string-to-char char)))
                      (nskk-input-routing-test-with-state 'latin
                                                          (with-temp-buffer
                                                           (setq last-command-event char-val)
                                                           (nskk-self-insert 1)
                                                           (should
                                                            (string=
                                                             (buffer-string)
                                                             char)))))
                    50)

;;;;
;;;; Property 2: ASCII and Latin Modes Are Equivalent for Input Routing
;;;;
(nskk-property-test nskk-pbt-ascii-latin-equivalent
                    ((char lowercase-char))
                    (let ((char-val (string-to-char char))
                          ascii-result
                          latin-result)
                      (nskk-input-routing-test-with-state 'ascii
                                                          (with-temp-buffer
                                                           (setq last-command-event char-val)
                                                           (nskk-self-insert 1)
                                                           (setq ascii-result (buffer-string))))
                      (nskk-input-routing-test-with-state 'latin
                                                          (with-temp-buffer
                                                           (setq last-command-event char-val)
                                                           (nskk-self-insert 1)
                                                           (setq latin-result (buffer-string))))
                      (should (string= ascii-result latin-result)))
                    50)

;;;;
;;;; Property 3: Japanese Modes Convert Romaji to Kana
;;;;
(nskk-property-test nskk-pbt-hiragana-mode-converts
                    ((dummy lowercase-char))
                    (let* ((vowel-map
                            '((?a . "あ") (?i . "い")
                                          (?u . "う")
                                          (?e . "え")
                                          (?o . "お")))
                           (pair (nskk--pbt-random-choice vowel-map))
                           (char (car pair))
                           (expected (cdr pair)))
                      (nskk-input-routing-test-with-state 'hiragana
                                                          (with-temp-buffer
                                                           (setq last-command-event char)
                                                           (nskk-self-insert 1)
                                                           (should
                                                            (string=
                                                             (buffer-string)
                                                             expected)))))
                    30)

(nskk-property-test nskk-pbt-katakana-mode-converts
                    ((dummy lowercase-char))
                    (let* ((vowel-map
                            '((?a . "ア") (?i . "イ")
                                          (?u . "ウ")
                                          (?e . "エ")
                                          (?o . "オ")))
                           (pair (nskk--pbt-random-choice vowel-map))
                           (char (car pair))
                           (expected (cdr pair)))
                      (nskk-input-routing-test-with-state 'katakana
                                                          (with-temp-buffer
                                                           (setq last-command-event char)
                                                           (nskk-self-insert 1)
                                                           (should
                                                            (string=
                                                             (buffer-string)
                                                             expected)))))
                    30)

;;;;
;;;; Property 4: Mode Routing Invariant
;;;;
(nskk-property-test nskk-pbt-mode-routing-invariant
                    ((char any-char))
                    (let* ((cases
                            '((ascii ?a 2 "aa" "") (latin ?z 1 "z" "")
                                                   (jisx0208-latin ?a
                                                                   2
                                                                   "ａａ"
                                                                   "")
                                                   (abbrev ?k 1 "k" "")
                                                   (hiragana ?a 1 "あ" "")
                                                   (katakana ?a 1 "ア" "")
                                                   (katakana-半角 ?a 1 "ｱ" "")
                                                   (hiragana ?k 1 "" "k")))
                           (case (nth (mod (string-to-char char) (length cases))
                                      cases))
                           (mode (nth 0 case))
                           (input-char (nth 1 case))
                           (repeat (nth 2 case))
                           (expected-buffer (nth 3 case))
                           (expected-romaji (nth 4 case)))
                      (nskk-input-routing-test-with-state mode
                                                          (with-temp-buffer
                                                           (setq last-command-event input-char)
                                                           (nskk-self-insert
                                                            repeat)
                                                           (should
                                                            (equal
                                                             (buffer-string)
                                                             expected-buffer))
                                                           (should
                                                            (equal
                                                             (nskk-state-romaji-buffer)
                                                             expected-romaji))
                                                           (should
                                                            (eq
                                                             (nskk-state-mode
                                                              nskk-current-state)
                                                             mode)))))
                    60)

;;;;
;;;; Regression Test: ASCII Mode Bug Detection
;;;;
(nskk-describe "input routing regression: ASCII mode bug"
               (nskk-it
                "should insert 'a' directly in ASCII mode without Japanese conversion"
                (nskk-input-routing-test-with-state 'ascii
                                                    (with-temp-buffer
                                                     (setq last-command-event ?a)
                                                     (nskk-self-insert 1)
                                                     (should
                                                      (string= (buffer-string)
                                                               "a"))))))

;;;; Enhanced PBT Coverage
;;;;
;;;
;;; Shrinking Property: Input routing is deterministic
;;;
;;; Same character in same mode must always produce the same buffer content.
;;; When a failure is found, the framework attempts to shrink the scenario to
;;; the smallest character/mode pair that still exhibits the failure.
;;;
(nskk-property-test-with-shrinking input-routing-deterministic
  ((scenario input-scenario))
  (let* ((mode (plist-get scenario :mode))
         (key-seq (plist-get scenario :key-sequence))
         (char-str (if key-seq (car key-seq) "a"))
         (single-char-p (and (stringp char-str)
                             (= (length char-str) 1)))
         (char-val (when single-char-p
                     (string-to-char char-str))))
    (if (not single-char-p)
        t  ; Skip non-single-char keys — not testable as direct insert
      (let (result-1 result-2)
        (nskk-input-routing-test-with-state mode
          (with-temp-buffer
            (setq last-command-event char-val)
            (nskk-self-insert 1)
            (setq result-1 (buffer-string))))
        (nskk-input-routing-test-with-state mode
          (with-temp-buffer
            (setq last-command-event char-val)
            (nskk-self-insert 1)
            (setq result-2 (buffer-string))))
        (string= result-1 result-2))))
  60)

(provide 'nskk-input-routing-pbt-test)

;;; nskk-input-routing-pbt-test.el ends here
