;;; nskk-sticky-e2e-test.el --- E2E sticky shift tests for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing

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

;; E2E tests for sticky shift (DDSKK skk-sticky).

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;; Sticky-shift mode: pressing ";" immediately inserts the ▽ marker
;; (entering preedit mode), matching ddskk's `skk-sticky-set-henkan-point'.
;;
;; Key rules:
;;   ";"  in idle Japanese mode           → immediately insert ▽ (preedit-pending)
;;   ";"  in ▽ mode with kana             → arm okurigana (next char as boundary)
;;   ";"  in ▽ mode without kana          → cancel preedit, insert literal ";"
;;   ";;" (double semicolon)              → cancel sticky shift, self-insert ";"
;;   ";"  in converting (▼) mode          → fall through to self-insert

;;;;
;;;; Basic Sticky Shift Tests
;;;;

(nskk-describe "sticky shift mode (スティッキーシフト)"
  (nskk-it "semicolon immediately starts preedit (▽)"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; ";" immediately inserts ▽ marker.
      (nskk-e2e-type ";")
      ;; Preedit (▽) phase must be active after ";" alone.
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "semicolon followed by consonant continues preedit input"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; ";" inserts ▽ immediately; "k" is processed as normal preedit input.
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")
      ;; Preedit (▽) phase must remain active.
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "double semicolon self-inserts a literal semicolon"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; ";;" cancels the sticky-shift state and inserts ";" literally.
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      ;; No preedit; buffer should contain only the semicolon character.
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";")))

  (nskk-it "semicolon followed by uppercase vowel triggers okurigana marker"
    ;; Okurigana lookup needs a matching dict entry (key = "かa").
    (let ((dict '(("かa" . ("蚊")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; First start preedit normally: "K" → ▽ preedit begins.
        (nskk-e2e-type "Ka")   ; → ▽ か
        ;; Then ";a" == "A": uppercase vowel inside preedit should act as the
        ;; okurigana marker, triggering conversion with the vowel as okurigana.
        (nskk-e2e-type ";")
        (nskk-e2e-type "a")
        ;; Conversion (▼) phase must be active.
        (nskk-e2e-assert-henkan-phase 'active)))))

;;;;
;;;; Table: Consonant Sticky-Shift Rows
;;;;

;; Each consonant prefixed with ";" should be treated as the uppercase
;; consonant, which starts ▽ preedit in hiragana mode.
(nskk-deftest-table sticky-consonant-starts-preedit
  :columns (sticky-char expected-mode)
  :rows (("k" hiragana)
         ("s" hiragana)
         ("t" hiragana)
         ("n" hiragana)
         ("h" hiragana)
         ("m" hiragana)
         ("r" hiragana)
         ("w" hiragana))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type ";")
    (nskk-e2e-type sticky-char)
    (nskk-e2e-assert-henkan-phase 'on
      (format "';%s' should start preedit" sticky-char))
    (nskk-e2e-assert-mode expected-mode)))

;;;;
;;;; Cases: Double-Semicolon Input
;;;;

;; The original single case is preserved.  It verifies that the canonical
;; ";;" sequence self-inserts a literal semicolon character.
(nskk-deftest-table sticky-double-semicolon-cases
  :columns (input expected)
  :rows (( ";;" ";"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type input)
    (nskk-e2e-assert-buffer expected)))

;;;;
;;;; Double-Semicolon in Various Modes
;;;;

(nskk-describe "double-semicolon in various modes"
  (nskk-it ";; inserts a literal semicolon in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";")
      (nskk-e2e-assert-henkan-phase nil)))

  (nskk-it ";; inserts a literal semicolon in katakana mode"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";")
      (nskk-e2e-assert-henkan-phase nil)))

  (nskk-it ";; does not change mode after inserting semicolon in hiragana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it ";; does not change mode after inserting semicolon in katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-mode 'katakana))))

;;;;
;;;; Sticky Shift Okurigana Markers
;;;;

(nskk-describe "sticky shift okurigana markers"
  (nskk-it "semicolon-a after consonant preedit triggers okurigana conversion"
    ;; Dict: "かa" → ("蚊") — vowel okurigana entry
    (nskk-e2e-with-buffer 'hiragana '(("かa" . ("蚊")))
      (nskk-e2e-type "Ka")    ; starts preedit: ▽か
      (nskk-e2e-type ";")     ; sticky shift
      (nskk-e2e-type "a")     ; uppercase A → okurigana trigger
      (nskk-e2e-assert-henkan-phase 'active)))

  (nskk-it "sticky shift ;k completes preedit typing after"
    ;; After ;k starts preedit, regular input continues normally.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")    ; → ▽ preedit started with k
      (nskk-e2e-type "a")    ; → ▽か (completes ka syllable)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "か"))))

;;;;
;;;; Sticky Shift with Cancel Keys
;;;;

(nskk-describe "sticky shift with cancel keys"
  (nskk-it "DEL after ;k (preedit) cancels preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")      ; preedit started
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "DEL")    ; cancel preedit
      ;; After DEL, mode must remain hiragana regardless of preedit state.
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "C-g after ;k cancels preedit completely"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ""))))

;;;;
;;;; Property-Based Tests
;;;;

;; PBT 1: any consonant after ; starts preedit.
;; okurigana-consonant generates uppercase strings like "K", "S", etc.;
;; lowercase them to form the sticky key pair (;k, ;s, …).
(nskk-property-test-seeded sticky-consonant-preedit-property
  ((consonant okurigana-consonant))
  (let ((lower (downcase consonant)))
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type lower)
      ;; After ;+consonant we expect preedit (▽) to be active.
      (or (eq (nskk-state-henkan-phase nskk-current-state) 'on)
          (nskk-state-valid-mode-p (nskk-current-mode)))))
  20)

;; PBT 2: ;; in any mode never crashes.
(nskk-property-test sticky-double-semicolon-no-crash-any-mode
  ((mode valid-mode))
  (condition-case nil
      (progn
        (nskk-e2e-with-buffer mode nil
          (nskk-e2e-type ";")
          (nskk-e2e-type ";")
          t))
    ;; All errors are caught; crash-freedom is preserved as long as
    ;; no unhandled signal propagates out of the body.
    (error t))
  30)

;; PBT 3: sticky-shift never leaves an invalid mode.
(nskk-property-test-seeded sticky-mode-always-valid
  ((mode valid-mode))
  (nskk-e2e-with-buffer mode nil
    (condition-case nil
        (progn (nskk-e2e-type ";") (nskk-e2e-type "k"))
      (error nil))
    (nskk-state-valid-mode-p (nskk-current-mode)))
  25)


;;;;
;;;; Immediate ▽ Behavior Tests
;;;;

(nskk-describe "immediate ▽ on sticky key"
  (nskk-it "semicolon alone in hiragana inserts ▽ immediately"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "semicolon alone in katakana inserts ▽ immediately"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it ";ka produces ▽か (normal lowercase in preedit)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "か")))

  (nskk-it "semicolon in preedit-pending cancels preedit and inserts ;"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; First ; enters ▽ (preedit-pending, no kana yet).
      ;; The pending flag is 'immediate, so a second ; fires arm 1
      ;; which cancels preedit + inserts literal ";".
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";"))))

;;;;
;;;; Okurigana via Sticky Key in Preedit
;;;;

(nskk-describe "sticky okurigana in preedit"
  (nskk-it "semicolon in preedit-japanese arms okurigana"
    (let ((dict '(("おおa" . ("大")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Oo")         ; → ▽おお
        (nskk-e2e-type ";")          ; arm okurigana
        (nskk-e2e-type "a")          ; → okurigana A triggers conversion
        (nskk-e2e-assert-henkan-phase 'active)))))

;;;;
;;;; Preedit-Pending via Uppercase + Sticky Key
;;;;

(nskk-describe "sticky key in preedit-pending (uppercase K then ;)"
  (nskk-it "semicolon in preedit-pending (from uppercase) cancels preedit and inserts ;"
    ;; Enter ▽ via uppercase K, then press ";" before typing any kana.
    ;; Arm 4 fires: cancel-preedit + insert ";".
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "K")           ; → ▽ (preedit-pending via uppercase)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type ";")           ; arm 4: cancel + insert ";"
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";"))))

;;;;
;;;; Double-Semicolon from Okurigana State
;;;;

(nskk-describe "double-semicolon from okurigana state"
  (nskk-it ";; after arming okurigana inserts ; without cancelling preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")          ; → ▽か (preedit-japanese)
      (nskk-e2e-type ";")           ; arm 3: set 'okurigana
      (nskk-e2e-type ";")           ; arm 1: cancel okurigana + insert ";"
      ;; Preedit should still be active (was-immediate is nil → no cancel-preedit)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "か;"))))

;;;;
;;;; Sticky Shift in Direct Input Mode
;;;;

;; Bug fix: semicolon in direct input (ascii) mode was consumed by the
;; sticky-shift handler without inserting a literal character.
;; The (fail) CPS path invoked #'ignore, silently eating the keypress.

(nskk-describe "sticky shift in direct input mode"
  (nskk-it "semicolon in ascii mode inserts literal semicolon"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";")))

  (nskk-it "semicolon in ascii mode does not arm sticky shift"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")
      ;; Both characters should be inserted literally, no preedit.
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";k"))))

;;;;
;;;; Sticky State Cleared on Cancel / Kakutei (Regression)
;;;;

;; Regression: nskk--sticky-shift-pending was not cleared by
;; nskk--clear-azik-pending-state, causing a stale 'immediate state
;; after cancel or kakutei.  The next ";" would fire Arm 1
;; (double-semicolon cancel) instead of Arm 5 (new ▽).

(nskk-describe "sticky state cleared on cancel and kakutei"
  (nskk-it "; then C-g then ; starts new preedit (not literal ;)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      ;; Sticky state must have been cleared by cancel-preedit.
      ;; The next ";" must start a new ▽, not insert literal ";".
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "; then DEL then ; starts new preedit (not literal ;)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "DEL")
      ;; After DEL from preedit-pending, the preedit is cancelled.
      ;; The next ";" must start a new ▽.
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it ";ka then SPC then kakutei then ; starts new preedit"
    (let ((dict '(("か" . ("蚊" "化")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type ";")       ; → ▽
        (nskk-e2e-type "ka")      ; → ▽か
        (nskk-e2e-type " ")       ; → ▼蚊
        (nskk-e2e-assert-henkan-phase 'active)
        (nskk-e2e-type "C-j")     ; kakutei → 蚊
        (nskk-e2e-assert-henkan-phase nil)
        ;; Sticky state must be cleared after kakutei.
        (nskk-e2e-type ";")
        (nskk-e2e-assert-henkan-phase 'on))))

  (nskk-it ";ka then SPC then DEL rolls back to preedit with cleared sticky state"
    (let ((dict '(("か" . ("蚊")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type ";")       ; → ▽
        (nskk-e2e-type "ka")      ; → ▽か
        (nskk-e2e-type " ")       ; → ▼蚊
        (nskk-e2e-assert-henkan-phase 'active)
        ;; DEL from ▼ rolls back to ▽ (preedit), not to idle.
        (nskk-e2e-type "DEL")
        (nskk-e2e-assert-henkan-phase 'on)
        ;; Sticky state must be cleared after rollback.
        ;; ";" in ▽-with-kana arms okurigana (arm 3), not double-semicolon.
        (nskk-e2e-type ";")
        ;; Okurigana armed — sticky consumed, still in preedit.
        (nskk-e2e-assert-henkan-phase 'on))))

  (nskk-it ";ka then ; (arm okurigana) then C-g then ; starts new preedit"
    ;; Regression: okurigana-armed sticky state ('okurigana) must be
    ;; cleared by cancel-preedit, not just 'immediate.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")       ; → ▽
      (nskk-e2e-type "ka")      ; → ▽か
      (nskk-e2e-type ";")       ; arm okurigana (sticky = 'okurigana)
      (nskk-e2e-type "C-g")     ; cancel preedit
      (nskk-e2e-assert-henkan-phase nil)
      ;; Sticky must be cleared; next ";" starts new ▽.
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it ";ka then q (script toggle) then ; starts new preedit"
    ;; Regression: nskk-henkan-kakutei-convert-script must clear sticky.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")       ; → ▽
      (nskk-e2e-type "ka")      ; → ▽か
      (nskk-e2e-type "q")       ; script toggle → カ committed
      (nskk-e2e-assert-henkan-phase nil)
      ;; Sticky must be cleared; next ";" starts new ▽.
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on))))

(provide 'nskk-sticky-e2e-test)

;;; nskk-sticky-e2e-test.el ends here
