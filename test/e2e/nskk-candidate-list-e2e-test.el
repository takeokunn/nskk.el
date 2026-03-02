;;; nskk-candidate-list-e2e-test.el --- E2E tests for candidate list phase  -*- lexical-binding: t; -*-

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

;; E2E tests for the candidate list (候補一覧) phase of NSKK.
;;
;; The candidate list phase (henkan-phase = 'list) is entered after the user
;; presses SPC `nskk-henkan-show-candidates-nth' (default: 5) times during
;; conversion (▼ state).  The first SPC starts conversion and shows candidate[0]
;; inline.  Subsequent SPCs cycle candidates inline one-by-one.  On the Nth
;; press (count >= threshold), `nskk--show-candidate-list-next' is called, which
;; sets henkan-phase to 'list and fires the show-candidates hook.
;;
;; Key behaviors under test:
;;   1. Entering list phase: SPC x5 sets henkan-phase to 'list.
;;   2. Key selection ('a'/'s'/'d'/'f'): commits a specific candidate.
;;   3. 'x' in list phase: shows previous page, henkan-phase stays 'list.
;;   4. C-g in list phase: cancels conversion, restores kana, phase → nil.
;;   5. RET in list phase: commits the current candidate (page-start index).
;;
;; Dict layout for all tests (7 candidates):
;;   ("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事"))
;;
;; SPC press trace (nskk-henkan-show-candidates-nth = 5):
;;   SPC#1 (preedit→converting): nskk-start-conversion sets count=1,
;;          current-index=0, shows "漢字".
;;   SPC#2 (count=2 < 5 → select-next): current-index→1, shows "感じ".
;;   SPC#3 (count=3 < 5 → select-next): current-index→2, shows "幹事".
;;   SPC#4 (count=4 < 5 → select-next): current-index→3, shows "換字".
;;   SPC#5 (count=5 >= 5 → show-list-next): current-index=3 (unchanged at
;;          entry; next-start = current = 3 because list was not yet active),
;;          phase set to 'list.  List page starts at index 3 = "換字".
;;
;; Key mapping in list phase (current-index = 3 after SPC x5):
;;   'a' → absolute = 3 + 0 = 3 → "換字"
;;   's' → absolute = 3 + 1 = 4 → "貫地"
;;   'd' → absolute = 3 + 2 = 5 → "刊事"
;;   'f' → absolute = 3 + 3 = 6 → "肝事"
;;   'j' → absolute = 3 + 4 = 7 → out of range (nil, no commit)
;;
;; nskk-candidate-show-list is mocked as #'ignore in nskk-e2e-with-buffer,
;; but nskk-henkan-select-candidate-by-key-function is set to
;; #'nskk-candidate-list-select-by-key by nskk--enable, so key selection
;; (a/s/d/f/j/k/l) is fully functional in E2E tests.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))

;;; Test dictionary (shared by all sections)

(defconst nskk-e2e--kanji-7cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事")))
  "Seven-candidate dict entry for かんじ, used in candidate-list E2E tests.
Indices 0-6: 漢字 感じ 幹事 換字 貫地 刊事 肝事.")

;;;;
;;;; Section 1: Entering list phase
;;;;

(nskk-describe "entering candidate list phase"
  (nskk-it "sets henkan-phase to list after 5 SPC presses"
    ;; SPC#1 starts conversion (preedit→converting, count=1, shows 漢字).
    ;; SPC#2..#4 cycle inline (count=2,3,4 < 5 → select-next).
    ;; SPC#5 triggers show-list-next (count=5 >= 5) → phase = 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
        (nskk-e2e-type "SPC")   ; SPC#2: select-next
        (nskk-e2e-type "SPC")   ; SPC#3: select-next
        (nskk-e2e-type "SPC")   ; SPC#4: select-next
        (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list "Phase must be 'list after SPC x5"))))

  (nskk-it "sets nskk-henkan--candidate-list-active to non-nil"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-then
        (should nskk-henkan--candidate-list-active))))

  (nskk-it "is still in converting state while in list phase"
    ;; nskk-converting-p returns t for all henkan phases including 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-then
        (nskk-e2e-assert-converting))))

  (nskk-context "before the 5th SPC"
    (nskk-it "is still in active (inline) phase after 4 SPC presses"
      ;; count=4 < 5: inline cycling, not yet list phase.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")  ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")  ; SPC#2: select-next
          (nskk-e2e-type "SPC")  ; SPC#3: select-next
          (nskk-e2e-type "SPC")) ; SPC#4: select-next (count=4 < 5)
        (nskk-then
          (nskk-e2e-assert-henkan-phase 'active "Phase must be 'active before SPC x5")
          (should-not nskk-henkan--candidate-list-active))))))

;;;;
;;;; Section 2: Key selection in list phase
;;;;

(nskk-describe "candidate selection by key in list phase"
  ;; In list phase after SPC x5: current-index = 3 (see commentary above).
  ;; nskk-candidate-list-select-by-key is active via nskk-henkan-select-candidate-by-key-function.
  ;; Pressing a key commits the candidate and ends conversion.

  (nskk-it "pressing 'a' selects candidate at page position 0 (index 3 = 換字)"
    ;; 'a' → pos=0 → absolute = 3 + 0 = 3 → "換字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "a"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "換字" "Key 'a' in list phase must commit 換字 (index 3)"))))

  (nskk-it "pressing 's' selects candidate at page position 1 (index 4 = 貫地)"
    ;; 's' → pos=1 → absolute = 3 + 1 = 4 → "貫地"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "s")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "貫地" "Key 's' in list phase must commit 貫地 (index 4)")))

  (nskk-it "pressing 'd' selects candidate at page position 2 (index 5 = 刊事)"
    ;; 'd' → pos=2 → absolute = 3 + 2 = 5 → "刊事"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "d")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "刊事" "Key 'd' in list phase must commit 刊事 (index 5)")))

  (nskk-it "pressing 'f' selects candidate at page position 3 (index 6 = 肝事)"
    ;; 'f' → pos=3 → absolute = 3 + 3 = 6 → "肝事"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "肝事" "Key 'f' in list phase must commit 肝事 (index 6)")))

  (nskk-it "pressing 'j' does not commit when page position 4 is out of range"
    ;; 'j' → pos=4 → absolute = 3 + 4 = 7 → out of range for 7-candidate list.
    ;; nskk-candidate-list-select-by-key returns nil for out-of-range index,
    ;; so nskk--try-candidate-selection returns nil and 'j' is handled as
    ;; romaji self-insert (じ in hiragana mode), not a commit.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      ;; 'j' is out-of-range; conversion must still be active
      (nskk-e2e-type "j")
      (nskk-e2e-assert-converting)))

  (nskk-it "key selection clears conversion state (phase → nil)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "a")
      (nskk-e2e-assert-henkan-phase nil "After key selection phase must be nil"))))

;;;;
;;;; Section 3: 'x' in list phase (previous page)
;;;;

(nskk-describe "x key in list phase"
  (nskk-it "stays in list phase after x (previous page)"
    ;; x → nskk-previous-candidate → show-list-prev; henkan-phase stays 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "x"))
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list "Phase must remain 'list after x"))))

  (nskk-it "remains in converting state after x"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")
      (nskk-e2e-assert-converting)))

  (nskk-it "key selection after x commits the correct candidate from previous page"
    ;; After SPC x5 → list phase (page start = index 3).
    ;; x → prev page: prev-start = 3 - 7 = -4 → clamped to 0.
    ;;   current-index = 0, page shows candidates 0..6 starting at index 0.
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")
      (nskk-e2e-assert-henkan-phase 'list)
      ;; After x (prev-start = max(0, 3-7) = 0), current-index = 0.
      ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
      (nskk-e2e-type "a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字" "After x then 'a', should commit 漢字 (index 0)"))))

;;;;
;;;; Section 4: C-g in list phase (cancel conversion)
;;;;

(nskk-describe "C-g in list phase"
  (nskk-it "cancels conversion and resets henkan-phase to nil"
    ;; C-g → nskk-handle-cancel → 'cancel-conversion → nskk-cancel-conversion-to-reading.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "C-g"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil "C-g in list phase must reset phase to nil"))))

  (nskk-it "restores kana reading to buffer after cancel"
    ;; nskk-cancel-conversion-to-reading restores bare kana (without ▽/▼)
    ;; for the input "Kanji" → "かんじ".
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      ;; Buffer should contain the bare kana "かんじ" (no ▽ or ▼ marker).
      (nskk-e2e-assert-buffer "かんじ" "C-g must restore kana reading to buffer")))

  (nskk-it "clears nskk-henkan--candidate-list-active after cancel"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk-henkan--candidate-list-active)
      (nskk-e2e-type "C-g")
      (should-not nskk-henkan--candidate-list-active))))

;;;;
;;;; Section 5: RET in list phase (commit current candidate)
;;;;

(nskk-describe "RET in list phase"
  (nskk-it "commits the current candidate (page-start index) without newline"
    ;; RET → nskk-handle-return → 'commit-candidate → nskk-commit-current.
    ;; Current-index after SPC x5 = 3 → "換字".
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "RET"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil "After RET, phase must be nil")
        ;; RET commits current-index=3 → "換字"
        (nskk-e2e-assert-buffer "換字" "RET in list phase must commit current candidate 換字"))))

  (nskk-it "ends conversion state after RET"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "does not insert a newline (buffer contains only the committed candidate)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      ;; No trailing newline: buffer-string is exactly the committed kanji.
      (should-not (string-match-p "\n" (buffer-string))))))

;;;;
;;;; Section 6: Candidate list phase properties
;;;;

(nskk-describe "candidate list phase properties"
  ;; Exhaustive test: each selection key (a/s/d/f) must produce a non-empty
  ;; committed string from the 7-candidate list when there is a valid mapping.
  ;; Keys j/k/l map to out-of-range indices for a page starting at index 3 with
  ;; only 4 remaining candidates (indices 3-6), so they must NOT commit.

  (nskk-property-test-exhaustive candidate-list-valid-keys-commit-and-end-conversion
    '(?a ?s ?d ?f)
    ;; item = one of the valid selection keys that maps to an in-range candidate.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      ;; Dispatch the selection key as a character event
      (nskk-e2e--dispatch-event item)
      ;; After a valid selection key, conversion must have ended
      (and (not (nskk-converting-p))
           ;; Buffer must contain a non-empty string (the committed candidate)
           (not (string-empty-p (buffer-string))))))

  (nskk-property-test-exhaustive candidate-list-out-of-range-keys-keep-converting
    '(?j ?k)
    ;; item = selection keys that map to out-of-range indices for a 7-candidate
    ;; list with page starting at index 3:
    ;;   'j' → pos=4 → absolute=7 (>= 7), 'k' → pos=5 → absolute=8 (>= 7).
    ;; NOTE: '?l' is excluded here because 'l' is bound to nskk-handle-l in
    ;; nskk-mode-map with key-action = kakutei-then-latin in converting state.
    ;; That dedicated handler fires BEFORE any candidate-selection logic,
    ;; so 'l' always commits the current candidate — it does not stay converting.
    ;; nskk-candidate-list-select-by-key returns nil → no commit → still converting.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e--dispatch-event item)
      ;; Out-of-range key: conversion must still be active
      (nskk-converting-p))))

;;;;
;;;; Section 7: DEL in list phase (cancel conversion)
;;;;

(nskk-describe "DEL key in list phase"
  ;; DEL in list phase is bound to `cancel-conversion' in nskk-keymap.el:
  ;;   (backspace converting cancel-conversion)
  ;; This is identical in effect to C-g: it calls nskk-cancel-conversion-to-reading,
  ;; which restores the kana reading, resets henkan-phase to nil, and clears
  ;; nskk-henkan--candidate-list-active.

  (nskk-it "cancels conversion and resets henkan-phase to nil"
    ;; DEL → cancel-conversion → nskk-cancel-conversion-to-reading → phase = nil.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")    ; SPC#1: start-conversion
        (nskk-e2e-type "SPC")    ; SPC#2: select-next
        (nskk-e2e-type "SPC")    ; SPC#3: select-next
        (nskk-e2e-type "SPC")    ; SPC#4: select-next
        (nskk-e2e-type "SPC"))   ; SPC#5: show-list-next → 'list
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "DEL"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil "DEL in list phase must reset phase to nil"))))

  (nskk-it "restores kana reading to buffer (same as C-g)"
    ;; nskk-cancel-conversion-to-reading restores bare kana for "Kanji" → "かんじ".
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      ;; Buffer must contain the bare kana "かんじ" with no ▽/▼ markers.
      (nskk-e2e-assert-buffer "かんじ" "DEL must restore kana reading to buffer")))

  (nskk-it "clears nskk-henkan--candidate-list-active"
    ;; After cancel, the list-active flag must be nil (matching C-g behavior).
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk-henkan--candidate-list-active)
      (nskk-e2e-type "DEL")
      (should-not nskk-henkan--candidate-list-active))))

;;;;
;;;; Section 8: SPC in list phase (next page)
;;;;

;; 11-candidate dict so that SPC#6 advances to page 2 without exhaustion.
;; With per-page = min(7, 7) = 7:
;;   After SPC x5: list phase, current-index = 3.
;;   SPC#6: next-start = 3 + 7 = 10 < 11 → page 2 (index 10), no exhaustion.
(defconst nskk-e2e--kanji-11cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事" "感事" "看事" "官事" "貫字")))
  "Eleven-candidate dict entry for かんじ, used to test next-page without exhaustion.
Indices 0-10: 漢字 感じ 幹事 換字 貫地 刊事 肝事 感事 看事 官事 貫字.")

(nskk-describe "SPC in list phase advances to next page"
  ;; Strategy A (7-cand dict): SPC#6 exhausts all 7 candidates.
  ;; nskk--exhaust-candidates fires, then nskk-start-registration is called.
  ;; read-from-minibuffer is mocked to return "" → registration cancelled.
  ;; Cancel path in nskk--exhaust-candidates wraps to index 0:
  ;;   current-index = 0, henkan-phase = 'list, candidate-list-active = t.

  (nskk-context "Strategy A: 7-candidate dict — SPC#6 exhausts and triggers registration"
    (nskk-it "SPC in list phase triggers registration prompt when candidates exhausted"
      ;; After SPC x5 → list phase (current-index=3).
      ;; SPC#6: next-start = 3 + 7 = 10 >= 7 → nskk--exhaust-candidates called.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")   ; SPC#2: select-next
          (nskk-e2e-type "SPC")   ; SPC#3: select-next
          (nskk-e2e-type "SPC")   ; SPC#4: select-next
          (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-when
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=10 >= 7 → exhaust-candidates
        (nskk-then
          ;; After registration cancel: phase wraps back to 'list at index 0.
          (nskk-e2e-assert-henkan-phase 'list
            "After exhaustion and registration cancel, phase must remain 'list"))))

    (nskk-it "after registration cancel wraps current-index back to 0"
      ;; nskk--exhaust-candidates cancel path:
      ;;   (setf (nskk-state-current-index ...) 0) → index = 0.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → exhaust-candidates → cancel → index=0
        (nskk-e2e-assert-henkan-phase 'list)
        ;; 'a' selects page position 0 from current-index=0 → index 0 = "漢字"
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "After exhaustion cancel wrap, 'a' must commit index 0 = 漢字")))

    (nskk-it "after registration cancel nskk-henkan--candidate-list-active is t"
      ;; Cancel path in nskk--exhaust-candidates sets candidate-list-active = t.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase
        (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → candidate-list-active=t
        (nskk-e2e-assert-henkan-phase 'list)
        (should nskk-henkan--candidate-list-active))))

  ;; Strategy B (11-cand dict): SPC#6 goes to page 2 without exhaustion.
  ;; per-page = min(7, 7) = 7; current-index=3 after SPC#5.
  ;; SPC#6: next-start = 3 + 7 = 10 < 11 → set current-index=10, show page 2.

  (nskk-context "Strategy B: 11-candidate dict — SPC#6 shows next page without exhaustion"
    (nskk-it "SPC in list phase advances to next page when candidates remain"
      ;; SPC#6 with 11 candidates: next-start = 3 + 7 = 10, which is < 11.
      ;; Phase must remain 'list (no exhaustion triggered).
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")   ; SPC#2: select-next
          (nskk-e2e-type "SPC")   ; SPC#3: select-next
          (nskk-e2e-type "SPC")   ; SPC#4: select-next
          (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list, index=3
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-when
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=10 < 11 → page 2
        (nskk-then
          (nskk-e2e-assert-henkan-phase 'list "Phase must remain 'list after next-page SPC")
          (nskk-e2e-assert-converting))))

    (nskk-it "SPC next page stays in converting state"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=3
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=10
        (nskk-e2e-assert-converting)))

    (nskk-it "key selection after next-page SPC commits correct candidate from page 2"
      ;; After SPC#6 with 11 candidates: current-index = 10 (index into candidates).
      ;; 'a' → page position 0 → absolute = 10 + 0 = 10 → "貫字" (index 10).
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=3
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=10
        (nskk-e2e-assert-henkan-phase 'list)
        ;; 'a' → pos=0 → absolute=10 → "貫字"
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "貫字"
          "After next-page SPC, 'a' must commit index 10 = 貫字")))))

;;;;
;;;; Section 9: x at first page (boundary behavior)
;;;;

(nskk-describe "x at first page in list phase"
  ;; After SPC x5: list phase, current-index = 3 (page start = index 3).
  ;; x → nskk--show-candidate-list-prev:
  ;;   prev-start = 3 - 7 = -4 → clamped to 0.
  ;;   current-index becomes 0.
  ;; x again when current-index = 0:
  ;;   prev-start = 0 - 7 = -7 → clamped to 0.
  ;;   current-index stays 0 → same page re-displayed, still in 'list.
  ;; This is NOT a cancel/exit from list phase.

  (nskk-it "x at first page stays in list phase (does not exit or cancel)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1
        (nskk-e2e-type "SPC")   ; SPC#2
        (nskk-e2e-type "SPC")   ; SPC#3
        (nskk-e2e-type "SPC")   ; SPC#4
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "x")    ; prev-start = max(0, 3-7) = 0, index=0
        (nskk-e2e-type "x"))   ; prev-start = max(0, 0-7) = 0, index=0 (re-display)
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list
          "x at page-0 must not exit list phase"))))

  (nskk-it "x at first page still shows converting state"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")    ; x at page-0 (after clamp to 0)
      (nskk-e2e-type "x")    ; x again at page-0
      (nskk-e2e-assert-converting)))

  (nskk-it "x at page-0 then 'a' commits index 0 = 漢字"
    ;; After SPC x5 → list, index=3.
    ;; x → prev-start = max(0, 3-7) = 0, current-index = 0.
    ;; x again → prev-start = max(0, 0-7) = 0, current-index = 0 (unchanged).
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))  ; list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "x")    ; index → 0
        (nskk-e2e-type "x"))   ; index stays 0 (boundary clamp)
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "a"))   ; pos=0 → absolute=0 → "漢字"
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "x at page-0 boundary then 'a' must commit 漢字 (index 0)"))))

  (nskk-it "nskk-henkan--candidate-list-active remains t after x at page-0"
    ;; x in list phase keeps candidate-list-active = t regardless of page boundary.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase
      (should nskk-henkan--candidate-list-active)
      (nskk-e2e-type "x")    ; prev page at page-0
      (nskk-e2e-type "x")    ; again at page-0
      (should nskk-henkan--candidate-list-active))))

;;;;
;;;; Section 10: SPC exhaustion → registration (and cancel wraps back)
;;;;

(nskk-describe "SPC exhaustion triggers registration in list phase"
  ;; With 7-candidate dict: per-page = min(7, 7) = 7.
  ;; After SPC x5: current-index = 3.
  ;; SPC#6 in list phase: next-start = 3 + 7 = 10 >= 7 → exhaustion.
  ;; nskk--exhaust-candidates → nskk-start-registration called.
  ;; read-from-minibuffer mocked → returns "" → registration cancelled.
  ;; Cancel path: current-index = 0, henkan-phase = 'list, candidate-list-active = t.

  (nskk-it "SPC in list phase exhausts candidates and triggers registration"
    ;; After exhaustion with cancelled registration, phase must be 'list (wrapped).
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1
        (nskk-e2e-type "SPC")   ; SPC#2
        (nskk-e2e-type "SPC")   ; SPC#3
        (nskk-e2e-type "SPC")   ; SPC#4
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        ;; SPC#6: next-start=10 >= 7 → exhaust-candidates → registration cancelled.
        (nskk-e2e-type "SPC"))
      (nskk-then
        ;; Registration was cancelled (mock returned ""); wrap to list at index 0.
        (nskk-e2e-assert-henkan-phase 'list
          "After exhaustion and registration cancel, phase must remain 'list"))))

  (nskk-it "after registration cancel still in converting state"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; SPC#5 → list
      (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel
      (nskk-e2e-assert-converting)))

  (nskk-it "after registration cancel index wraps to 0 and 'a' commits 漢字"
    ;; Cancel path: current-index = 0.
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")   ; SPC#5 → list phase, index=3
        (nskk-e2e-type "SPC"))  ; SPC#6 → exhaust → cancel → index=0
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "a"))    ; pos=0 → absolute=0+0=0 → "漢字"
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "After exhaustion cancel, 'a' must commit 漢字 (index 0)"))))

  (nskk-it "nskk-henkan--candidate-list-active is t after registration cancel"
    ;; Cancel path in nskk--exhaust-candidates:
    ;;   (setq nskk-henkan--candidate-list-active t)
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; SPC#5 → list
      (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → active=t
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk-henkan--candidate-list-active)))

  (nskk-it "two successive exhaustion cycles both wrap back to list phase"
    ;; SPC#6 → exhaust → cancel → index=0, list phase.
    ;; SPC#7: now candidate-list-active=t, next-start = 0 + 7 = 7 >= 7 → exhaust again.
    ;; Cancel again → index=0, list phase.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")   ; SPC#5 → list, index=3
        (nskk-e2e-type "SPC")   ; SPC#6 → exhaust → cancel → index=0
        (nskk-e2e-type "SPC"))  ; SPC#7 → exhaust again → cancel → index=0
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list
          "Second consecutive exhaustion must also wrap back to list phase")
        (nskk-e2e-assert-converting)))))

(provide 'nskk-candidate-list-e2e-test)

;;; nskk-candidate-list-e2e-test.el ends here
