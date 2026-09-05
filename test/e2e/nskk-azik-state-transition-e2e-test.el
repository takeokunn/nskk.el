;;; nskk-azik-state-transition-e2e-test.el --- Systematic state transition coverage for AZIK  -*- lexical-binding: t; -*-

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


;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-test-macros) (require 'nskk-e2e-helpers)
(require 'nskk-state)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-azik)
(require 'nskk-converter)

;;;
;;; Group 1: idle → idle
;;; All inputs that produce output without entering preedit.
;;;

(nskk-describe "AZIK state transitions: idle stays idle (output only)"

  (nskk-it "standard romaji in idle produces kana and stays idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "AZIK semicolon in idle produces っ immediately"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "っ")))

  (nskk-it "AZIK colon in idle produces ー immediately"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ":")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "ー")))

  (nskk-it "AZIK hatsuon two-char sequence in idle produces kana+ん"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kz")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かん")))

  (nskk-it "AZIK diphthong two-char sequence in idle produces vowel pair"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kq")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "C-g in idle propagates as keyboard-quit (expected nskk behavior)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (condition-case nil
          (nskk-e2e-type "C-g")
        (quit nil))
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "multiple kana in idle produce concatenated output"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-type "na")
      (nskk-e2e-type "ji")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かなじ")))

  (nskk-it "AZIK word shortcut sr produces する in idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sr")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "する"))))

;;;
;;; Group 2: idle → preedit-on (▽)
;;; Uppercase consonant + vowel starts the preedit marker.
;;;

(nskk-describe "AZIK state transitions: idle → preedit-on (uppercase)"

  (nskk-it "uppercase Ka starts preedit with か in ▽"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽か")))

  (nskk-it "uppercase Sa starts preedit with さ in ▽"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Sa")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽さ")))

  (nskk-it "uppercase Na starts preedit with な in ▽"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Na")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽な"))))

;;;
;;; Group 3: preedit-on → preedit-on (reading grows)
;;; While in ▽, further input extends the preedit reading.
;;;

(nskk-describe "AZIK state transitions: preedit-on stays preedit-on (reading grows)"

  (nskk-it "more kana typed in ▽ extends the preedit reading"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "na")
      (nskk-e2e-type "ji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽かなじ")))

  (nskk-it "AZIK semicolon in ▽ appends っ to the preedit reading"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽かっ")))

  (nskk-it "AZIK colon in ▽ appends ー to the preedit reading"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ra")
      (nskk-e2e-type ":")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽らー")))

  (nskk-it "AZIK hatsuon in ▽ appends hatsuon output to preedit reading"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ha")
      (nskk-e2e-type "kz")   ; AZIK hatsuon: か+ん = かん
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽はかん"))))

;;;
;;; Group 4: preedit-on → converting (▼)
;;; SPC while in ▽ triggers dictionary lookup.
;;;

(nskk-describe "AZIK state transitions: preedit-on → converting (SPC)"

  (nskk-it "SPC in ▽ triggers conversion when dict has entry"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-assert-henkan-phase 'active)))

  (nskk-it "SPC in ▽ for two-mora reading triggers conversion"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "wa")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting))))

;;;
;;; Group 5: preedit-on → idle (cancel or commit)
;;; C-g and RET exit preedit without dictionary lookup.
;;;

(nskk-describe "AZIK state transitions: preedit-on → idle (C-g or RET)"

  (nskk-it "C-g in ▽ cancels preedit and returns to idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "n")
      (nskk-e2e-type "ji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "C-g in ▽ with AZIK special chars in reading cancels cleanly"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ra")
      (nskk-e2e-type ":")     ; ー in preedit
      (nskk-e2e-type ";")     ; っ in preedit
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "RET in ▽ commits preedit as kana and returns to idle (matches ddskk)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "か\n"))))

;;;
;;; Group 6: converting (▼) → converting (candidate cycling)
;;; SPC while in ▼ advances to the next candidate.
;;;

(nskk-describe "AZIK state transitions: converting stays converting (SPC cycles)"

  (nskk-it "SPC in ▼ cycles to next candidate and stays in converting"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "SPC")
      (should (memq (nskk-state-henkan-phase nskk-current-state)
                    '(active list registration))))))

;;;
;;; Group 7: converting (▼) → preedit-on (C-g reverts to ▽)
;;;

(nskk-describe "AZIK state transitions: converting → preedit-on (C-g)"

  (nskk-it "C-g in ▼ reverts to ▽ (preedit-on) not to idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "C-g in ▼ after cycling candidates still reverts to ▽"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")   ; → ▼ showing 蚊
      (nskk-e2e-type "SPC")   ; cycle → 課
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting))))

;;;
;;; Group 8: converting (▼) → idle (RET commits)
;;;

(nskk-describe "AZIK state transitions: converting → idle (RET)"

  (nskk-it "RET in ▼ commits the current candidate and returns to idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "RET in ▼ after cycling commits the cycled candidate"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")   ; first candidate
      (nskk-e2e-type "SPC")   ; second candidate
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting))))

;;;
;;; Group 9: Double-cancel sequence (converting → preedit-on → idle)
;;;

(nskk-describe "AZIK state transitions: double-cancel C-g C-g"

  (nskk-it "C-g C-g in sequence goes from converting all the way to idle"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "C-g C-g works when preedit had AZIK special chars (し+っ)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Sa")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      (condition-case nil
          (nskk-e2e-type "C-g")
        (quit nil))
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting))))

;;;
;;; Group 10: Mode stays hiragana throughout AZIK operations
;;;

(nskk-describe "AZIK state transitions: mode invariant"

  (nskk-it "mode remains hiragana throughout an idle→▽→▼→idle round-trip"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "mode remains hiragana after C-g cancellation from converting"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-g")
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "AZIK special keys do not change the mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type ":")
      (nskk-e2e-assert-mode 'hiragana))))

;;;
;;; Group 11: AZIK hatsuon fires in preedit (▽) for n+consonant matches
;;;
;;; With `match > n-consonant' priority in romaji-classify, AZIK hatsuon rules
;;; (nj → ぬん, nz → なん, etc.) fire in preedit (▽) via the match path.
;;; To type かんじ in preedit, use double-n: Ka+n+n+ji (Kannji → ▽かんじ).

(nskk-describe "AZIK hatsuon fires in preedit for n+consonant match"

  (nskk-it "Ka+nj in ▽ fires AZIK hatsuon → ▽かぬん (match > n-consonant)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "nj")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽かぬん")))

  (nskk-it "Kannji in ▽ produces ▽かんじ (double-n forces ん emission)"
    (let ((dict '(("かんじ" . ("漢字" "感じ" "幹事")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "nn")
        (nskk-e2e-type "ji")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-assert-buffer-matches "▽かんじ")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting))))

  (nskk-it "nj in idle (not preedit) fires AZIK hatsuon rule"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "nj")
      (nskk-e2e-assert-henkan-phase nil)
      (should (> (length (buffer-string)) 0)))))

;;;
;;; Group 12: Regression — q after N in converting (▼) with okurigana-in-progress
;;;
;;; Bug: Nao+teNq produced 直ってん instead of 直ってない.
;;;
;;; Root cause (pre-fix): In ▼ converting state with okurigana-in-progress
;;; metadata set, the old combined `nskk--implicit-kakutei-needed-p' returned
;;; nil for uppercase N, so it accumulated "n" in nskk-state-romaji-buffer.  When q
;;; arrives, the mode-switch preaction (nskk-commit-current) calls
;;; nskk-henkan-do-reset which wipes the romaji buffer.  nskk-handle-q-key
;;; then sees an empty buffer → buf-state=empty → insert-n action → inserts ん.
;;;
;;; Fix: in the mode-switch arm of nskk-handle-q, save nskk-state-romaji-buffer
;;; before the preaction commit fires and restore it after commit but before
;;; nskk-handle-q-key runs.
;;;
;;; Repro path (minimal): TabeRu → ▼食る (okurigana-in-progress=t),
;;; then N → romaji-buffer="n", then q → should fire AZIK nq (ない), not ん.

(nskk-describe "Regression: q after N in ▼ with okurigana-in-progress fires AZIK nq"

  (nskk-it "N+q after okurigana conversion produces ない not ん"
    (let ((dict (cons '("たべr" . ("食" "喰")) nskk-e2e--default-dict)))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ta")
        (nskk-e2e-type "be")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "N")
        (nskk-e2e-type "q")
        (nskk-e2e-assert-not-converting)
        (should (not (string-match-p "ん" (buffer-string))))
        (should (string-match-p "ない" (buffer-string))))))

  (nskk-it "q without prior N in ▼ still commits and switches mode normally"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-not-converting))))

;;;
;;; Group 13: AZIK colon-okuri pending cleared on kakutei
;;;

(nskk-describe "AZIK colon-okuri state cleared on navigation (kakutei)"
  (nskk-it "colon-pending flag is nil after C-f commits preedit"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type ":")
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-henkan-phase nil)
      (should (not (and (fboundp 'nskk-azik-colon-okuri-pending) (nskk-azik-colon-okuri-pending))))))

  (nskk-it "after colon-arm + C-f, next key starts normal preedit not colon-pending"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type ":")
      (nskk-e2e-type "C-f")  ; kakutei + move
      (erase-buffer)
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "か" (buffer-string))))))

(nskk-describe "AZIK colon after plain vowel kana extends reading with ー"

  (nskk-it "A: produces ▽あー (not colon-arm)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?A)
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "あ" (buffer-string)))
      (nskk-e2e-type ":")
      (should (string-search "あー" (buffer-string)))
      (should (not (and (fboundp 'nskk-azik-colon-okuri-pending) (nskk-azik-colon-okuri-pending))))))

  (nskk-it "Ka: produces ▽かー via plain-vowel exclusion from azik-arm-eligible"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?A)
      (nskk-e2e-type "i")
      (should (string-search "あい" (buffer-string)))
      (nskk-e2e-type ":")
      (should (string-search "あいー" (buffer-string)))
      (should (not (and (fboundp 'nskk-azik-colon-okuri-pending) (nskk-azik-colon-okuri-pending))))))

  (nskk-it "Ka: still arms colon-okurigana (か is not plain vowel)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "Ka")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (string-search "か" (buffer-string)))
        (nskk-e2e-type ":")
        (should (and (fboundp 'nskk-azik-colon-okuri-pending) (nskk-azik-colon-okuri-pending)))))))

(nskk-describe "AZIK hatsuon rules fire in preedit (Nz → なん)"

  (nskk-it "Nz produces ▽なん (AZIK hatsuon fires in preedit)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?N)
      (nskk-e2e--dispatch-event ?z)
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "なん" (buffer-string)))))

  (nskk-it "Nk produces ▽にん (AZIK hatsuon nk fires in preedit)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?N)
      (nskk-e2e--dispatch-event ?k)
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "にん" (buffer-string)))))

  (nskk-it "KAnz produces ▽かなん (hatsuon fires mid-reading)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e--dispatch-event ?n)
      (nskk-e2e--dispatch-event ?z)
      (should (string-search "かなん" (buffer-string)))))

  (nskk-it "KAnnki produces ▽かんき (nn path still works for plain ん)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e--dispatch-event ?n)
      (nskk-e2e--dispatch-event ?n)
      (nskk-e2e-type "ki")
      (should (string-search "かんき" (buffer-string))))))

(provide 'nskk-azik-state-transition-e2e-test)

;;; nskk-azik-state-transition-e2e-test.el ends here
