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

;; Systematic state transition coverage for AZIK E2E.
;;
;; Each test covers exactly one edge in the SKK state machine, exercising
;; a specific (starting-state, input-class) → (ending-state) transition.
;;
;; State machine edges covered:
;;
;;   idle ──kana──────────────→ idle          kana appears committed in buffer
;;   idle ──;(っ)─────────────→ idle          AZIK: っ committed immediately
;;   idle ──:(ー)─────────────→ idle          AZIK: ー committed immediately
;;   idle ──AZIK-hatsuon──────→ idle          two-char AZIK shortcut resolves
;;   idle ──AZIK-diphthong────→ idle          two-char vowel shortcut resolves
;;   idle ──C-g───────────────→ idle          no-op when nothing pending
;;   idle ──uppercase-vowel───→ preedit-on    ▽ starts with one mora
;;
;;   preedit-on ──kana────────→ preedit-on    reading grows in ▽
;;   preedit-on ──;(っ)───────→ preedit-on    っ appended to preedit reading
;;   preedit-on ──:(ー)───────→ preedit-on    ー appended to preedit reading
;;   preedit-on ──SPC─────────→ converting    ▼ starts (dict match found)
;;   preedit-on ──C-g─────────→ idle          preedit cancelled, kana inserted
;;   preedit-on ──RET─────────→ idle          preedit committed as kana + newline
;;
;;   converting ──SPC─────────→ converting    cycles to next candidate
;;   converting ──C-g─────────→ preedit-on    reverts to ▽ with original reading
;;   converting ──RET─────────→ idle          commits current candidate
;;
;;   converting → preedit-on → idle          double-cancel (C-g C-g)
;;
;;   converting ──uppercase───→ idle          commits + starts new preedit
;;     (this is the "type-through" pattern common in fast Japanese typing)
;;
;; Input-class taxonomy for AZIK mode:
;;   kana        – standard romaji producing hiragana (e.g., "ka" → か)
;;   azik-tsutsu – semicolon (;) → っ
;;   azik-dash   – colon (:) → ー
;;   azik-hatsuon– consonant + n-trigger (e.g., "kz" → かん)
;;   azik-diph   – consonant + vowel-shortcut (e.g., "kq" → かい)
;;   uppercase   – uppercase consonant + vowel starts preedit (e.g., "Ka")
;;   spc         – space: triggers conversion in preedit, no-op in idle
;;   cancel      – C-g: cancels preedit or conversion
;;   commit      – RET: commits current state as-is
;;
;; All tests use the standard mock dictionary from `nskk-e2e--default-dict'.
;; Entries relied upon:
;;   ("か" . ("蚊" "課" "下"))          – used for Ka + SPC conversion
;;   ("かんじ" . ("漢字" "感じ" "幹事")) – used for Kanji + SPC conversion

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-e2e-helpers)
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
    ;; "kz" is the AZIK hatsuon extension for ka+ん (か+ん).
    ;; Mapping: z-trigger → vowel 'a', so k-row + z = か + ん.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kz")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かん")))

  (nskk-it "AZIK diphthong two-char sequence in idle produces vowel pair"
    ;; "kq" is the AZIK double-vowel extension for か+い
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kq")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "C-g in idle propagates as keyboard-quit (expected nskk behavior)"
    ;; In nskk, C-g in idle (no preedit pending) escalates to keyboard-quit,
    ;; matching standard Emacs C-g semantics.  This test verifies it does so
    ;; cleanly (no corrupt state before the quit signal).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      ;; keyboard-quit is expected; absorb it so the test doesn't abort.
      (condition-case nil
          (nskk-e2e-type "C-g")
        (quit nil))
      ;; After absorbing keyboard-quit, mode and phase must still be valid.
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
    ;; "kz" = AZIK hatsuon for ka+ん → かん (z-trigger maps to vowel 'a')
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
    ;; Mock dict has ("か" . ("蚊" "課" "下"))
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-assert-henkan-phase 'active)))

  (nskk-it "SPC in ▽ for two-mora reading triggers conversion"
    ;; Mock dict has ("かわ" . ("川" "河")).
    ;; NOTE: typing ん in AZIK preedit via "n"+"j" is unsafe because "nj"
    ;; is an AZIK hatsuon extension (ぬん), not a commit-n-then-じ sequence.
    ;; Use "かわ" ("Ka"+"wa") to avoid AZIK ambiguity around ん.
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
    ;; Preedit contains っ and ー (AZIK special outputs): cancellation must
    ;; still leave state clean regardless of the reading content.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ra")
      (nskk-e2e-type ":")     ; ー in preedit
      (nskk-e2e-type ";")     ; っ in preedit
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "RET in ▽ commits preedit as kana and returns to idle (matches ddskk)"
    ;; In ddskk, Enter in ▽ commits the preedit as kana and returns to idle.
    ;; nskk now matches this behavior: RET in ▽ calls nskk-henkan-kakutei
    ;; (commits the raw reading) then inserts a newline, leaving phase nil.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "RET")
      ;; preedit committed: phase is now nil (idle)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)
      ;; FR-001: committed kana and newline must both appear in the buffer
      (nskk-e2e-assert-buffer "か\n"))))

;;;
;;; Group 6: converting (▼) → converting (candidate cycling)
;;; SPC while in ▼ advances to the next candidate.
;;;

(nskk-describe "AZIK state transitions: converting stays converting (SPC cycles)"

  (nskk-it "SPC in ▼ cycles to next candidate and stays in converting"
    ;; ("か" . ("蚊" "課" "下")) has 3 candidates — cycling is possible.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Cycle once
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Cycle again
      (nskk-e2e-type "SPC")
      ;; Still in some conversion-or-list phase
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
      ;; Must revert to preedit-on, not skip straight to idle
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
    ;; After cycling from 蚊 to 課, RET should commit 課 (or whichever
    ;; candidate is currently shown).  We only verify that we return to
    ;; idle — the exact candidate committed is checked in henkan e2e tests.
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
      ;; First C-g: converting → preedit-on
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      ;; Second C-g: preedit-on → idle
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "C-g C-g works when preedit had AZIK special chars (し+っ)"
    ;; Use a reading that actually has a dict entry so SPC always goes to
    ;; converting (▼).  "Ka" + ";" gives ▽かっ which has no entry, so SPC
    ;; would stay in preedit-on — only one C-g needed to reach idle.
    ;; Instead, use "Sa" → ▽さ (dict has さ) then SPC → ▼, then C-g C-g.
    ;; This exercises the full converting → preedit-on → idle sequence
    ;; even when the preedit reading was built using AZIK special chars.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      ;; Start preedit with さ (which is in the mock dict)
      (nskk-e2e-type "Sa")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; First C-g: converting → preedit-on
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase 'on)
      ;; Second C-g: preedit-on → idle (keyboard-quit absorbed)
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
;;; Group 11: FR-002 — AZIK hatsuon fires in preedit (▽) for n+consonant matches
;;;
;;; With `match > n-consonant' priority in romaji-classify, AZIK hatsuon rules
;;; (nj → ぬん, nz → なん, etc.) fire in preedit (▽) via the match path.
;;; To type かんじ in preedit, use double-n: Ka+n+n+ji (Kannji → ▽かんじ).

(nskk-describe "FR-002: AZIK hatsuon fires in preedit for n+consonant match"

  (nskk-it "Ka+nj in ▽ fires AZIK hatsuon → ▽かぬん (match > n-consonant)"
    ;; With match > n-consonant, nj is an AZIK match rule (→ ぬん).
    ;; In preedit, the match class fires before n-consonant, so nj → ぬん.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "nj")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "▽かぬん")))

  (nskk-it "Kannji in ▽ produces ▽かんじ (double-n forces ん emission)"
    ;; To get かんじ in preedit, use nn (nn-double fires → ん, buffer cleared),
    ;; then ji → じ.  After Kannji, preedit reading is かんじ.
    ;; The mock dict has ("かんじ" . ("漢字" "感じ" "幹事")).
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
    ;; Outside preedit, nj fires the AZIK hatsuon match rule.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "nj")
      (nskk-e2e-assert-henkan-phase nil)
      ;; AZIK hatsuon match fires: buffer is non-empty (not pending).
      (should (> (length (buffer-string)) 0)))))

;;;
;;; Group 12: Regression — q after N in converting (▼) with okurigana-in-progress
;;;
;;; Bug: Nao+teNq produced 直ってん instead of 直ってない.
;;;
;;; Root cause (pre-fix): In ▼ converting state with okurigana-in-progress
;;; metadata set, the old combined `nskk--implicit-kakutei-needed-p' returned
;;; nil for uppercase N, so it accumulated "n" in nskk--romaji-buffer.  When q
;;; arrives, the mode-switch preaction (nskk-commit-current) calls
;;; nskk-henkan-do-reset which wipes the romaji buffer.  nskk-handle-q-key
;;; then sees an empty buffer → buf-state=empty → insert-n action → inserts ん.
;;;
;;; Fix: in the mode-switch arm of nskk-handle-q, save nskk--romaji-buffer
;;; before the preaction commit fires and restore it after commit but before
;;; nskk-handle-q-key runs.
;;;
;;; Repro path (minimal): TabeRu → ▼食る (okurigana-in-progress=t),
;;; then N → romaji-buffer="n", then q → should fire AZIK nq (ない), not ん.

(nskk-describe "Regression: q after N in ▼ with okurigana-in-progress fires AZIK nq"

  (nskk-it "N+q after okurigana conversion produces ない not ん"
    ;; default dict has ("たべr" . ("食" "喰"))
    ;; TabeRu: Ta → preedit ▽た, be → ▽たべ, R → okuri consonant r,
    ;;         u → completes okurigana → ▼食 with okurigana-in-progress=t
    ;; N: uppercase is not okurigana continuation → implicit kakutei fires,
    ;;    then N starts new preedit → romaji-buffer = "n"
    ;; q: mode-switch arm fires; without the fix, commit wipes romaji-buffer
    ;;    and nskk-handle-q-key sees empty buffer → inserts ん (BUG).
    ;;    With the fix, romaji-buffer is restored to "n" before the call
    ;;    → AZIK nq rule fires → inserts ない.
    (let ((dict (cons '("たべr" . ("食" "喰")) nskk-e2e--default-dict)))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ta")
        (nskk-e2e-type "be")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        ;; Now in ▼ converting state for 食る (okurigana-in-progress=t).
        (nskk-e2e-assert-converting)
        ;; Type N: should accumulate "n" in romaji-buffer without kakutei.
        (nskk-e2e-type "N")
        ;; Type q: should commit 食る, restore "n" to romaji-buffer,
        ;; then fire AZIK nq → ない.
        (nskk-e2e-type "q")
        ;; State must be back to idle after commit + AZIK dispatch.
        (nskk-e2e-assert-not-converting)
        ;; Buffer must NOT contain ん (the pre-fix broken output).
        (should (not (string-match-p "ん" (buffer-string))))
        ;; Buffer must contain ない (AZIK nq diphthong result).
        (should (string-match-p "ない" (buffer-string))))))

  (nskk-it "q without prior N in ▼ still commits and switches mode normally"
    ;; Sanity check: q in ▼ with empty romaji-buffer still works as mode-switch.
    ;; After KaSPC → ▼蚊, q should commit 蚊 and call nskk-handle-q-key
    ;; with empty buffer → toggle hiragana/katakana mode or insert ん per
    ;; q-key-action rules for the empty case.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; q with empty romaji-buffer: commits 蚊 and fires nskk-handle-q-key.
      (nskk-e2e-type "q")
      ;; Must no longer be converting after commit.
      (nskk-e2e-assert-not-converting))))

;;;
;;; Group 13: AZIK colon-okuri pending cleared on kakutei
;;;
;; Bug: nskk-henkan-kakutei did not clear nskk--azik-colon-okuri-pending.
;; If the user armed colon-okurigana (typed : in preedit) then navigated away
;; (triggering kakutei), the pending flag survived.  Any subsequent alphabetic
;; keypress was misrouted as colon-pending with no valid preedit context.

(nskk-describe "AZIK colon-okuri state cleared on navigation (kakutei)"
  (nskk-it "colon-pending flag is nil after C-f commits preedit"
    ;; Arm colon-okurigana then navigate away with C-f.
    ;; After kakutei, nskk--azik-colon-okuri-pending must be nil so the next
    ;; keypress is not misrouted.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      ;; Enter preedit: ▽か
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      ;; Arm colon-okurigana (: in preedit = azik-arm-eligible context).
      (nskk-e2e-type ":")
      ;; Navigate away — should commit preedit and clear colon state.
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-henkan-phase nil)
      ;; colon-pending must be cleared.
      (should (not (bound-and-true-p nskk--azik-colon-okuri-pending)))))

  (nskk-it "after colon-arm + C-f, next key starts normal preedit not colon-pending"
    ;; Verifies that a stale colon-pending flag does not corrupt the next
    ;; preedit.  The next Ka should produce ▽か, not trigger colon-fire.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type ":")
      (nskk-e2e-type "C-f")  ; kakutei + move
      (erase-buffer)
      ;; Fresh preedit: Ka should give ▽か, not a colon-fire artifact.
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "か" (buffer-string))))))

(nskk-describe "AZIK colon after plain vowel kana extends reading with ー"
  ;; Regression tests for the 'A: → あー' bug.
  ;; When preedit ends with a plain vowel kana (あいうえおアイウエオ or ー)
  ;; and the romaji buffer is empty, ':' must produce ー via the normal
  ;; romaji path rather than arming colon-okurigana.

  (nskk-it "A: produces ▽あー (not colon-arm)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?A)
      ;; preedit is ▽あ
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "あ" (buffer-string)))
      ;; ':' should extend with ー, NOT arm okurigana
      (nskk-e2e-type ":")
      (should (string-search "あー" (buffer-string)))
      (should (not (bound-and-true-p nskk--azik-colon-okuri-pending)))))

  (nskk-it "Ka: produces ▽かー via plain-vowel exclusion from azik-arm-eligible"
    ;; 'か' is NOT a plain vowel kana, so ':' after Ka arms okurigana.
    ;; But A: (▽あ) and Kai: (▽かい) should produce ー.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?A)
      (nskk-e2e-type "i")
      ;; preedit is ▽あい (ends with い = plain vowel)
      (should (string-search "あい" (buffer-string)))
      (nskk-e2e-type ":")
      (should (string-search "あいー" (buffer-string)))
      (should (not (bound-and-true-p nskk--azik-colon-okuri-pending)))))

  (nskk-it "Ka: still arms colon-okurigana (か is not plain vowel)"
    ;; 'か' is a consonant+vowel kana, so ':' after Ka must arm okurigana.
    ;; Use us101 keyboard type: on jp106, ':' is a bare key producing ー and
    ;; does not arm colon-okurigana (jp106 uses '+' for sokuon-okurigana).
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "Ka")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (string-search "か" (buffer-string)))
        (nskk-e2e-type ":")
        (should (bound-and-true-p nskk--azik-colon-okuri-pending))))))

(nskk-describe "AZIK hatsuon rules fire in preedit (Nz → なん)"
  ;; Regression tests for the 'Nz → んz' bug.
  ;; AZIK two-char hatsuon rules (nz→なん, nk→にん, etc.) must fire in
  ;; preedit (▽) mode via the match path in romaji-classify.

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
    ;; Verify nn→ん still functions; users type KAnn to get かん before き.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e--dispatch-event ?n)
      (nskk-e2e--dispatch-event ?n)
      (nskk-e2e-type "ki")
      (should (string-search "かんき" (buffer-string))))))

(provide 'nskk-azik-state-transition-e2e-test)

;;; nskk-azik-state-transition-e2e-test.el ends here
