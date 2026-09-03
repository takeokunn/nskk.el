;;; nskk-azik-e2e-test.el --- E2E tests for AZIK extended romaji input  -*- lexical-binding: t; -*-

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
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-input)
(require 'nskk-azik)
(require 'nskk-converter)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; AZIK Buffer Helper Macro
;;;;

;;;;
;;;; Section 1: AZIK Semicolon as っ
;;;;

(nskk-describe "AZIK semicolon produces っ"

  (nskk-it "semicolon in AZIK hiragana mode inserts っ"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "っ")))

  (nskk-it "n then semicolon produces んっ in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "n")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "んっ")))

  (nskk-it "semicolon in AZIK katakana mode inserts ッ"
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "ッ")))

  (nskk-it "multiple semicolons produce multiple っ in sequence"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "っっ")))

  (nskk-it "semicolon followed by ka produces っか in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";ka")
      (nskk-e2e-assert-buffer "っか")))

  (nskk-it "semicolon self-inserts after switching from hiragana to latin in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'latin)
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";"))))

;;;;
;;;; Section 2: AZIK Colon as Long Vowel Mark (ー)
;;;;

(nskk-describe "AZIK colon produces ー"

  (nskk-it "colon in AZIK hiragana mode inserts ー"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ":")
      (nskk-e2e-assert-buffer "ー")))

  (nskk-it "ka followed by colon produces かー in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka:")
      (nskk-e2e-assert-buffer "かー")))

  (nskk-it "colon in AZIK katakana mode inserts ー"
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-type ":")
      (nskk-e2e-assert-buffer "ー")))

  (nskk-it "ko followed by colon produces こー"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ko:")
      (nskk-e2e-assert-buffer "こー")))

  (nskk-it "colon after semicolon produces っー"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";:")
      (nskk-e2e-assert-buffer "っー"))))

;;;;
;;;; Section 3: AZIK Hatsuon Extensions via Key Dispatch
;;;;

(nskk-describe "AZIK hatsuon extensions via key dispatch"

  (nskk-it "kz produces かん in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kz")
      (nskk-e2e-assert-buffer "かん")))

  (nskk-it "sz produces さん in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sz")
      (nskk-e2e-assert-buffer "さん")))

  (nskk-it "tk produces ちん in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "tk")
      (nskk-e2e-assert-buffer "ちん")))

  (nskk-it "hl produces ほん in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "hl")
      (nskk-e2e-assert-buffer "ほん")))

  (nskk-it "hatsuon sequence: kz + to produces かんと"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kzto")
      (nskk-e2e-assert-buffer "かんと"))))

;;;;
;;;; Section 4: AZIK Double Vowel Extensions via Key Dispatch
;;;;

(nskk-describe "AZIK double vowel extensions via key dispatch"

  (nskk-it "kq produces かい in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kq")
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "kp produces こう in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kp")
      (nskk-e2e-assert-buffer "こう")))

  (nskk-it "tp produces とう in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "tp")
      (nskk-e2e-assert-buffer "とう")))

  (nskk-it "kw produces けい in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kw")
      (nskk-e2e-assert-buffer "けい")))

  (nskk-it "double vowel sequence: kp + ka produces こうか"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kpka")
      (nskk-e2e-assert-buffer "こうか")))

  (nskk-it "ChO preserves deferred ちゅう and appends お in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ChO")
      (nskk-e2e-assert-buffer "▽ちゅうお")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ChOu produces ちゅうおう in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ChOu")
      (nskk-e2e-assert-buffer "▽ちゅうおう")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ShO preserves deferred すう and appends お in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ShO")
      (nskk-e2e-assert-buffer "▽すうお")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ShOu produces すうおう in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ShOu")
      (nskk-e2e-assert-buffer "▽すうおう")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ThO preserves deferred つう and appends お in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ThO")
      (nskk-e2e-assert-buffer "▽つうお")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ThOu produces つうおう in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ThOu")
      (nskk-e2e-assert-buffer "▽つうおう")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ChA continues deferred ちゅう and appends あ"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ChA")
      (nskk-e2e-assert-buffer "▽ちゅうあ")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ShI continues deferred すう and appends い"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ShI")
      (nskk-e2e-assert-buffer "▽すうい")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ThU continues deferred つう and appends う"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ThU")
      (nskk-e2e-assert-buffer "▽つうう")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "ChE continues deferred ちゅう and appends え"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ChE")
      (nskk-e2e-assert-buffer "▽ちゅうえ")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "Cho with lowercase o continues deferred ちゅう in conversion mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Cho")
      (nskk-e2e-assert-buffer "▽ちゅうお")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "Sha with lowercase a continues deferred すう in conversion mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Sha")
      (nskk-e2e-assert-buffer "▽すうあ")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))))

  (nskk-it "cho all-lowercase in hiragana mode still gives ちょ via DV correction"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "cho")
      (nskk-e2e-assert-buffer "ちょ")))

  (nskk-it "sha all-lowercase in hiragana mode still gives しゃ via DV correction"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sha")
      (nskk-e2e-assert-buffer "しゃ"))))

;;;;
;;;; Section 5: AZIK Q-Key Behavior — Context-Aware Mode
;;;;

(nskk-describe "AZIK q context-aware mode (default)"

  (nskk-it "q with empty romaji buffer inserts ん in hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ん")))

  (nskk-it "q with empty romaji buffer inserts ン in katakana mode"
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-assert-buffer "ン")))

  (nskk-it "q completing an AZIK double-vowel rule produces the kana (not ん)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "k")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "q with pending n romaji produces ない (AZIK nq rule)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "n")
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ない")))

  (nskk-it "fq produces ふぁい in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "f")
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ふぁい")))

  (nskk-it "jq produces じゃい in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "j")
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "じゃい")))

  (nskk-it "vq produces ゔぁい in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "v")
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ゔぁい")))

  (nskk-it "q with pending non-AZIK romaji produces ん"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-state-set-romaji-buffer "l")
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ん"))))

;;;;
;;;; Section 6: AZIK Toggle Key Behavior (@ and [)
;;;;

(nskk-describe "AZIK toggle key behavior"

  (nskk-it "@ key toggles hiragana to katakana (jp106 keyboard)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "@ key toggles katakana to hiragana (jp106 keyboard)"
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "[ key toggles hiragana to katakana (us101 keyboard)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "[ key toggles katakana to hiragana (us101 keyboard)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'katakana nil
        (nskk-e2e-assert-mode 'katakana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "@ toggle on jp106 clears pending romaji and toggles mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "k")  ; pending romaji
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "bracket toggle on us101 clears pending romaji and toggles mode"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "k")  ; pending romaji
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'katakana)
        (nskk-e2e-assert-buffer "")))))

;;;;
;;;; Section 6b: AZIK Toggle Key During Henkan-Active (Issue #34)
;;;;

(nskk-describe "AZIK toggle key commits conversion during henkan-active"

  (nskk-it "@ key during ▼ commits conversion and toggles to katakana (jp106)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Yama")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'active)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "山")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "[ key during ▼ commits conversion and toggles to katakana (us101)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "Yama")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-henkan-phase 'active)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "山")
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "@ key during ▽ converts script and commits (existing behavior preserved)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Yama")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "ヤマ")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "[ key in idle hiragana toggles to katakana (existing behavior preserved)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "[ key in ascii mode self-inserts (existing behavior preserved)"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer nil nil
        (nskk-e2e-type "[")
        (nskk-e2e-assert-buffer "["))))

  (nskk-it "@ key during preedit-pending clears marker and toggles mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "K")               ; uppercase K starts henkan (▽)
      (nskk-e2e-assert-henkan-phase 'on) ; marker set, no kana yet
      (nskk-e2e-type "@")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-mode 'katakana))))

;;;;
;;;; Section 7: AZIK Standard Romaji Compatibility in E2E Buffer
;;;;

(nskk-describe "AZIK standard romaji compatibility via key dispatch"

  (nskk-it "ka still produces か in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "sh produces すう in AZIK mode (vowel-shadow deferred)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sh")
      (nskk-e2e-assert-buffer "すう")))

  (nskk-it "sha still produces しゃ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sha")
      (nskk-e2e-assert-buffer "しゃ")))

  (nskk-it "tsu still produces つ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "tsu")
      (nskk-e2e-assert-buffer "つ")))

  (nskk-it "a i u e o produce あいうえお in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "aiueo")
      (nskk-e2e-assert-buffer "あいうえお")))

  (nskk-it "sokuon (doubled consonant) still produces っ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kka")
      (nskk-e2e-assert-buffer "っか"))))

;;;;
;;;; Section 8: AZIK Youon Compatibility via Key Dispatch
;;;;

(nskk-describe "AZIK youon (g substitutes for y) via key dispatch"

  (nskk-it "kga produces きゃ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kga")
      (nskk-e2e-assert-buffer "きゃ")))

  (nskk-it "kgu produces きゅ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kgu")
      (nskk-e2e-assert-buffer "きゅ")))

  (nskk-it "hga produces ひゃ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "hga")
      (nskk-e2e-assert-buffer "ひゃ")))

  (nskk-it "kgp produces きょう via youon + diphthong in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kgp")
      (nskk-e2e-assert-buffer "きょう")))

  (nskk-it "nga produces にゃ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "nga")
      (nskk-e2e-assert-buffer "にゃ")))

  (nskk-it "ngu produces にゅ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngu")
      (nskk-e2e-assert-buffer "にゅ")))

  (nskk-it "ngo produces にょ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngo")
      (nskk-e2e-assert-buffer "にょ")))

  (nskk-it "ngz produces にゃん via ng youon + hatsuon in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngz")
      (nskk-e2e-assert-buffer "にゃん")))

  (nskk-it "ngq produces にゃい via ng youon + double vowel in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngq")
      (nskk-e2e-assert-buffer "にゃい"))))

;;;;
;;;; Section 9: AZIK Word Shortcuts via Key Dispatch
;;;;

(nskk-describe "AZIK word shortcuts via key dispatch"

  (nskk-it "sr produces する in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sr")
      (nskk-e2e-assert-buffer "する")))

  (nskk-it "ms produces ます in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ms")
      (nskk-e2e-assert-buffer "ます")))

  (nskk-it "mn produces もの in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "mn")
      (nskk-e2e-assert-buffer "もの")))

  (nskk-it "kt produces こと in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kt")
      (nskk-e2e-assert-buffer "こと")))

  (nskk-it "ss produces せい in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ss")
      (nskk-e2e-assert-buffer "せい")))

  (nskk-it "ssa produces っさ via azik-deferred retroactive correction"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ssa")
      (nskk-e2e-assert-buffer "っさ"))))

;;;;
;;;; Section 10: AZIK Same-Finger Alternatives via Key Dispatch
;;;;

(nskk-describe "AZIK same-finger alternatives via key dispatch"

  (nskk-it "kf produces き in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kf")
      (nskk-e2e-assert-buffer "き")))

  (nskk-it "rf produces る in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "rf")
      (nskk-e2e-assert-buffer "る")))

  (nskk-it "yf produces ゆ in AZIK mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "yf")
      (nskk-e2e-assert-buffer "ゆ"))))

;;;;
;;;; Section 11: AZIK Mixed Sequence Integration
;;;;

(nskk-describe "AZIK mixed sequence integration"

  (nskk-it "kztp produces かんとう (hatsuon + diphthong sequence)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kztp")
      (nskk-e2e-assert-buffer "かんとう")))

  (nskk-it "semicolon then ka then colon produces っかー"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";ka:")
      (nskk-e2e-assert-buffer "っかー")))

  (nskk-it "szpo produces さんぽ (hatsuon + standard romaji)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "szpo")
      (nskk-e2e-assert-buffer "さんぽ")))

  (nskk-it "kgpto produces きょうと via youon + diphthong + standard"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kgpto")
      (nskk-e2e-assert-buffer "きょうと")))

  (nskk-it "AZIK input in preedit and commit with C-j works correctly"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "か"))))

;;;;
;;;; Property-Based Tests: AZIK Hatsuon Known Pairs
;;;;

(nskk-deftest-table azik-hatsuon-pairs
  :columns (input expected)
  :rows (("kz" "かん")
         ("sz" "さん")
         ("tz" "たん"))
  :body
  (nskk-e2e-with-azik-buffer 'hiragana nil
    (nskk-e2e-type input)
    (nskk-e2e-assert-buffer expected)))

;;;;
;;;; Property-Based Tests: AZIK Double-Vowel Rules Table
;;;;

(nskk-deftest-table azik-double-vowel-rules
  :columns (pattern expected)
  :rows (("aa" "ああ")
         ("ii" "いい")
         ("uu" "うう"))
  :body
  (nskk-e2e-with-azik-buffer 'hiragana nil
    (nskk-e2e-type pattern)
    (nskk-e2e-assert-buffer expected)))

;;;;
;;;; Property-Based Tests: AZIK Any Rule Does Not Crash
;;;;

(nskk-property-test azik-e2e-any-rule-does-not-crash
    ((rule azik-rule))
  (progn
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (condition-case err
          (nskk-e2e-type rule)
        (error (ert-fail (format "AZIK rule %S raised error: %s"
                                 rule (error-message-string err))))))
    t)
  30)

;;;;
;;;; Property-Based Tests: AZIK Consistent Romaji Dispatch
;;;;

(nskk-describe "AZIK property: consistent romaji dispatch"

  (nskk-it "basic romaji sequences produce non-empty buffer in hiragana"
    (dotimes (_ 20)
      (nskk-for-all ((r romaji-basic))
        (nskk-e2e-with-azik-buffer 'hiragana nil
          (nskk-e2e-type r)
          (nskk-e2e-type "C-j")
          (should (not (string-empty-p (buffer-string)))))))))

;;;
;;; AZIK @ Key in ▽ Preedit — Script Conversion (DDSKK-compatible)
;;;

(nskk-describe "AZIK @ key in ▽ preedit: script conversion"
  (nskk-it "hiragana preedit + @ → katakana committed, mode stays hiragana"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kana")
      (nskk-e2e-assert-henkan-phase 'on "should be in ▽ preedit after Kana")
      (nskk-e2e-type "@")
      (nskk-e2e-assert-henkan-phase nil "henkan-phase must clear after @")
      (nskk-e2e-assert-mode 'hiragana "mode must remain hiragana (no toggle)")
      (nskk-e2e-assert-buffer "カナ")))

  (nskk-it "idle hiragana + @ → still toggles to katakana (existing behaviour unchanged)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-assert-henkan-phase nil "idle: no preedit")
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'katakana "idle @ must still toggle mode"))))

;;;;
;;;; Section 12: AZIK q Key in ▽ Preedit
;;;;

(nskk-describe "AZIK q key in ▽ preedit: romaji rules take priority, empty → convert-script"

  (nskk-it "Zdtq: ▽ mode stays active after q fires tq→たい rule"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Zdtq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Zdtq")
      (nskk-e2e-assert-mode 'hiragana "mode must not change after q in AZIK ▽ preedit")))

  (nskk-it "Katq: q fires tq→たい in ▽ preedit, stays in ▽"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Katq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Katq")
      (nskk-e2e-assert-mode 'hiragana "mode must not change after tq in AZIK ▽ preedit")))

  (nskk-it "Kaq: standalone q in AZIK ▽ preedit inserts ん (not katakana conversion)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kaq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Kaq")
      (nskk-e2e-assert-buffer "▽かん" "Kaq must produce かん (q=ん in AZIK)")))

  (nskk-it "Sq: q fires sq→さい as first char after henkan start (preedit-pending state)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Sq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Sq")
      (nskk-e2e-assert-buffer "▽さい" "Sq must produce さい via AZIK diphthong rule")))

  (nskk-it "Tq: q fires tq→たい as first char after henkan start"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Tq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Tq")
      (nskk-e2e-assert-buffer "▽たい" "Tq must produce たい via AZIK diphthong rule")))

  (nskk-it "Kq: q fires kq→かい as first char after henkan start"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Kq")
      (nskk-e2e-assert-buffer "▽かい" "Kq must produce かい via AZIK diphthong rule")))

  (nskk-it "A:q → ▽あーん (AZIK colon ー then q inserts ん)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "A:q")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after A:q")
      (nskk-e2e-assert-buffer "▽あーん" "A:q must produce あーん (q=ん in AZIK)")))

  (nskk-it "Dezqq: double-vowel then standalone q inserts ん in ▽ preedit"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Dezqq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Dezqq")
      (nskk-e2e-assert-buffer "▽でざいん" "Dezqq must produce でざいん (second q=ん)"))))

;;;;
;;;; Section 13: AZIK Okurigana with AZIK Kana Shortcuts — Double-* Regression
;;;;

(nskk-describe "AZIK okurigana with AZIK kana shortcuts: double-* regression"

  (nskk-it "XhSS triggers conversion (no spurious double * marker)"
    (let ((dict '(("しゅうs" . ("修正")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "XhSS")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "修正"))))

  (nskk-it "XhSS commits to 修正せい after C-j"
    (let ((dict '(("しゅうs" . ("修正")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "XhSS")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "修正せい"))))

  (nskk-it "TukaTTe triggers conversion showing 使 (no spurious double * marker)"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "TukaTTe")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "使"))))

  (nskk-it "TukaTTe commits to 使って after C-j"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "TukaTTe")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "使って"))))

  (nskk-it "TukaT;te triggers conversion then implicit kakutei on t (AZIK ; sokuon in okurigana)"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type "T")
        (nskk-e2e-type ";")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "te")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "使って"))))

  (nskk-it "TukaT;te produces 使って without explicit C-j"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type "T")
        (nskk-e2e-type ";")
        (nskk-e2e-type "te")
        (nskk-e2e-assert-buffer "使って"))))

  (nskk-it "Tuka:te triggers conversion showing 使 on US101 (AZIK Shift+; sokuon okurigana)"
    (let ((dict '(("つかt" . ("使")))))
      (let ((nskk-azik-keyboard-type 'us101))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Tuka")
          (nskk-e2e-type ":")
          (nskk-e2e-type "t")
          (nskk-e2e-type "e")
          (nskk-e2e-assert-converting)
          (nskk-e2e-assert-overlay-shows "使")))))

  (nskk-it "Tuka:te commits to 使って after C-j on US101"
    (let ((dict '(("つかt" . ("使")))))
      (let ((nskk-azik-keyboard-type 'us101))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Tuka")
          (nskk-e2e-type ":")
          (nskk-e2e-type "t")
          (nskk-e2e-type "e")
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "使って"))))))

;;;;
;;;; Section 14: Okurigana Conversion in AZIK Mode
;;;;

;;; 14.1 Basic consonant okurigana

(nskk-describe "AZIK okurigana: basic consonant okurigana"

  (nskk-it "OkuRu shows first candidate 送 in overlay"
    (let ((dict '(("おくr" . ("送" "贈" "遅")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "送"))))

  (nskk-it "OkuRu + SPC cycles to second candidate 贈"
    (let ((dict '(("おくr" . ("送" "贈" "遅")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-overlay-shows "送")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "贈"))))

  (nskk-it "OkuRu + SPC SPC cycles to third candidate 遅"
    (let ((dict '(("おくr" . ("送" "贈" "遅")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-overlay-shows "送")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "贈")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "遅"))))

  (nskk-it "KaKu shows first candidate 書 in overlay"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "TaBeRu shows first candidate 食 in overlay"
    (let ((dict '(("たべr" . ("食" "喰")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tabe")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "食")))))

;;; 14.2 Commit and buffer content

(nskk-describe "AZIK okurigana: commit and buffer content"

  (nskk-it "OkuRu + C-j commits 送る to buffer"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "送る")
        (nskk-e2e-assert-not-converting))))

  (nskk-it "KaKu + SPC + C-j commits second candidate 掛く to buffer"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "掛く")))))

;;; 14.3 Vowel okurigana

(nskk-describe "AZIK okurigana: vowel okurigana"

  (nskk-it "OkuRI triggers vowel okurigana with overlay showing 送"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "送")))))

;;; 14.4 AZIK-specific combos with okurigana

(nskk-describe "AZIK okurigana: AZIK-specific combos"

  (nskk-it "AZIK hatsuon kz + okurigana Ku: KzKu shows 換"
    (let ((dict '(("かんk" . ("換")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "z")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "換"))))

  (nskk-it "AZIK double-vowel kp + okurigana Ku: KpKu shows 耕"
    (let ((dict '(("こうk" . ("耕")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "p")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "耕"))))

  (nskk-it "AZIK same-finger hf + okurigana Ku: HfKu shows 吹"
    (let ((dict '(("ふk" . ("吹")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "H")
        (nskk-e2e-type "f")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "吹"))))

  (nskk-it "AZIK youon kga + okurigana Ru: KgaRu shows 嫌"
    (let ((dict '(("きゃr" . ("嫌")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "g")
        (nskk-e2e-type "a")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "嫌")))))

;;; 14.5 SPC during partial consonant okurigana

(nskk-describe "AZIK okurigana: SPC during partial consonant okurigana"

  (nskk-it "OkuR + SPC triggers okurigana conversion showing 送"
    (let ((dict '(("おくr" . ("送" "贈")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "送"))))

  (nskk-it "OkuR + SPC SPC cycles to second candidate 贈"
    (let ((dict '(("おくr" . ("送" "贈")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "送")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "贈")))))

;;; 14.6 KaKi multi-candidate cycling regression guard

(nskk-describe "AZIK okurigana: KaKi multi-candidate SPC cycling regression guard"

  (nskk-it "KaKi + SPC cycles to second candidate 掛 (different vowel okurigana)"
    (let ((dict '(("かk" . ("書" "掛" "欠")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "i")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "掛")))))

;;;;
;;;; Section 15: AZIK NN Okurigana — YoNN → 読ん Regression Guard
;;;;

;;; 15.1 Basic YoNN conversion

(nskk-describe "AZIK okurigana: YoNN triggers ん okurigana conversion"

  (nskk-it "YoNN shows first candidate 読 in overlay"
    (let ((dict '(("よn" . ("読" "呼")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "N")
        (nskk-e2e-type "N")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "読"))))

  (nskk-it "YoNN + C-j commits 読ん to buffer"
    (let ((dict '(("よn" . ("読")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "N")
        (nskk-e2e-type "N")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "読ん"))))

  (nskk-it "YoNN + SPC cycles to second candidate 呼"
    (let ((dict '(("よn" . ("読" "呼")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "N")
        (nskk-e2e-type "N")
        (nskk-e2e-assert-overlay-shows "読")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "呼")))))

;;; 15.2 YoNN regression guard: no double * marker

(nskk-describe "AZIK okurigana: YoNN does not produce double okurigana marker"

  (nskk-it "YoNN (empty dict) does not produce よ*ん* with two * markers"
    (let ((dict '(("dummy" . ("dummy")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "N")
        (nskk-e2e-type "N")
        (let ((buf (buffer-string)))
          (should-not (string-match-p "\\*.*\\*" buf)))))))

;;;;
;;;; Section 16: JP106 Keyboard Colon-Okurigana via + Key
;;;;

(nskk-describe "AZIK JP106 + key as colon-okurigana trigger"
  (nskk-it "Nao+ta converts to 治った on JP106 keyboard"
    (let ((dict '(("なおt" . ("治")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Nao")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "ta")
          (nskk-e2e-assert-converting)
          (nskk-e2e-assert-overlay-shows "治")
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "治った")))))

  (nskk-it "Tuka+te converts to 使って on JP106 keyboard"
    (let ((dict '(("つかt" . ("使")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Tuka")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "te")
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "使って")))))

  (nskk-it "+ does not trigger colon-okurigana on US101 keyboard"
    (let ((dict '(("なおt" . ("治")))))
      (let ((nskk-azik-keyboard-type 'us101))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Nao")
          (nskk-e2e--dispatch-event ?+)
          (should-not (nskk-azik-colon-okuri-pending)))))))

(nskk-describe "AZIK semicolon respects sticky shift"
  (nskk-it "sticky-shift state survives AZIK style on semicolon"
    (nskk-e2e-with-buffer 'hiragana nil
      (let ((nskk-converter-romaji-style 'standard))
        (nskk-e2e-type ";"))
      (let ((nskk-converter-romaji-style 'azik))
        (nskk-e2e-type ";"))
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";"))))

;;;; 
;;;; Section 17: JP106 + Key Immediate Sokuon Okurigana
;;;;

(nskk-describe "AZIK JP106 + key immediate sokuon okurigana"
  (nskk-it "Oku+ enters conversion state on JP106 keyboard"
    (let ((dict '(("おくt" . ("送")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Oku")
          (nskk-e2e--dispatch-event ?+)
          (should (string-match-p "▼" (buffer-string)))))))

  (nskk-it "Oku+ C-j commits to 送っ on JP106 keyboard"
    (let ((dict '(("おくt" . ("送")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Oku")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "送っ")))))

  (nskk-it "Tuka+ C-j commits to 使っ on JP106 keyboard"
    (let ((dict '(("つかt" . ("使")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Tuka")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "使っ")))))

  (nskk-it "Tuka+ C-j te produces 使って on JP106 keyboard"
    (let ((dict '(("つかt" . ("使")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Tuka")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "C-j")
          (nskk-e2e-type "te")
          (nskk-e2e-assert-buffer "使って")))))

  (nskk-it "+ in idle hiragana produces っ on JP106 keyboard"
    (let ((nskk-azik-keyboard-type 'jp106))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e--dispatch-event ?+)
        (nskk-e2e-assert-buffer "っ"))))

  (nskk-it "++ produces っっ on JP106 keyboard"
    (let ((nskk-azik-keyboard-type 'jp106))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e--dispatch-event ?+)
        (nskk-e2e--dispatch-event ?+)
        (nskk-e2e-assert-buffer "っっ"))))

  (nskk-it "+ on US101 keyboard does not produce っ"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e--dispatch-event ?+)
        (should-not (string-match-p "っ" (buffer-string))))))

  (nskk-it "colon in JP106 preedit produces ー (not colon-okurigana)"
    (let ((nskk-azik-keyboard-type 'jp106))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "Ka")
        (nskk-e2e--dispatch-event ?:)
        (should (string-match-p "ー" (buffer-string)))
        (should-not (nskk-azik-colon-okuri-pending)))))

  (nskk-it "Ka:soru converts to カーソル on JP106 keyboard"
    (let ((dict '(("かーそる" . ("カーソル")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Ka")
          (nskk-e2e--dispatch-event ?:)
          (nskk-e2e-type "soru")
          (nskk-e2e-type " ")
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "カーソル")))))

  (nskk-it "Oku+ SPC cycles to next candidate on JP106 keyboard"
    (let ((dict '(("おくt" . ("送" "奥")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Oku")
          (nskk-e2e--dispatch-event ?+)
          (should (string-match-p "▼" (buffer-string)))
          (nskk-e2e-type "SPC")
          (should (string-match-p "▼" (buffer-string)))))))

  (nskk-it "Oku+ with empty dict enters registration dialog on JP106 keyboard"
    (let ((dict '(("あ" . ("亜")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Oku")
          (nskk-e2e--dispatch-event ?+)
          (let ((buf (buffer-string)))
            (should (or (string-match-p "\\[辞書登録\\]" buf)
                        (string-match-p "▽" buf)
                        (string-match-p "おく" buf))))))))

  (nskk-it "+ with empty preedit does not error on JP106 keyboard"
    (let ((nskk-azik-keyboard-type 'jp106))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "Q")
        (nskk-e2e--dispatch-event ?+)
        (should-not (string-match-p "error" (downcase (buffer-string)))))))

  (nskk-it "Okona+te clears okurigana-in-progress so m triggers implicit kakutei"
    (let ((dict '(("おこなt" . ("行")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Okona")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-assert-converting)
          (nskk-e2e-type "te")
          (should (not (nskk-state-get-metadata nskk-current-state 'okurigana-in-progress)))
          (should (not (and (fboundp 'nskk-azik-sokuon-okuri-kana-pending) (nskk-azik-sokuon-okuri-kana-pending))))))))

  (nskk-it "C-g after Okona+ clears nskk--azik-sokuon-okuri-kana-pending"
    (let ((dict '(("おこなt" . ("行")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Okona")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "C-g")
          (should (not (and (fboundp 'nskk-azik-sokuon-okuri-kana-pending) (nskk-azik-sokuon-okuri-kana-pending))))))))

  (nskk-it "Okona+temoraitq produces 行ってもらいたい (full implicit-kakutei flow)"
    (let ((dict '(("おこなt" . ("行")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Okona")
          (nskk-e2e--dispatch-event ?+)
          (nskk-e2e-type "te")
          (nskk-e2e-type "moraitq")
          (nskk-e2e-assert-not-converting)
          (nskk-e2e-assert-buffer "行ってもらいたい"))))))

;;;;
;;;; Section 18: DA/DV state cleared by cancel/rollback paths (FR-001 regression)
;;;;

(nskk-describe "§18: nskk--deferred-azik/vowel-shadow-state cleared by cancel-preedit and rollback"

  (nskk-it "T-01: nskk--deferred-azik-state is nil after C-g cancels preedit"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kakk")
      (nskk-e2e-type "C-g")
      (should (not (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state))))
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")))

  (nskk-it "T-02: nskk--deferred-vowel-shadow-state is nil after C-g cancels preedit"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kash")
      (nskk-e2e-type "C-g")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")))

  (nskk-it "T-03: nskk--deferred-azik-state is nil after rollback-conversion"
    (let ((dict '(("かきん" . ("過去問")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Kakk")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-g")
        (should (not (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state)))))))

  (nskk-it "T-04: nskk--deferred-vowel-shadow-state is nil after rollback-conversion"
    (let ((dict '(("かすう" . ("加数")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Kash")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-g")
        (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))
        (nskk-e2e-type "a")
        (nskk-e2e-assert-buffer "▽かすうあ")))))

;;;;
;;;; Section 19: Output correctness PBT — sokuon via DA correction (P-04)
;;;;

(nskk-describe "§19: PBT — doubled consonant + vowel produces っ + kana via DA correction"


  (nskk-deftest-table azik-sokuon-da-correction
    :columns (input expected)
    :rows    (("kka" "っか") ("kki" "っき") ("kku" "っく") ("kke" "っけ") ("kko" "っこ")
              ("ssa" "っさ") ("ssi" "っし") ("ssu" "っす") ("sse" "っせ") ("sso" "っそ")
              ("tta" "った") ("tte" "って") ("tto" "っと")
              ("hha" "っは") ("hhi" "っひ") ("hhu" "っふ") ("hhe" "っへ") ("hho" "っほ")
              ("mma" "っま") ("mmi" "っみ") ("mmu" "っむ") ("mme" "っめ") ("mmo" "っも"))
    :body
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type input)
      (nskk-e2e-assert-buffer expected))))

;;;;
;;;; Section 20: Backspace with AZIK Deferred State in Preedit
;;;;

(nskk-describe "§20: DEL clears AZIK deferred state in preedit (backspace-in-preedit bug)"

  (nskk-it "T-01: DEL rolls back tentative きん from DA (kk in preedit)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "k")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kk': should be in ▽ preedit")
      (should (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state)))
      (nskk-e2e-type "DEL")
      (should (not (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state))))
      (nskk-e2e-assert-henkan-phase nil "After DEL of DA: preedit cancelled (no content left)")
      (nskk-e2e-assert-buffer "" "After DEL of DA: buffer empty")))

  (nskk-it "T-02: DEL rolls back tentative すう from DV (sh in preedit)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "S")
      (nskk-e2e-type "h")
      (nskk-e2e-assert-henkan-phase 'on "After 'Sh': should be in ▽ preedit")
      (should (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state)))
      (nskk-e2e-type "DEL")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))
      (nskk-e2e-assert-henkan-phase nil "After DEL of DV: preedit cancelled (no content left)")
      (nskk-e2e-assert-buffer "" "After DEL of DV: buffer empty")))

  (nskk-it "T-03: DEL with DA preserves prior kana in preedit (kakk)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "a")
      (nskk-e2e-type "k")
      (nskk-e2e-type "k")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kakk': should be in ▽ preedit")
      (should (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state)))
      (nskk-e2e-type "DEL")
      (should (not (and (fboundp 'nskk-deferred-azik-state) (nskk-deferred-azik-state))))
      (nskk-e2e-assert-henkan-phase 'on "After DEL of DA: preedit survives with prior kana")
      (nskk-e2e-assert-buffer "▽か" "After DEL of DA: tentative きん removed, か remains")))

  (nskk-it "T-04: DEL with DV preserves prior kana in preedit (kash)"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "a")
      (nskk-e2e-type "s")
      (nskk-e2e-type "h")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kash': should be in ▽ preedit")
      (should (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state)))
      (nskk-e2e-type "DEL")
      (should (not (and (fboundp 'nskk-deferred-vowel-shadow-state) (nskk-deferred-vowel-shadow-state))))
      (nskk-e2e-assert-henkan-phase 'on "After DEL of DV: preedit survives with prior kana")
      (nskk-e2e-assert-buffer "▽か" "After DEL of DV: tentative すう removed, か remains"))))

;;;;
;;;; Section 21: AZIK Custom Conversion Table — E2E Input Pipeline
;;;;

(nskk-describe "AZIK custom conversion table E2E"
  (nskk-it "user override beats built-in AZIK rule through the full input pipeline"
    (let ((nskk-azik-conversion-table '(("kz" "かすたむ"))))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "k")
        (nskk-e2e-type "z")
        (nskk-e2e-assert-buffer "かすたむ"))))

  (nskk-it "new romaji sequence added via custom table produces kana in buffer"
    (let ((nskk-azik-conversion-table '(("wv" "わかす"))))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "w")
        (nskk-e2e-type "v")
        (nskk-e2e-assert-buffer "わかす"))))

  (nskk-it "qz custom rule initializes through the canonical AZIK bridge"
    (let ((nskk-azik-conversion-table '(("qz" "くす"))))
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (should (equal (nskk-converter-lookup "qz") "くす"))
        (should (equal (nskk-converter-get-rule "qz") "くす"))
        (should (equal (nskk-prolog-query-value
                        '(azik-rule "qz" \?kana) '\?kana)
                       "くす"))))))

(provide 'nskk-azik-e2e-test)

;;; nskk-azik-e2e-test.el ends here
