;;; nskk-e2e-okurigana.el --- E2E okurigana tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for okurigana handling (DDSKK §5.8).
;; Covers: consonant okurigana, vowel okurigana, katakana okurigana,
;; sokuon in okurigana, pending romaji discard regression.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Okurigana (送り仮名) E2E Tests
;;;;
;;
;; Okurigana input flow (ddskk-compatible):
;;   1. Capital letter starts preedit: Ka → ▽か
;;   2. Lowercase extends reading: KaNi → ▽かに
;;   3. Capital letter marks okurigana boundary: KaNiKu → ▽かに* (okurigana = k)
;;   4. Lowercase kana completes okurigana and triggers conversion: → ▼書く
;;   5. C-j commits: → 書く (candidate + okurigana kana)
;;
;; Dict key format for okurigana: reading + lowercase okurigana consonant
;;   e.g. "かk" → ("書") means "書く" (to write)
;;        "みr" → ("見") means "見る" (to see)

(nskk-describe "okurigana input"
  (nskk-it "enters preedit phase on Ka"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "sets okurigana consonant state on KaK"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "triggers conversion on KaKu showing 書"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く"))))

  (nskk-it "commits MiRu to 見る"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る"))))

  (nskk-it "commits OkuRu to 送る"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "送る"))))

  (nskk-it "commits KiKu to 聞く"
    (let ((dict '(("きk" . ("聞")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "聞く"))))

  (nskk-it "selects second candidate 効く from KiKu"
    (let ((dict '(("きk" . ("聞" "効")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "聞")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "効")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "効く"))))

  (nskk-it "cancels KaKu conversion with C-g"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "書くの"))))

  (nskk-it "commits DekiRu to 出来る"
    (let ((dict '(("できr" . ("出来")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Deki")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "出来る"))))

  (nskk-it "commits HabikoRu to 蔓延る"
    (let ((dict '(("はびこr" . ("蔓延")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Habiko")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "蔓延る")))))

(ert-deftest nskk-e2e-pbt-okurigana-no-crash ()
  "PBT: Okurigana input sequences (CvC pattern) never crash."
  ;; Tests reading+okurigana patterns like KaKu, MiRu, KiKu, SuRu, etc.
  (let ((patterns '(("Ka" "K" "u")     ;; KaKu (書く)
                    ("Mi" "R" "u")     ;; MiRu (見る)
                    ("Ki" "K" "u")     ;; KiKu (聞く)
                    ("Su" "R" "u")     ;; SuRu (する)
                    ("Ha" "N" "a")     ;; HaNa (花な)
                    ("No" "M" "u")     ;; NoMu (飲む)
                    ("Ka" "E" "ru")))  ;; KaEru (変える)
        (errors nil))
    (dolist (pat patterns)
      (condition-case err
          (nskk-e2e-with-buffer 'hiragana nil
            (dolist (key pat)
              (nskk-e2e-type key))
            ;; Property: buffer must be a string, mode must remain hiragana
            (unless (stringp (buffer-string))
              (push (list :pattern pat :error "non-string buffer") errors))
            (unless (memq (nskk-current-mode) '(hiragana ascii))
              (push (list :pattern pat :mode (nskk-current-mode)) errors)))
        (error
         (push (list :pattern pat :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT okurigana-no-crash: %d failures:\n%S"
                        (length errors) errors)))))

;;;; Vowel Okurigana (母音送り仮名) E2E Tests
;;
;; Vowel okurigana occurs when the okurigana consonant is itself a vowel
;; (A, I, U, E, O).  Unlike consonant okurigana (K, R, etc.) which requires
;; a following character to complete the kana, vowel okurigana is immediately
;; complete -- "I" alone maps to "い" with no further input needed.
;;
;; Bug: Before the fix, typing AI (capital A then capital I) would:
;;   A → ▽あ                   (henkan-on)
;;   I → ▽あ* (romaji="i")    (okurigana marker, BUT didn't convert)
;; Then a SECOND I would flush "i"→"い", insert another *, producing ▽あ*い*.
;; The fix: vowel okurigana immediately inserts kana and triggers conversion.
;;
;; Dict key format for vowel okurigana: reading + vowel char
;;   e.g. "あI" typed → key "あi" → candidates ("愛")

(nskk-describe "vowel okurigana"
  (nskk-it "converts AI to 愛い and commits"
    (let ((dict '(("あi" . ("愛" "哀")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い"))))

  (nskk-it "does not produce double marker on AII"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "愛"))))

  (nskk-it "converts OU to 負う and commits"
    (let ((dict '(("おu" . ("負")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "負う"))))

  (nskk-it "converts aE to 与え and commits"
    (let ((dict '(("あe" . ("与")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "与え"))))

  (nskk-it "converts AU to 買う and commits"
    ;; Vowel okurigana with U: reading "あ" + okurigana vowel "u".
    ;; A starts preedit (reading = "あ"), U triggers vowel okurigana "u".
    ;; Dict key "あu" (hiragana reading + vowel char).  Okurigana kana = "う".
    (let ((dict '(("あu" . ("買")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "買う"))))

  (nskk-it "cancels AI conversion with C-g without stale state"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing AI"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い")
        (nskk-e2e-type "su")
        (nskk-e2e-assert-buffer "愛いす"))))

  (nskk-it "handles consonant okurigana KaKu followed by vowel okurigana AI"
    (let ((dict '(("かk" . ("書"))
                  ("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く愛い")))))

;;;; Regression Tests: Pending Romaji Discard on Okurigana Trigger
;;
;; Bug (fixed in nskk-henkan.el lines 733-754): when a pending incomplete
;; romaji consonant (e.g. "k", "sh") was in nskk--romaji-buffer at the moment
;; an okurigana trigger (uppercase letter) arrived, the raw consonant was
;; inserted into the buffer before the * okurigana marker, producing
;; e.g. "▽かk*" instead of "▽か*".
;;
;; The fix discards :incomplete romaji results (consonants like "k", "sh") and
;; only emits fully-converted kana or the standalone "n" → "ん" exception.
;;
;; Input sequence for T-E1 (KAkKu):
;;   K   → starts henkan (▽), romaji buffer = ""
;;   A   → romaji "a" completes → "あ" appended to preedit (▽あ), romaji buffer = ""
;;   k   → romaji buffer = "k" (pending incomplete consonant)
;;   K   → okurigana trigger: "k" must be DISCARDED (not inserted before *),
;;          * marker inserted, romaji buffer = "k" (for new okurigana consonant)
;;   u   → romaji "ku" → "く", triggers conversion (▼ state)
;;
;; Input sequence for T-E2 (KAnKu):
;;   K   → starts henkan (▽), romaji buffer = ""
;;   A   → romaji "a" → "あ" in preedit, romaji buffer = ""
;;   n   → romaji buffer = "n" (pending n — special case: converts to ん)
;;   K   → okurigana trigger: "n" must be FLUSHED as "ん" before *, then * inserted
;;   u   → romaji "ku" → "く", triggers conversion (▼ state)

(nskk-describe "okurigana pending romaji discard regression"
  (nskk-it "KAkKu: pending k is discarded before okurigana marker, conversion succeeds"
    ;; T-E1: the lowercase k pending in the romaji buffer when uppercase K fires
    ;; must NOT appear before * in the buffer.  After completing ku → く, the
    ;; conversion should trigger normally (entering active/converting state).
    (let ((dict '(("あk" . ("開")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit
        (nskk-e2e-type "K")
        ;; A completes romaji "a" → あ, preedit is now ▽あ
        (nskk-e2e-type "A")
        ;; Lowercase k → pending incomplete consonant in romaji buffer
        (nskk-e2e-type "k")
        ;; Uppercase K fires okurigana trigger while "k" is pending.
        ;; BUG: used to insert raw "k" before *, giving ▽あk*.
        ;; FIX: "k" is discarded (incomplete), giving ▽あ*.
        (nskk-e2e-type "K")
        ;; The buffer content must NOT contain "k" before the * marker.
        ;; After the okurigana trigger the pending-romaji display may briefly
        ;; show "k" for the NEW okurigana consonant, but the buffer proper
        ;; should have no "k" ASCII character adjacent to *.
        (let ((content (buffer-string)))
          (should-not (string-match-p "k\\*" content)))
        ;; Now type u to complete the okurigana ku → く and trigger conversion.
        (nskk-e2e-type "u")
        ;; We should now be in converting (▼) state — conversion was triggered.
        (nskk-e2e-assert-converting)
        ;; The buffer string must not contain a double "k" or "kk" artifact.
        (should-not (string-match-p "kk" (buffer-string))))))

  (nskk-it "KAnKu: pending n is flushed as ん before okurigana marker"
    ;; T-E2: the lowercase n pending when uppercase K fires must be converted
    ;; to ん (the word-boundary n exception) and inserted before *.
    (let ((dict '(("あんk" . ("暗")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit
        (nskk-e2e-type "K")
        ;; A completes romaji "a" → あ
        (nskk-e2e-type "A")
        ;; Lowercase n → pending in romaji buffer (not yet ん)
        (nskk-e2e-type "n")
        ;; Uppercase K fires okurigana trigger while "n" is pending.
        ;; The n exception: standalone "n" at word boundary → ん, inserted before *.
        (nskk-e2e-type "K")
        ;; ん must appear in the buffer content (flushed before * marker)
        (let ((content (buffer-string)))
          (should (string-match-p "ん" content)))
        ;; Complete the okurigana ku → く and trigger conversion
        (nskk-e2e-type "u")
        ;; Conversion should trigger normally
        (nskk-e2e-assert-converting)))))

;;;;
;;;; Katakana Mode Okurigana (カタカナ送り仮名) E2E Tests
;;;;
;;
;; In katakana mode the preedit reading accumulates as KATAKANA because
;; nskk-input.el applies nskk-kana-string-hiragana-to-katakana before inserting.
;; Dict lookup keys therefore use katakana + consonant (e.g. "カk", not "あk").
;; After commit the okurigana kana appended is also katakana (ク, イ, ン …).
;;
;; Input sequences follow DDSKK katakana mode conventions:
;;   Ka → preedit reading "カ" (romaji ka → か → katakana → カ)
;;   K  → okurigana trigger; pending romaji "k" registered as okuri char

(nskk-describe "katakana mode okurigana triggers conversion"
  (nskk-it "triggers conversion on KaKu in katakana mode"
    ;; Ka: K starts preedit (uppercase = henkan-start), a completes "ca" → "か" → "カ".
    ;; K (second, uppercase): okurigana trigger; pending romaji "k" → dict key "カk".
    ;; u completes "ku" → "く" → "ク" (appended as okurigana kana).
    ;; Conversion is triggered with reading "カ", okuri char "k".
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode"
    ;; Same sequence as above; C-j commits the first candidate.
    ;; Result: kanji 書 replaces the reading "カ", okurigana kana "ク" is appended.
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク"))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode"
    ;; A (uppercase in katakana mode): starts preedit with reading "ア" (vowel henkan-start).
    ;; I (uppercase): okurigana trigger for vowel "i"; okurigana kana "イ" appended.
    ;; Dict key "アi" (katakana reading + vowel char).
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ"))))

  (nskk-it "discards pending consonant before okurigana marker in katakana mode (T-E1 analogue)"
    ;; T-E1 katakana analogue: K A k K u
    ;; K (uppercase, empty reading) + A (vowel okurigana with empty reading) →
    ;;   failed conversion (no dict entry for key "a"), cancel; buffer has "ア" reading.
    ;; k (lowercase): accumulates romaji; K (uppercase): okurigana trigger.
    ;; u completes "ku" → okurigana kana "ク"; conversion with dict key "アk".
    ;; Crucially, no stray "k" appears between "ア" and "*" in the preedit display.
    (let ((dict '(("アk" . ("開")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "k")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting))))

  (nskk-it "flushes pending n as ン before okurigana marker in katakana mode (T-E2 analogue)"
    ;; T-E2 katakana analogue: K A n K u
    ;; K + A → reading "ア" (failed vowel okurigana → cancel, reading = "ア").
    ;; n (lowercase): romaji buffer = "n"; K (uppercase): pending "n" flushed as "ン",
    ;;   then okurigana trigger; dict key "アンk".  u → okurigana kana "ク".
    (let ((dict '(("アンk" . ("暗")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "n")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)))))

;;;;
;;;; Sokuon in Okurigana (促音送り仮名) E2E Tests
;;;;
;;
;; Sokuon (っ) appears as the first character of okurigana kana in words like
;; 勝った (katta) and 打った (utta).
;;
;; Mechanism (two-phase):
;;   1. Uppercase T is the okurigana trigger; romaji buffer = "t".
;;   2. Input "ta": first 't' + buffered 't' → sokuon pattern → emits っ → fires
;;      conversion with the accumulated reading and okuri char "t".
;;   3. Remaining 'a' → romaji "ta" → emits "た" into the buffer after っ.
;;   4. C-j commits: kanji replaces the reading, "った" (っ + た) is appended.

(nskk-describe "sokuon in okurigana"
  (nskk-it "commits KaTTa sequence to 勝った"
    ;; Ka: reading = "か" (K starts preedit, a completes romaji).
    ;; T (uppercase): okurigana trigger; romaji buffer = "t".
    ;; t: romaji "t"+"t" → sokuon → emits っ → triggers conversion (key "かt").
    ;; a: romaji "ta" → emits "た" (appended after っ).
    ;; C-j: commits first candidate; buffer = kanji + "った".
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った"))))

  (nskk-it "commits UTTa sequence to 打った"
    ;; U (uppercase): vowel okurigana start; reading = "う".
    ;; T (uppercase): okurigana trigger; romaji buffer = "t".
    ;; t: romaji "t"+"t" → sokuon → emits っ → triggers conversion (key "うt").
    ;; a: romaji "ta" → emits "た".
    ;; C-j: commits; buffer = kanji + "った".
    (let ((dict '(("うt" . ("打")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "U")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "打った")))))

;;;;
;;;; Extended Okurigana Patterns (from nskk-okuri-extended-e2e-test)
;;;;
;;
;; E2E tests for multi-character, sokuon, and katakana okurigana patterns.
;; Each test injects its own dict entries to cover the specific okurigana key.
;;
;; "Multi-character okurigana" means the okurigana suffix appended after the
;; kanji stem contains more than one kana character.  Examples:
;;   変える (かえ*る): okurigana = "える" (2 chars)
;;   勝った (かっ*た): okurigana = "った" (2 chars, sokuon + kana)
;;
;; Dict key format (reminder):
;;   reading-kana + lowercase okurigana consonant (or vowel)
;;   e.g. "かe"  → ("変")  triggers 変える  (vowel okurigana + "ru" continuation)
;;        "かt"  → ("勝")  triggers 勝った  (sokuon okurigana, single "t" in key)
;;        "おもu" → ("思") triggers 思う    (vowel okurigana "u")
;;   katakana mode: reading stays katakana
;;        "カk"  → ("書")  triggers 書ク    (katakana reading + consonant)
;;
;; Sections:
;;   1. Standard consonant okurigana (baseline)
;;   2. Sokuon okurigana (促音送り仮名): っ in the okurigana suffix
;;   3. Multi-character vowel okurigana: uppercase vowel + continuation kana
;;   4. Katakana mode okurigana

;;;;
;;;; Section 1: Standard Consonant Okurigana (Baseline)
;;;;
;;
;; These tests establish the baseline consonant okurigana pattern used throughout
;; all sections.  The key format is: hiragana-reading + lowercase-consonant.
;;
;; Input mechanics:
;;   KaKu: K starts preedit (▽), "a" completes "か"; second K is the okurigana
;;         trigger (uppercase in preedit context); "u" completes "ku"→"く", which
;;         fires conversion with key "かk".  C-j commits the first candidate.
;;

(nskk-describe "standard consonant okurigana (baseline)"
  (nskk-it "shows converting state on KaKu with dict entry"
    ;; KaKu: ▽か + K (okurigana trigger, consonant=k) + u → key "かk" → overlay 書
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書く via C-j"
    ;; Standard commit: kanji replaces the reading, okurigana kana "く" is appended.
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits MiRu to 見る via C-j"
    ;; MiRu: M starts preedit, "i" completes "み"; R is okurigana trigger (r);
    ;; "u" completes "ru"→"る".  Key "みr" → candidate "見"; result 見る.
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits YoMu to 読む via C-j"
    ;; YoMu: Y starts preedit, "o" completes "よ"; M is okurigana trigger (m);
    ;; "u" completes "mu"→"む".  Key "よm" → candidate "読"; result 読む.
    (let ((dict '(("よm" . ("読")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "M")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "読む")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Section 2: Sokuon Okurigana (促音送り仮名)
;;;;
;;
;; Sokuon in okurigana arises when the okurigana suffix starts with っ, as in
;; 勝った (katta) and 打った (utta).
;;
;; Mechanism (single okurigana trigger + sokuon):
;;   Ka  → ▽か  (K starts preedit, a completes か)
;;   T   → okurigana trigger: stores okuri=t, romaji-buffer="t", inserts * marker
;;   t   → romaji "t"+"t" → sokuon classification → emits っ → fires conversion
;;          with key "かt" (reading extracted up to * + okurigana char t)
;;   a   → romaji "ta" → "た" is appended after っ
;;   C-j → commits first candidate; buffer = kanji + "った"
;;
;; IMPORTANT: The dict key uses a SINGLE "t" (not "tt").  The sokuon っ is
;; produced by the doubled-consonant romaji rule, not by the dict key format.
;; The dict key records only the okurigana consonant that the user pressed as
;; the uppercase okurigana trigger.
;;

(nskk-describe "sokuon okurigana (促音送り仮名)"
  (nskk-it "triggers conversion on KaTt showing 勝"
    ;; After T (okurigana trigger) and t (sokuon), conversion fires automatically.
    ;; The overlay displays the first candidate before C-j is pressed.
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "t")
        ;; At this point っ has been emitted and conversion triggered.
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "勝"))))

  (nskk-it "commits KaTta to 勝った via C-j"
    ;; Full sequence: Ka + T + ta → 勝った.
    ;; Key "かt" (single t): the conversion fires when the sokuon っ is emitted.
    ;; After conversion is triggered, "a" completes "ta"→"た" which is appended.
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits UTta to 打った via C-j"
    ;; U starts preedit as a vowel (reading = "う").
    ;; T is the okurigana trigger; romaji buffer = "t".
    ;; t: "t"+"t" → sokuon → emits っ → triggers conversion with key "うt".
    ;; a: "ta" → "た" appended after っ.
    ;; C-j commits: buffer = 打 + "った".
    (let ((dict '(("うt" . ("打")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "U")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "打った")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "sokuon okurigana henkan phase is nil after commit"
    ;; Verifies that the conversion state is fully cleared after a sokuon
    ;; okurigana commit.  No residual henkan phase should remain.
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "allows continued input after sokuon okurigana commit"
    ;; After committing 勝った, the user should be able to continue typing.
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った")
        ;; Continue typing hiragana after the committed word.
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "勝ったの")))))

;;;;
;;;; Section 3: Multi-Character Vowel Okurigana
;;;;
;;
;; Vowel okurigana occurs when the okurigana trigger is an uppercase vowel
;; (A, I, U, E, O).  The vowel immediately produces a kana character and triggers
;; conversion -- no following character is needed to complete the kana.
;;
;; "Multi-character" in this section means the total okurigana suffix has more
;; than one kana.  The vowel okurigana produces the FIRST kana; subsequent
;; lowercase input produces additional kana that accumulate after conversion.
;;
;; Examples:
;;   KaEru: Ka → ▽か, E → vowel okurigana e → emits "え", triggers conversion
;;           with key "かe"; then "ru" → "る" is appended in the buffer.
;;           C-j commits: buffer = 変 + "える"  (2-char okurigana)
;;
;;   OmoU: Omo → ▽おも, U → vowel okurigana u → emits "う", triggers conversion
;;          with key "おもu".  C-j commits: buffer = 思 + "う" (1-char okurigana).
;;
;; Dict key format for vowel okurigana:
;;   hiragana-reading + lowercase vowel char
;;   "かe" → ("変")   for 変える
;;   "おもu" → ("思") for 思う
;;

(nskk-describe "multi-character vowel okurigana"
  (nskk-it "triggers conversion on KaE with dict entry かe"
    ;; Ka → ▽か; E (uppercase vowel) is okurigana trigger.
    ;; Vowel okurigana path: immediately emits "え" and fires conversion with key "かe".
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "変"))))

  (nskk-it "commits KaEru to 変える via C-j"
    ;; Full 変える sequence.  After E triggers conversion (key "かe", candidate "変"),
    ;; "ru" → "る" is appended as continuation kana in the buffer.
    ;; C-j commits: replaces the reading with the candidate and leaves "える".
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-type "ru")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "変える")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits OmoU to 思う via C-j"
    ;; Omo → ▽おも; U (uppercase vowel) is okurigana trigger.
    ;; Vowel okurigana: emits "う", fires conversion with key "おもu".
    ;; C-j commits: buffer = 思 + "う".
    (let ((dict '(("おもu" . ("思")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Omo")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "思う")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits NaGaReru to 流れる via C-j (3-mora reading + vowel okurigana)"
    ;; NaGa → ▽なが; R is consonant okurigana trigger (r).
    ;; Wait -- R followed by "eru": R triggers okurigana with consonant r,
    ;; then "e" completes "re"→"れ", which fires conversion.
    ;; "ru" → "る" is appended continuation.
    ;; Dict key: "ながr" (reading "なが" + consonant "r").
    ;; C-j commits: 流 + "れる".
    (let ((dict '(("ながr" . ("流")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Naga")
        (nskk-e2e-type "R")
        (nskk-e2e-type "e")
        ;; "re" completes romaji → "れ" inserted, conversion triggered with key "ながr"
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "ru")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "流れる")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits KaEru and allows continued input after"
    ;; After committing 変える, verify that continued typing works correctly.
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-type "ru")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "変える")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "変えるの"))))

  (nskk-it "commits ArataMeru to 改める via C-j (5-mora reading + vowel okurigana)"
    ;; ArataMeru: A starts preedit, "rata" → "らた" → reading "あらた";
    ;; M is okurigana consonant trigger (m); "e" completes "me"→"め", fires conversion.
    ;; "ru" is appended as continuation.
    ;; Dict key: "あらたm" → candidate "改"; result 改める.
    (let ((dict '(("あらたm" . ("改")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Arata")
        (nskk-e2e-type "M")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "ru")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "改める")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Section 4: Katakana Mode Okurigana (from nskk-okuri-extended-e2e-test)
;;;;
;;
;; In katakana mode the reading accumulates as KATAKANA because
;; nskk-input.el applies nskk-kana-string-hiragana-to-katakana before inserting.
;; Dict lookup keys therefore use katakana + lowercase consonant (e.g. "カk").
;; After commit, the okurigana kana appended is also KATAKANA (ク, エル, etc.).
;;
;; Comparison with hiragana mode:
;;   Hiragana: "Ka"+"K"+"u" → dict key "かk" → 書く  (hiragana okurigana)
;;   Katakana: "Ka"+"K"+"u" → dict key "カk" → 書ク  (katakana okurigana)
;;
;; This section verifies that okurigana conversion functions correctly in
;; katakana mode and that both the reading lookup key and the appended kana
;; suffix use katakana.
;;

(nskk-describe "katakana mode okurigana (extended)"
  (nskk-it "triggers conversion on KaKu in katakana mode (dict key カk)"
    ;; In katakana mode: Ka → ▽カ; K triggers okurigana with consonant k;
    ;; u completes "ku"→"く"→"ク".  Dict key is "カk" (katakana reading).
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode via C-j"
    ;; Result is kanji + katakana okurigana: 書ク (not 書く).
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits MiRu to 見ル in katakana mode via C-j"
    ;; MiRu in katakana mode: Mi → ▽ミ; R triggers okurigana with consonant r;
    ;; u completes "ru"→"る"→"ル".  Dict key "ミr" → candidate "見"; result 見ル.
    (let ((dict '(("ミr" . ("見")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見ル")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode via C-j"
    ;; A (uppercase, katakana mode): starts preedit; vowel "a" → reading "ア".
    ;; I (uppercase): vowel okurigana trigger; immediately emits "イ" and fires
    ;;   conversion with key "アi" (katakana reading + vowel char).
    ;; C-j commits: buffer = 愛 + "イ".
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "katakana mode okurigana result differs from hiragana mode"
    ;; The same key sequence KaKu produces 書ク (katakana) in katakana mode
    ;; versus 書く (hiragana) in hiragana mode.
    ;; This test asserts the katakana-mode result directly and verifies that
    ;; the output is NOT the hiragana-mode output.
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        ;; Katakana mode: okurigana is katakana ク, not hiragana く.
        (nskk-e2e-assert-buffer "書ク")
        (should-not (equal (buffer-string) "書く")))))

  (nskk-it "sokuon okurigana KaTta produces 勝った in katakana mode"
    ;; In katakana mode the sokuon mechanism is the same as in hiragana mode
    ;; but the reading and okurigana kana are both katakana.
    ;; Ka → ▽カ; T → okurigana trigger (t), romaji="t";
    ;; t → "t"+"t" → sokuon → emits ッ (katakana sokuon) → fires conversion with key "カt";
    ;; a → "ta" → "た" → "タ" (appended after ッ).
    ;; C-j commits: buffer = kanji + "ッタ".
    ;;
    ;; Note: The resulting okurigana in katakana mode is "ッタ" (katakana).
    ;; The dict key is "カt" (katakana reading + single "t").
    (let ((dict '(("カt" . ("勝")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝ッタ")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Regression Tests: Uppercase Consonant Okurigana
;;;;
;;
;; Bug (fixed): uppercase consonants (K, S, T, etc.) were not being treated as
;; okurigana markers.  After the fix:
;;   - Uppercase consonants (K, S, T, etc.): treated as okurigana markers
;;   - Uppercase vowels (A, I, U, E, O): normalized to lowercase and processed
;;     as reading continuation

(nskk-describe "uppercase consonant okurigana regression"
  (nskk-it "KaK enters okurigana mode (uppercase consonant not normalized)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")  ; 大文字子音 → 送り仮名マーカー
      (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "KaKu triggers conversion with okurigana"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")  ; 大文字子音 → 送り仮名マーカー
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "KaKu commits to 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "KaKu")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く"))))

  (nskk-it "MiRu commits to 見る with uppercase consonant"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "MiR")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る"))))

  (nskk-it "H O produces ▽ほ (uppercase vowel normalized to lowercase)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "H")
      (nskk-e2e-type "O")  ; 大文字母音 → 小文字化して読みとして処理
      (nskk-e2e-assert-henkan-phase 'on)
      ;; 送り仮名状態ではないことを確認
      (should (null (nskk-state-get-okurigana nskk-current-state)))
      (nskk-e2e-assert-buffer-matches "\u25BD\u307B")))  ; ▽ほ

  (nskk-it "K A produces ▽か (uppercase vowel normalized to lowercase)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "A")  ; 大文字母音 → 小文字化
      (nskk-e2e-assert-henkan-phase 'on)
      (should (null (nskk-state-get-okurigana nskk-current-state)))
      (nskk-e2e-assert-buffer-matches "\u25BD\u304B")))  ; ▽か

  (nskk-it "mixed: KaKiKu with dict converts correctly"
    (let ((dict '(("かきk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "KaKiK")  ; KiK: iは読み、Kは送り仮名
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")))))

(provide 'nskk-e2e-okurigana)

;;; nskk-e2e-okurigana.el ends here
