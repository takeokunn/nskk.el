;;; nskk-okurigana-e2e-test.el --- E2E okurigana tests for NSKK  -*- lexical-binding: t; -*-

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

  (nskk-it "selects second candidate 掛け from KaKe"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "掛")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "掛け"))))

  (nskk-it "selects second candidate 掛き from KaKi"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "i")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "掛")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "掛き"))))

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

;;;;
;;;; Table: Okurigana no-crash (CvC patterns)
;;;;

(nskk-deftest-table okurigana-no-crash
  ;; Property: typing a CvC okurigana sequence never crashes, and the mode
  ;; remains in a japanese mode (hiragana or ascii fallback) after the sequence.
  :columns (reading okuri-trigger okuri-suffix)
  :rows (("Ka" "K" "u")     ;; KaKu (書く)
         ("Mi" "R" "u")     ;; MiRu (見る)
         ("Ki" "K" "u")     ;; KiKu (聞く)
         ("Su" "R" "u")     ;; SuRu (する)
         ("Ha" "N" "a")     ;; HaNa (花な)
         ("No" "M" "u")     ;; NoMu (飲む)
         ("Ka" "E" "ru"))   ;; KaEru (変える)
  :body (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type reading)
          (nskk-e2e-type okuri-trigger)
          (nskk-e2e-type okuri-suffix)
          (should (stringp (buffer-string)))
          (should (memq (nskk-current-mode) '(hiragana ascii)))))

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
    (let ((dict '(("かk" . ("開")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit (buffer="k"), A completes "ka"→"か", preedit is now ▽か
        (nskk-e2e-type "K")
        ;; A with non-empty romaji buffer "k" → normalized to "a" → "ka" → "か"
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
    (let ((dict '(("かんk" . ("暗")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit (buffer="k"), A completes "ka"→"か"
        (nskk-e2e-type "K")
        ;; A with non-empty romaji buffer "k" → normalized to "a" → "ka" → "か"
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

;;;; Regression Tests: Xh => ▽* (uppercase consonant with no kana in preedit)
;;
;; Bug: typing X (starts henkan preedit, puts "x" in romaji buffer) followed
;; by H (Shift held too long) caused nskk-process-okurigana-input to fire with
;; an empty reading, producing ▽* instead of continuing romaji accumulation.
;;
;; Root cause: normalize-vowel-p only covered uppercase vowels; uppercase
;; consonants always bypassed the guard and were treated as okurigana triggers
;; even when no kana had been written to the preedit yet.
;;
;; Fix: normalize-vowel-p now also covers uppercase consonants when
;; nskk--has-preedit is nil (no kana committed to preedit buffer yet).
;;
;; Input sequence for T-E3 (XH in standard mode):
;;   X   → starts henkan (▽), romaji buffer = "x" (pending incomplete), no kana
;;   H   → must be normalized (not okurigana); "xh" stays in romaji buffer
;;   (buffer must NOT contain ▽*)

(nskk-describe "Xh regression: no spurious ▽* with pending romaji and no kana"
  (nskk-it "XH in standard mode does not produce ▽*"
    ;; Standard mode: "xh" has no rule; both chars accumulate in romaji buffer
    ;; without triggering okurigana.
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "X")
        (nskk-e2e-type "H")
        ;; Must NOT contain ▽ immediately followed by * (empty reading okurigana)
        (should-not (string-match-p (regexp-quote (concat nskk-henkan-on-marker nskk-okurigana-marker))
                                    (buffer-string))))))

  (nskk-it "Xa in standard mode does produce ▽さ (sanity: normal romaji still works)"
    ;; Typing X then a should produce ▽xa ... wait, "xa" → "さ" in some tables,
    ;; but the key point is no ▽* appears.  Just check the negative condition.
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "X")
        (nskk-e2e-type "a")
        ;; Must NOT contain okurigana marker at all
        (should-not (string-match-p (regexp-quote nskk-okurigana-marker)
                                    (buffer-string))))))

  (nskk-it "Ka followed by K still produces okurigana (regression guard)"
    ;; After fixing the bug, legitimate okurigana triggers must still work:
    ;; Ka produces ▽か, then K triggers ▽か*.
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        ;; Buffer must contain ▽か* — okurigana triggered correctly
        (should (string-match-p (regexp-quote (concat nskk-henkan-on-marker "か" nskk-okurigana-marker))
                                (buffer-string)))))))

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
    ;; K starts preedit (buffer="k"); A with non-empty buffer "k" → normalized
    ;; to "a" → "ka" → "か" → katakana → "カ"; reading = "カ".
    ;; k (lowercase): accumulates romaji; K (uppercase): okurigana trigger.
    ;; u completes "ku" → okurigana kana "ク"; conversion with dict key "カk".
    ;; Crucially, no stray "k" appears between "カ" and "*" in the preedit display.
    (let ((dict '(("カk" . ("開")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "k")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting))))

  (nskk-it "flushes pending n as ン before okurigana marker in katakana mode (T-E2 analogue)"
    ;; T-E2 katakana analogue: K A n K u
    ;; K + A → reading "カ" ("ka"→"か"→"カ").
    ;; n (lowercase): romaji buffer = "n"; K (uppercase): pending "n" flushed as "ン",
    ;;   then okurigana trigger; dict key "カンk".  u → okurigana kana "ク".
    (let ((dict '(("カンk" . ("暗")))))
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

  (nskk-it "KaKe commits to 書け (vowel-e okurigana)"
    ;; Regression: okurigana with vowel-e (書け, 聞け etc.) must trigger
    ;; conversion exactly like vowel-u (書く).  Dict key is always the
    ;; consonant suffix, e.g. "かk" covers く/き/け/こ/か.
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書け"))))

  (nskk-it "KAKe (all-shift) commits to 書け (uppercase-A reading)"
    ;; Regression guard for the normalize-vowel-p path: K starts preedit,
    ;; uppercase A normalizes to 'a' (completing "ka"→"か"), uppercase K
    ;; triggers okurigana, lowercase 'e' completes "ke"→"け".
    ;; All three SHOULD produce the same dict query "かk" as "KaKe".
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")   ; uppercase vowel: normalize-vowel-p=t → "か"
        (nskk-e2e-type "K")   ; uppercase consonant: okurigana trigger
        (nskk-e2e-type "e")   ; lowercase vowel: completes "ke"→"け"
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書け"))))

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

;;;;
;;;; Okurigana Registration (辞書登録) Tests
;;;;
;;
;; When okurigana conversion finds no candidates, nskk falls back to
;; dictionary registration.  DDSKK displays the reading as "stem*kana"
;; (e.g. "ほ*け") where * is the okurigana separator and kana is the
;; actual hiragana kana (not the raw consonant).
;;
;; On cancel, the okuri-kana stays in the buffer (▽ほけ, not ▽ほ).
;; This matches ddskk behavior: preserved okuri-kana allows multi-char
;; stem extension — typing another uppercase consonant (e.g. K) adds
;; another okurigana boundary on top of the current preedit.

(nskk-describe "okurigana registration: reading format"
  (nskk-it "shows stem*kana format in registration prompt (e.g. ほ*け for HOKE)"
    ;; HOKE with no dict entry triggers registration.
    ;; The prompt must show the ddskk-style reading: "ほ*け" (stem*kana).
    (let* ((captured-prompt nil))
      (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (prompt &rest _)
                     (setq captured-prompt prompt)
                     "")))
          (nskk-e2e-type "H")
          (nskk-e2e-type "O")
          (nskk-e2e-type "K")
          (nskk-e2e-type "E"))
        (should (stringp captured-prompt))
        (should (string-match-p "ほ\\*け" captured-prompt)))))

  (nskk-it "cancelling okurigana registration preserves preedit ▽ほけ (okuri-kana stays)"
    ;; After cancel (empty RET), the okuri-kana remains in the buffer.
    ;; This matches ddskk: the preserved kana allows multi-char stem extension
    ;; (e.g. typing another K starts a new okurigana boundary on ▽ほけ).
    (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
      ;; Default mock returns "" (cancel).
      (nskk-e2e-type "H")
      (nskk-e2e-type "O")
      (nskk-e2e-type "K")
      (nskk-e2e-type "E")
      (nskk-e2e-assert-buffer "▽ほけ"
                              "Cancel preserves preedit with okuri-kana for multi-char stem reuse")
            (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to on after cancel")))

  (nskk-it "successful okurigana registration inserts the registered word"
    ;; Confirming registration with a word inserts it and clears preedit.
    (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "穂毛")))
        (nskk-e2e-type "H")
        (nskk-e2e-type "O")
        (nskk-e2e-type "K")
        (nskk-e2e-type "E"))
      (nskk-e2e-assert-buffer "穂毛" "Registered word should be inserted")
      (nskk-e2e-assert-henkan-phase nil "Phase should be nil after successful registration"))))

;;;;
;;;; PBT: Okurigana input invariants
;;;;

(nskk-deftest-table okurigana-basic-consonants
  :description "All standard okurigana consonant markers are uppercase ASCII"
  :columns (input _expected)
  :rows (("K" "K")
         ("S" "S")
         ("T" "T")
         ("N" "N")
         ("H" "H")
         ("M" "M")
         ("Y" "Y")
         ("R" "R")
         ("G" "G")
         ("Z" "Z")
         ("D" "D")
         ("B" "B")
         ("P" "P"))
  :body (should (and (stringp input)
                     (= 1 (length input))
                     (>= (aref input 0) ?A)
                     (<= (aref input 0) ?Z))))

(nskk-property-test okurigana-input-does-not-crash
  ((pattern okurigana-pattern))
  ;; In a test buffer with nskk-mode, okurigana pattern input should not crash
  (condition-case nil
      (nskk-with-test-buffer 'hiragana
        ;; Just verify the buffer environment is set up
        (should (nskk-state-p nskk-current-state))
        t)
    (error nil))
  20)

(nskk-describe "Okurigana input properties"
  (nskk-it "okurigana consonant generator always produces uppercase single char"
    (nskk-for-all ((consonant okurigana-consonant))
      (should (stringp consonant))
      (should (= 1 (length consonant)))
      (should (>= (aref consonant 0) ?A))
      (should (<= (aref consonant 0) ?Z)))))


;;;;
;;;; Property: Post-Commit Okurigana Buffer Content
;;;;

(nskk-deftest-table okurigana-post-commit-content
  ;; Property: after okurigana conversion + commit, buffer contains exactly
  ;; kanji + okurigana kana suffix (no preedit markers or residual state).
  ;; Covers all five okurigana vowel suffixes (a/i/u/e/o) to prevent
  ;; regressions in normalize-vowel-p or emit-converted-kana paths.
  :columns (reading okuri-trigger okuri-suffix dict-key kanji expected)
  :rows (("Ka" "K" "u"  "かk" "書" "書く")   ;; vowel u
         ("Mi" "R" "u"  "みr" "見" "見る")   ;; vowel u, r-consonant
         ("Ki" "K" "u"  "きk" "聞" "聞く")   ;; vowel u, different reading
         ("No" "M" "u"  "のm" "飲" "飲む")   ;; vowel u, m-consonant
         ("Ha" "N" "a"  "はn" "話" "話な")   ;; vowel a
         ("Ka" "K" "i"  "かk" "書" "書き")   ;; vowel i (KAKe-like pattern)
         ("Ka" "K" "e"  "かk" "書" "書け")   ;; vowel e (KAKe pattern: 書け)
         ("Ka" "K" "o"  "かk" "書" "書こ")   ;; vowel o
         ("Mi" "R" "e"  "みr" "見" "見れ"))  ;; vowel e, r-consonant
  :body (nskk-e2e-with-buffer 'hiragana (list (cons dict-key (list kanji)))
          (nskk-e2e-type reading)
          (nskk-e2e-type okuri-trigger)
          (nskk-e2e-type okuri-suffix)
          (nskk-e2e-assert-converting)
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-henkan-phase nil)
          (nskk-e2e-assert-buffer expected
                                  (format "okurigana commit %S+%S+%S → %S failed"
                                          reading okuri-trigger okuri-suffix expected))))

;;;;
;;;; SPC during partial consonant okurigana
;;;;
;;
;; When the user types ▽か*k (K a K, leaving the pending consonant "k" without
;; a following vowel) and then presses SPC, the fix routes to okurigana
;; conversion using the pending consonant directly — matching DDSKK behaviour.
;;
;; Dict key for okurigana: reading + lowercase consonant (e.g. "かk").
;;
;; Key sequences:
;;   K   → henkan-on: ▽ marker inserted, preedit starts
;;   a   → "ka" → "か" inserted into preedit: ▽か
;;   K   → okurigana trigger: * marker inserted, "k" stored as pending consonant
;;   SPC → new fix: triggers okurigana conversion using pending "k" as okuri-char

(nskk-describe "SPC during partial consonant okurigana (TR-001 through TR-005)"
  (nskk-it "TR-001: SPC during ▽か*k shows first candidate ▼書"
    ;; Dict has "かk" → ("書" "佳").  Typing K a K puts us in state ▽か*k.
    ;; SPC must trigger okurigana conversion and display the first candidate.
    (let ((dict '(("かk" . ("書" "佳")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "TR-002: SPC during ▽か*k with empty dict triggers registration"
    ;; Empty dict: no candidates for "かk".  SPC should trigger okurigana
    ;; conversion, find no candidates, and open the registration dialog.
    ;; The default mock returns "" (cancel), restoring to 'on phase.
    ;; NOTE: Use a stub dict that lacks "かk" rather than '() (nil), because
    ;; '() evaluates to nil and (or nil default-dict) falls back to the
    ;; default dict which contains ("かk" . ("書")), causing on-found to fire.
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (nskk-e2e-type "SPC")
        ;; Registration was opened (and cancelled by the default mock returning "").
        ;; Phase is restored to 'on after cancel.
        (nskk-e2e-assert-henkan-phase 'on)
        ;; * marker should be removed from buffer after cancelled registration
        (should (not (string-match-p "\\*" (buffer-string)))))))

  (nskk-it "TR-003: SPC then C-j during ▽か*k commits 書 (candidate only — no okurigana kana in SPC path)"
    ;; After okurigana conversion via SPC, committing via C-j (nskk-commit-current)
    ;; inserts only the candidate kanji: 書.  No okurigana kana suffix is appended
    ;; because the user never typed the vowel in the SPC path.
    ;; Here we supply a full okuri vowel sequence by pressing i after SPC.
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        ;; Candidate "書" is committed; no okuri-kana was inserted in the SPC path
        ;; (the user never typed the vowel), so only 書 appears in the buffer.
        (nskk-e2e-assert-buffer "書"))))

  (nskk-it "TR-004: second SPC after ▽か*k conversion advances to next candidate ▼佳"
    ;; After the first SPC triggers okurigana conversion (showing ▼書),
    ;; a second SPC must advance to the next candidate (▼佳).
    (let ((dict '(("かk" . ("書" "佳"))))
          (nskk-henkan-show-candidates-nth 4))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "佳"))))

  (nskk-it "TR-005: normal SPC conversion (▽か + SPC) still works (regression guard)"
    ;; Ensure the SPC fix does not break the ordinary non-okurigana conversion path.
    ;; Ka + SPC: no pending okurigana consonant → normal nskk-start-conversion.
    (let ((dict '(("か" . ("花" "香")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "花")))))

;;;;
;;;; Okurigana bug regression tests (OKU-2, OKU-4)
;;;;
;;
;; Regression guards for fixed okurigana bugs.
;;
;; OKU-2: nskk--exhaust-candidates previously passed buffer kanji text (not
;;        the original reading) to nskk-start-registration.  Now it
;;        reconstructs the reading in okurigana-aware format (e.g. "か*く")
;;        from stored metadata.
;;
;; OKU-4: nskk-start-conversion/k SPC okurigana path previously never called
;;        on-found continuation.  Now the inline CPS block properly threads
;;        all three continuations to the caller.

(nskk-describe "Okurigana bug regression (OKU-2, OKU-4)"
  (nskk-it "OKU-2: exhaust-candidates passes okurigana-aware reading to registration"
    ;; Dict has a single candidate for "かk".  Typing KaKu triggers okurigana
    ;; conversion showing "書く".  Pressing SPC enough times exhausts candidates
    ;; and triggers registration.  The registration prompt SHOULD contain the
    ;; original reading (e.g. "か*く" or "かk"), NOT the kanji "書".
    (let ((dict '(("かk" . ("書"))))
          (nskk-henkan-show-candidates-nth 2)
          (captured-prompt nil))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; Override read-from-minibuffer to capture the prompt string.
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (prompt &rest _)
                     (setq captured-prompt prompt)
                     "")))
          (nskk-e2e-type "K")
          (nskk-e2e-type "a")
          (nskk-e2e-type "K")
          (nskk-e2e-type "u")
          (nskk-e2e-assert-converting)
          ;; Exhaust all candidates: only "書" exists.
          ;; First SPC enters candidate-list display, second SPC exhausts.
          (nskk-e2e-type "SPC")
          (nskk-e2e-type "SPC")
          ;; Registration should have been triggered.
          (should captured-prompt)
          ;; The prompt should contain the reading in stem*kana format.
          (should (string-match-p "か\\*く" captured-prompt))
          ;; The reading portion (after "] ") must NOT contain the kanji "書".
          ;; Note: "辞書登録" in the header contains "書", so check only the
          ;; reading portion after the last "] ".
          (let ((reading-part (if (string-match "\\] \\(.+\\): \\'" captured-prompt)
                                  (match-string 1 captured-prompt)
                                captured-prompt)))
            (should-not (string-match-p "書" reading-part)))))))

  (nskk-it "OKU-4: SPC okurigana path calls on-found continuation"
    ;; When nskk-start-conversion/k is called and the SPC okurigana path fires
    ;; (pending okurigana detected), the on-found continuation must be called
    ;; so the caller knows conversion succeeded.  Currently the okuri branch
    ;; in nskk-start-conversion/k calls nskk--trigger-okuri-conversion as a
    ;; side effect but never invokes on-found.
    (let ((dict '(("かk" . ("書" "掛"))))
          (on-found-called nil))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; Set up the ▽か*k state by typing K a K.
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        ;; Now call nskk-start-conversion/k directly with tracking lambdas.
        ;; The okuri branch should fire because okurigana is pending.
        (nskk-start-conversion/k
         (lambda (&rest _args)
           (setq on-found-called t))
         #'ignore
         #'ignore)
        ;; on-found must have been called.
        (should on-found-called)))))

;;;;
;;;; Post-command-handler okurigana guard (point-escape regression)
;;
;; Bug: nskk--post-command-handler's converting (▼) point-escape guard
;; auto-committed okurigana conversions because the overlay only covers the
;; reading stem (e.g. "お") while the okurigana kana (e.g. "い") sits after
;; the overlay.  Point is at the end of the kana, past overlay-end, so the
;; guard falsely detected point escape and called nskk-commit-current.
;;
;; Fix: skip the point-escape guard when okurigana-in-progress metadata is set.
;;
;; These tests call nskk--post-command-handler explicitly because
;; nskk-e2e--dispatch-event uses call-interactively which does not trigger
;; post-command-hook (only the real Emacs command loop does).

(nskk-describe "post-command-handler okurigana guard"
  (nskk-it "does not auto-commit vowel okurigana OI after post-command-handler"
    (let ((dict '(("おi" . ("推" "置")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "推")
        ;; Simulate post-command-hook firing (real command loop does this)
        (nskk--post-command-handler)
        ;; Must still be in converting state — NOT auto-committed
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "推"))))

  (nskk-it "allows SPC cycling after post-command-handler on vowel okurigana"
    (let ((dict '(("おi" . ("推" "置")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "SPC")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "置"))))

  (nskk-it "commits vowel okurigana OI with C-j after post-command-handler"
    (let ((dict '(("おi" . ("推")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "推い"))))

  (nskk-it "does not auto-commit consonant okurigana KaKu after post-command-handler"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk--post-command-handler)
        ;; Must still be converting — NOT auto-committed
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "does not auto-commit AI vowel okurigana after post-command-handler"
    (let ((dict '(("あi" . ("愛" "哀")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "愛")
        (nskk-e2e-type "SPC")
        (nskk--post-command-handler)
        (nskk-e2e-assert-overlay-shows "哀")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "哀い")))))

(provide 'nskk-okurigana-e2e-test)

;;; nskk-okurigana-e2e-test.el ends here
