;;; nskk-okuri-extended-e2e-test.el --- E2E tests for extended okurigana patterns  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))

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
;;;; Section 4: Katakana Mode Okurigana
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

(nskk-describe "katakana mode okurigana"
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

(provide 'nskk-okuri-extended-e2e-test)

;;; nskk-okuri-extended-e2e-test.el ends here
