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

;; E2E tests for AZIK extended romaji input style.
;; Tests q-key behavior (context-aware, always-n, toggle-only),
;; semicolon as っ, colon as ー, and other AZIK-specific rules.
;;
;; AZIK must be activated per test via nskk-e2e-with-azik-buffer macro.
;;
;; Architecture notes for test authors:
;;
;; 1. Semicolon and colon: In AZIK mode, ";" and ":" are mapped in the
;;    romaji hash table (nskk--romaji-table) via nskk-converter-load-style.
;;    They reach the hash via the standard nskk-self-insert path:
;;      nskk-self-insert -> nskk-process-japanese-input
;;        -> nskk-convert-input-to-kana -> nskk-converter-convert
;;        -> nskk-converter-lookup (reads hash).
;;    So nskk-e2e-type ";" and nskk-e2e-type ":" work correctly in
;;    AZIK mode via normal key dispatch.
;;
;; 2. Q-key AZIK behavior: The nskk-mode-map binds "q" to nskk-handle-q,
;;    which calls nskk-toggle-japanese-mode unconditionally (not AZIK-aware).
;;    The AZIK-aware nskk-handle-q-key is a separate function not bound in
;;    the default keymap.  E2E q-behavior tests therefore call
;;    nskk-handle-q-key directly inside nskk-e2e-with-azik-buffer, which
;;    accurately exercises the AZIK q dispatch path (Prolog query on
;;    q-key-action/4) while still operating in a full live NSKK buffer.
;;
;; 3. AZIK romaji rules (hatsuon, double vowel, youon): These reach the
;;    buffer via normal nskk-e2e-type key dispatch because nskk-self-insert
;;    handles all printable ASCII, and the AZIK-loaded hash table resolves
;;    multi-char romaji sequences (e.g., "kz" -> "かん").
;;
;; Test name format: nskk-e2e-azik-*

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-input)
(require 'nskk-azik)
(require 'nskk-converter)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; AZIK Buffer Helper Macro
;;;;

(defmacro nskk-e2e-with-azik-buffer (initial-mode dict-entries &rest body)
  "Like `nskk-e2e-with-buffer' but with AZIK romaji style activated.
INITIAL-MODE is the starting mode symbol (e.g., \\='hiragana) or nil.
DICT-ENTRIES is an alist of (reading . candidates-list) or nil for defaults.
BODY is executed inside the AZIK-enabled buffer environment.

Activation sequence:
  1. Set `nskk-converter-romaji-style' to \\='azik via let-binding.
  2. Call `nskk-converter-load-style' \\='azik to populate the romaji hash.
  3. Run `nskk-e2e-with-buffer' with INITIAL-MODE and DICT-ENTRIES.
  4. Restore the standard romaji table after BODY via unwind-protect.

This ensures:
  - AZIK rules (;->っ, :->ー, hatsuon, double-vowel, youon, etc.) are active.
  - `nskk-converter-romaji-style' is \\='azik so Prolog-dispatched functions
    (nskk-handle-q-key, nskk-handle-semicolon-key) also detect AZIK style.
  - Standard romaji table is restored after the test, preventing cross-test
    contamination."
  (declare (indent 2) (debug t))
  `(let ((nskk-converter-romaji-style 'azik))
     (nskk-converter-load-style 'azik)
     (unwind-protect
         (nskk-e2e-with-buffer ,initial-mode ,dict-entries
           ,@body)
       (nskk-converter-load-style 'standard))))

;;;;
;;;; Section 1: AZIK Semicolon as っ
;;;;

(nskk-describe "AZIK semicolon produces っ"

  (nskk-it "semicolon in AZIK hiragana mode inserts っ"
    ;; In AZIK mode, ";" is mapped to "っ" in the romaji hash table.
    ;; nskk-self-insert handles ";" via the standard romaji path and
    ;; converts it to っ immediately (no pending romaji prefix needed).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "っ")))

  (nskk-it "n then semicolon produces んっ in AZIK hiragana mode"
    ;; DDSKK AZIK: "n" is flushed as ん (n-consonant case) when followed by
    ;; ";".  The ";" is then immediately recognized as a complete AZIK rule
    ;; (っ), so both are emitted together: "んっ".
    ;; Fix: after flushing ん in n-consonant case, check the remaining char
    ;; against the hash; if it is complete, emit it too.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "n")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "んっ")))

  (nskk-it "semicolon in AZIK katakana mode inserts ッ"
    ;; In katakana mode, っ is converted to its katakana counterpart ッ.
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "ッ")))

  (nskk-it "multiple semicolons produce multiple っ in sequence"
    ;; Each ";" is independently converted to っ.
    ;; NOTE: (kbd ";;") returns an empty vector in edmacro notation because ";;"
    ;; is treated as a comment.  Dispatch two separate nskk-e2e-type calls instead.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer "っっ")))

  (nskk-it "semicolon followed by ka produces っか in AZIK hiragana mode"
    ;; っ is emitted from ";" immediately, then "ka" → か.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";ka")
      (nskk-e2e-assert-buffer "っか"))))

;;;;
;;;; Section 2: AZIK Colon as Long Vowel Mark (ー)
;;;;

(nskk-describe "AZIK colon produces ー"

  (nskk-it "colon in AZIK hiragana mode inserts ー"
    ;; In AZIK mode, ":" is mapped to "ー" in the romaji hash table.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ":")
      (nskk-e2e-assert-buffer "ー")))

  (nskk-it "ka followed by colon produces かー in AZIK hiragana mode"
    ;; "ka" converts to か first, then ":" → ー.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka:")
      (nskk-e2e-assert-buffer "かー")))

  (nskk-it "colon in AZIK katakana mode inserts ー"
    ;; ー is the same in both hiragana and katakana modes.
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-type ":")
      (nskk-e2e-assert-buffer "ー")))

  (nskk-it "ko followed by colon produces こー"
    ;; Verify "ko:" = こー (a common AZIK pattern for elongated vowels).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ko:")
      (nskk-e2e-assert-buffer "こー")))

  (nskk-it "colon after semicolon produces っー"
    ;; Sequence: ";" → っ, then ":" → ー.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";:")
      (nskk-e2e-assert-buffer "っー"))))

;;;;
;;;; Section 3: AZIK Hatsuon Extensions via Key Dispatch
;;;;

(nskk-describe "AZIK hatsuon extensions via key dispatch"

  (nskk-it "kz produces かん in AZIK hiragana mode"
    ;; "kz" is a single AZIK rule mapping to "かん".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kz")
      (nskk-e2e-assert-buffer "かん")))

  (nskk-it "sz produces さん in AZIK hiragana mode"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sz")
      (nskk-e2e-assert-buffer "さん")))

  (nskk-it "tk produces ちん in AZIK hiragana mode"
    ;; "tk" → ちん (t-row, i-position hatsuon).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "tk")
      (nskk-e2e-assert-buffer "ちん")))

  (nskk-it "hl produces ほん in AZIK hiragana mode"
    ;; DDSKK AZIK: "hl" → ほん (h-row, o-position hatsuon).
    ;; Fix: nskk-handle-l now checks whether pending-romaji+l is a complete
    ;; AZIK hash match (l-key-action/3 table, azik-complete buf-state).
    ;; When it is, it fires nskk-process-japanese-input instead of switching modes.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "hl")
      (nskk-e2e-assert-buffer "ほん")))

  (nskk-it "hatsuon sequence: kz + to produces かんと"
    ;; Demonstrates a realistic multi-rule input sequence.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kzto")
      (nskk-e2e-assert-buffer "かんと"))))

;;;;
;;;; Section 4: AZIK Double Vowel Extensions via Key Dispatch
;;;;

(nskk-describe "AZIK double vowel extensions via key dispatch"

  (nskk-it "kq produces かい in AZIK hiragana mode"
    ;; DDSKK AZIK: "kq" → かい (k-row, double-vowel i extension).
    ;; Fix: nskk-handle-q now delegates to nskk-handle-q-key, which detects
    ;; azik-complete buf-state (pending+q is a complete hash match) and fires
    ;; nskk-process-japanese-input instead of toggling mode.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kq")
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "kp produces こう in AZIK hiragana mode"
    ;; "kp" → こう (k-row, o+う diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kp")
      (nskk-e2e-assert-buffer "こう")))

  (nskk-it "tp produces とう in AZIK hiragana mode"
    ;; "tp" → とう (t-row, o+う diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "tp")
      (nskk-e2e-assert-buffer "とう")))

  (nskk-it "kw produces けい in AZIK hiragana mode"
    ;; "kw" → けい (k-row, e+い diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kw")
      (nskk-e2e-assert-buffer "けい")))

  (nskk-it "double vowel sequence: kp + ka produces こうか"
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kpka")
      (nskk-e2e-assert-buffer "こうか"))))

;;;;
;;;; Section 5: AZIK Q-Key Behavior — Context-Aware (Default)
;;;;
;;
;; NOTE: In the standard nskk-mode-map, "q" is bound to nskk-handle-q
;; (which always toggles mode via nskk-toggle-japanese-mode).  The AZIK-aware
;; nskk-handle-q-key (which queries q-key-action/4) is a separate function.
;; These tests call nskk-handle-q-key directly to exercise AZIK q dispatch.

(nskk-describe "AZIK q context-aware mode (default)"

  (nskk-it "q with empty romaji buffer toggles hiragana to katakana"
    ;; context-aware + empty buffer -> toggle-mode action.
    ;; nskk--romaji-buffer is "" (cleared by nskk-e2e-with-azik-buffer setup),
    ;; so the q-key-action query returns toggle-mode.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'context-aware))
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "q with empty romaji buffer toggles katakana to hiragana"
    ;; context-aware + empty buffer -> toggle-mode action (reverse).
    (nskk-e2e-with-azik-buffer 'katakana nil
      (let ((nskk-azik-q-behavior 'context-aware))
        (nskk-e2e-assert-mode 'katakana)
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "q completing an AZIK double-vowel rule produces the kana (not ん)"
    ;; context-aware: when pending-romaji+q forms a complete AZIK hash match,
    ;; the AZIK rule fires (fire-romaji action) instead of inserting ん.
    ;; "k" pending + "q" = "kq" → "かい" (AZIK double-vowel: k-row, q-position).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'context-aware))
        ;; Type "k" to put an incomplete romaji prefix in the buffer.
        (nskk-e2e-type "k")
        (nskk-e2e-assert-mode 'hiragana)
        ;; q-key: "kq" is in hash → azik-complete buf-state → fire-romaji.
        (nskk-handle-q-key)
        ;; Mode should NOT have changed.
        (nskk-e2e-assert-mode 'hiragana)
        ;; "kq" fired as AZIK double-vowel rule → "かい" in buffer.
        (nskk-e2e-assert-buffer "かい"))))

  (nskk-it "q with pending n romaji produces ない (AZIK nq rule)"
    ;; "n" in romaji buffer + "q" = "nq" → "ない" (AZIK double-vowel: n-row).
    ;; context-aware + azik-complete → fire-romaji → "ない".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'context-aware))
        ;; Type "n" to put "n" in the romaji buffer.
        (nskk-e2e-type "n")
        ;; "nq" is an AZIK rule → buf-state=azik-complete → fire-romaji.
        (nskk-handle-q-key)
        ;; Mode should NOT have changed.
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-assert-buffer "ない")))))

;;;;
;;;; Section 6: AZIK Q-Key Behavior — Always-N Mode
;;;;

(nskk-describe "AZIK q always-n mode"

  (nskk-it "q with empty romaji buffer produces ん (not toggle)"
    ;; always-n: q always inserts ん regardless of romaji buffer state.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'always-n))
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-handle-q-key)
        ;; Mode should NOT have changed (no toggle).
        (nskk-e2e-assert-mode 'hiragana)
        ;; ん should be in the buffer.
        (nskk-e2e-assert-buffer "ん"))))

  (nskk-it "q with pending romaji produces ん in always-n mode"
    ;; always-n: q inserts ん even when romaji buffer is non-empty.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'always-n))
        ;; Leave "k" pending in romaji buffer.
        (nskk-e2e-type "k")
        (nskk-handle-q-key)
        ;; Mode should still be hiragana.
        (nskk-e2e-assert-mode 'hiragana)
        ;; ん should be in the buffer.
        (nskk-e2e-assert-buffer "ん"))))

  (nskk-it "multiple q presses in always-n mode produce multiple ん"
    ;; Each q call inserts ん; no mode toggle occurs.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'always-n))
        (nskk-handle-q-key)
        (nskk-handle-q-key)
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-assert-buffer "んんん"))))

  (nskk-it "q in katakana always-n mode produces ん"
    ;; always-n works in katakana mode too (no toggle to hiragana).
    (nskk-e2e-with-azik-buffer 'katakana nil
      (let ((nskk-azik-q-behavior 'always-n))
        (nskk-e2e-assert-mode 'katakana)
        (nskk-handle-q-key)
        ;; Mode should remain katakana.
        (nskk-e2e-assert-mode 'katakana)
        ;; ん is inserted (not ン, because insert is literal \u3093).
        (nskk-e2e-assert-buffer "ん")))))

;;;;
;;;; Section 7: AZIK Q-Key Behavior — Toggle-Only Mode
;;;;

(nskk-describe "AZIK q toggle-only mode"

  (nskk-it "q with empty romaji buffer toggles hiragana to katakana"
    ;; toggle-only: q always toggles mode, like standard SKK.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'toggle-only))
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "q with pending romaji toggles mode in toggle-only mode"
    ;; toggle-only: q toggles even when there is pending romaji.
    ;; The romaji buffer state is ignored; mode toggle always fires.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'toggle-only))
        ;; Leave "k" pending in romaji buffer.
        (nskk-e2e-type "k")
        ;; Mode is still hiragana (k is an incomplete prefix).
        (nskk-e2e-assert-mode 'hiragana)
        ;; q in toggle-only mode: toggles to katakana regardless.
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'katakana)
        ;; Nothing was inserted in the buffer (no ん, no は).
        ;; The pending "k" romaji was cleared by mode switch.
        (nskk-e2e-assert-buffer ""))))

  (nskk-it "double q in toggle-only mode returns to original mode"
    ;; Two toggles return to the starting mode: hiragana -> katakana -> hiragana.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (let ((nskk-azik-q-behavior 'toggle-only))
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'katakana)
        (nskk-handle-q-key)
        (nskk-e2e-assert-mode 'hiragana)))))

;;;;
;;;; Section 8: AZIK Standard Romaji Compatibility in E2E Buffer
;;;;

(nskk-describe "AZIK standard romaji compatibility via key dispatch"

  (nskk-it "ka still produces か in AZIK mode"
    ;; AZIK is a superset of standard romaji; ka->か must still work.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "sha still produces しゃ in AZIK mode"
    ;; DDSKK AZIK: standard "sha" → しゃ is preserved alongside AZIK extensions.
    ;; Fix: nskk--azik-restore-standard-prefixes demotes AZIK complete rules
    ;; (e.g. "sh"→"すう") that are proper prefixes of longer entries ("sha"→"しゃ")
    ;; back to :incomplete, so the standard multi-char sequence remains reachable.
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
    ;; DDSKK AZIK: standard sokuon "kka" → っか is preserved.
    ;; Fix: sokuon check is moved before match in nskk--classify-romaji-input,
    ;; so doubled consonants trigger sokuon (っ) even when an AZIK two-char
    ;; rule (e.g. "kk"→"きん") would otherwise fire first.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kka")
      (nskk-e2e-assert-buffer "っか"))))

;;;;
;;;; Section 9: AZIK Youon Compatibility via Key Dispatch
;;;;

(nskk-describe "AZIK youon (g substitutes for y) via key dispatch"

  (nskk-it "kga produces きゃ in AZIK mode"
    ;; In AZIK, "g" substitutes for "y" in youon: kga -> きゃ.
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
    ;; kgp = きょ + う (youon base kgo=きょ, then p=う diphthong extension).
    ;; Rule: kgp -> きょう (youon diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kgp")
      (nskk-e2e-assert-buffer "きょう"))))

;;;;
;;;; Section 10: AZIK Word Shortcuts via Key Dispatch
;;;;

(nskk-describe "AZIK word shortcuts via key dispatch"

  (nskk-it "sr produces する in AZIK mode"
    ;; AZIK word shortcut: "sr" -> "する".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sr")
      (nskk-e2e-assert-buffer "する")))

  (nskk-it "ms produces ます in AZIK mode"
    ;; AZIK word shortcut: "ms" -> "ます".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ms")
      (nskk-e2e-assert-buffer "ます")))

  (nskk-it "mn produces もの in AZIK mode"
    ;; AZIK word shortcut: "mn" -> "もの".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "mn")
      (nskk-e2e-assert-buffer "もの")))

  (nskk-it "kt produces こと in AZIK mode"
    ;; AZIK word shortcut: "kt" -> "こと".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kt")
      (nskk-e2e-assert-buffer "こと"))))

;;;;
;;;; Section 11: AZIK Same-Finger Alternatives via Key Dispatch
;;;;

(nskk-describe "AZIK same-finger alternatives via key dispatch"

  (nskk-it "kf produces き in AZIK mode"
    ;; AZIK same-finger alternative: "kf" -> "き".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kf")
      (nskk-e2e-assert-buffer "き")))

  (nskk-it "rf produces る in AZIK mode"
    ;; AZIK same-finger alternative: "rf" -> "る".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "rf")
      (nskk-e2e-assert-buffer "る")))

  (nskk-it "yf produces ゆ in AZIK mode"
    ;; AZIK same-finger alternative: "yf" -> "ゆ".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "yf")
      (nskk-e2e-assert-buffer "ゆ"))))

;;;;
;;;; Section 12: AZIK Mixed Sequence Integration
;;;;

(nskk-describe "AZIK mixed sequence integration"

  (nskk-it "kztp produces かんとう (hatsuon + diphthong sequence)"
    ;; "kz" -> かん, "tp" -> とう. Combined: かんとう.
    ;; This is a realistic input sequence for 関東 reading.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kztp")
      (nskk-e2e-assert-buffer "かんとう")))

  (nskk-it "semicolon then ka then colon produces っかー"
    ;; Combines special keys: っ + か + ー.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type ";ka:")
      (nskk-e2e-assert-buffer "っかー")))

  (nskk-it "szpo produces さんぽ (hatsuon + standard romaji)"
    ;; "sz" -> さん, "po" -> ぽ.  Mixed AZIK + standard.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "szpo")
      (nskk-e2e-assert-buffer "さんぽ")))

  (nskk-it "kgpto produces きょうと via youon + diphthong + standard"
    ;; "kgp" -> きょう (youon kgo=きょ + diphthong p=う), "to" -> と.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "kgpto")
      (nskk-e2e-assert-buffer "きょうと")))

  (nskk-it "AZIK input in preedit and commit with C-j works correctly"
    ;; Uppercase K starts preedit (▽) with effective char k.
    ;; Then "a" completes the "ka" romaji -> か in preedit.
    ;; C-j commits the preedit kana directly to the buffer.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "か"))))

(provide 'nskk-azik-e2e-test)

;;; nskk-azik-e2e-test.el ends here
