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
;; Tests q-key behavior (context-aware only),
;; toggle key behavior (@ for jp106, [ for us101),
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
;; 2. Q-key AZIK behavior: The nskk-mode-map binds "q" to nskk-handle-q.
;;    In AZIK mode, nskk-handle-q delegates to nskk-handle-q-key (which
;;    queries q-key-action/3) both in idle Japanese mode and in ▽ preedit
;;    mode.  In standard mode it calls nskk-henkan-kakutei-convert-script
;;    (script toggle) in ▽ preedit, or nskk-toggle-japanese-mode in idle.
;;    Section 5 tests call nskk-handle-q-key directly (idle mode) for
;;    focused AZIK dispatch testing; Section 12 tests use nskk-e2e-type "q"
;;    to cover the full nskk-handle-q dispatch path in ▽ preedit.
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
(require 'nskk-pbt-generators)
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
  `(let* ((nskk-converter-romaji-style 'azik)
          ;; Save @/[ binding before AZIK init modifies nskk-mode-map.
          ;; The toggle key is determined by nskk-azik-keyboard-type (default "@").
          (nskk--azik-e2e-toggle-key
           (if (and (boundp 'nskk-azik-keyboard-type)
                    (eq nskk-azik-keyboard-type 'us101)) "[" "@"))
          (nskk--azik-e2e-saved-binding
           (when (boundp 'nskk-mode-map)
             (lookup-key nskk-mode-map nskk--azik-e2e-toggle-key))))
     (nskk-converter-load-style 'azik)
     (unwind-protect
         (nskk-e2e-with-buffer ,initial-mode ,dict-entries
           ,@body)
       (nskk-converter-load-style 'standard)
       ;; Retract azik-rule/2 facts explicitly.  nskk-converter-load-style only
       ;; retracts romaji-to-kana/2; azik-rule/2 facts persist if not cleared.
       ;; Without this, the global Prolog DB retains AZIK facts after teardown,
       ;; polluting subsequent unit tests that expect an empty azik-rule/2 table.
       (nskk-prolog-retract-all 'azik-rule 2)
       ;; Restore the AZIK toggle key binding to prevent cross-test contamination.
       ;; Without this, @ remains bound to nskk-toggle-japanese-mode globally,
       ;; causing subsequent non-AZIK tests to drop @ characters.
       (when (boundp 'nskk-mode-map)
         (if nskk--azik-e2e-saved-binding
             (keymap-set nskk-mode-map nskk--azik-e2e-toggle-key
                         nskk--azik-e2e-saved-binding)
           (keymap-unset nskk-mode-map nskk--azik-e2e-toggle-key t))))))

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
;;;; Section 5: AZIK Q-Key Behavior — Context-Aware Mode
;;;;
;;
;; NOTE: In the standard nskk-mode-map, "q" is bound to nskk-handle-q
;; (which always toggles mode via nskk-toggle-japanese-mode).  The AZIK-aware
;; nskk-handle-q-key (which queries q-key-action/3) is a separate function.
;; These tests call nskk-handle-q-key directly to exercise AZIK q dispatch.
;;
;; Context-aware q-key behavior:
;; - Empty buffer + q → insert ん (hiragana) or ン (katakana)
;; - Pending romaji + q where AZIK match exists → fire AZIK rule (e.g., kq → かい)
;; - Pending romaji + q where no AZIK match → insert ん

(nskk-describe "AZIK q context-aware mode (default)"

  (nskk-it "q with empty romaji buffer inserts ん in hiragana mode"
    ;; context-aware + empty buffer -> insert-n action.
    ;; Standalone q produces ん without toggling mode.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ん")))

  (nskk-it "q with empty romaji buffer inserts ン in katakana mode"
    ;; context-aware + empty buffer -> insert-n action.
    ;; In katakana mode, ん is automatically converted to ン.
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-handle-q-key)
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-assert-buffer "ン")))

  (nskk-it "q completing an AZIK double-vowel rule produces the kana (not ん)"
    ;; context-aware: when pending-romaji+q forms a complete AZIK hash match,
    ;; the AZIK rule fires (fire-romaji action) instead of inserting ん.
    ;; "k" pending + "q" = "kq" → "かい" (AZIK double-vowel: k-row, q-position).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      ;; Type "k" to put an incomplete romaji prefix in the buffer.
      (nskk-e2e-type "k")
      (nskk-e2e-assert-mode 'hiragana)
      ;; q-key: "kq" is in hash → azik-complete buf-state → fire-romaji.
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      ;; "kq" fired as AZIK double-vowel rule → "かい" in buffer.
      (nskk-e2e-assert-buffer "かい")))

  (nskk-it "q with pending n romaji produces ない (AZIK nq rule)"
    ;; "n" in romaji buffer + "q" = "nq" → "ない" (AZIK double-vowel: n-row).
    ;; context-aware + azik-complete → fire-romaji → "ない".
    (nskk-e2e-with-azik-buffer 'hiragana nil
      ;; Type "n" to put "n" in the romaji buffer.
      (nskk-e2e-type "n")
      ;; "nq" is an AZIK rule → buf-state=azik-complete → fire-romaji.
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ない")))

  (nskk-it "fq produces ふぁい in AZIK hiragana mode"
    ;; "fq" → ふぁい (f-row, a+い diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "f")
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ふぁい")))

  (nskk-it "jq produces じゃい in AZIK hiragana mode"
    ;; "jq" → じゃい (j-row, a+い diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "j")
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "じゃい")))

  (nskk-it "vq produces ゔぁい in AZIK hiragana mode"
    ;; "vq" → ゔぁい (v-row, a+い diphthong).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "v")
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "ゔぁい")))

  (nskk-it "q with pending non-AZIK romaji produces ん"
    ;; When pending romaji + q does NOT form an AZIK rule, q inserts ん.
    ;; Example: "l" + "q" has no AZIK rule, so q acts as standalone ん.
    ;; NOTE: "l" cannot be typed via nskk-e2e-type because it is bound to
    ;; nskk-handle-l (latin-mode switch).  Set the romaji buffer directly to
    ;; simulate a pending "l" without triggering the mode-switch side-effect.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      ;; Directly put "l" in the romaji buffer (no lq AZIK rule exists).
      (setq nskk--romaji-buffer "l")
      (nskk-handle-q-key)
      ;; Mode should NOT have changed.
      (nskk-e2e-assert-mode 'hiragana)
      ;; "l" is discarded, q produces ん.
      (nskk-e2e-assert-buffer "ん"))))

;;;;
;;;; Section 6: AZIK Toggle Key Behavior (@ and [)
;;;;

(nskk-describe "AZIK toggle key behavior"

  (nskk-it "@ key toggles hiragana to katakana (jp106 keyboard)"
    ;; On jp106 keyboard, @ is the toggle key.
    ;; It should toggle mode regardless of AZIK state.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "@ key toggles katakana to hiragana (jp106 keyboard)"
    ;; Toggle should work in both directions.
    (nskk-e2e-with-azik-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "[ key toggles hiragana to katakana (us101 keyboard)"
    ;; On us101 keyboard, [ is the toggle key.
    ;; It should toggle mode regardless of AZIK state.
    ;; NOTE: Must set nskk-azik-keyboard-type to 'us101 and rebind toggle key.
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'katakana))))

  (nskk-it "[ key toggles katakana to hiragana (us101 keyboard)"
    ;; Toggle should work in both directions.
    ;; NOTE: Must set nskk-azik-keyboard-type to 'us101 and rebind toggle key.
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'katakana nil
        (nskk-e2e-assert-mode 'katakana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "@ toggle on jp106 clears pending romaji and toggles mode"
    ;; When there is pending romaji, @ toggle key should still toggle mode
    ;; and clear the pending romaji buffer.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "k")  ; pending romaji
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "@")
      (nskk-e2e-assert-mode 'katakana)
      ;; Pending "k" should be cleared
      (nskk-e2e-assert-buffer "")))

  (nskk-it "bracket toggle on us101 clears pending romaji and toggles mode"
    ;; When there is pending romaji, [ toggle key should still toggle mode
    ;; and clear the pending romaji buffer.
    ;; NOTE: Must set nskk-azik-keyboard-type to 'us101 and rebind toggle key.
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk--setup-azik-toggle-key)
      (nskk-e2e-with-azik-buffer 'hiragana nil
        (nskk-e2e-type "k")  ; pending romaji
        (nskk-e2e-assert-mode 'hiragana)
        (nskk-e2e-type "[")
        (nskk-e2e-assert-mode 'katakana)
        ;; Pending "k" should be cleared
        (nskk-e2e-assert-buffer "")))))

;;;;
;;;; Section 7: AZIK Standard Romaji Compatibility in E2E Buffer
;;;;

(nskk-describe "AZIK standard romaji compatibility via key dispatch"

  (nskk-it "ka still produces か in AZIK mode"
    ;; AZIK is a superset of standard romaji; ka->か must still work.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "sh produces すう in AZIK mode (vowel-shadow deferred)"
    ;; Bug fix: "sh" was incorrectly demoted to :incomplete, making Sh→すう impossible.
    ;; Now "sh" is vowel-only-shadowed: kept complete, using azik-vowel-deferred
    ;; mechanism so sh→すう works while sha/shi/shu/she/sho remain reachable.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "sh")
      (nskk-e2e-assert-buffer "すう")))

  (nskk-it "sha still produces しゃ in AZIK mode"
    ;; DDSKK AZIK: standard "sha" → しゃ is preserved alongside AZIK extensions.
    ;; azik-vowel-deferred: "sh" emits "すう" tentatively; next char "a" (vowel)
    ;; triggers retroactive correction: delete "すう", reset buffer to "sh",
    ;; then "sha" → "しゃ".
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
;;;; Section 8: AZIK Youon Compatibility via Key Dispatch
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
    ;; ngz = ng youon prefix (にゃ base) + z hatsuon suffix (a+ん) = にゃん
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngz")
      (nskk-e2e-assert-buffer "にゃん")))

  (nskk-it "ngq produces にゃい via ng youon + double vowel in AZIK mode"
    ;; ngq = ng youon prefix (にゃ base) + q double-vowel suffix (a+い) = にゃい
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ngq")
      (nskk-e2e-assert-buffer "にゃい"))))

;;;;
;;;; Section 9: AZIK Word Shortcuts via Key Dispatch
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
      (nskk-e2e-assert-buffer "こと")))

  (nskk-it "ss produces せい in AZIK mode"
    ;; AZIK same-key double: "ss" -> "せい" (same as sw, ddskk compatible).
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ss")
      (nskk-e2e-assert-buffer "せい")))

  (nskk-it "ssa produces っさ via azik-deferred retroactive correction"
    ;; azik-deferred: "ss" emits "せい" tentatively; next char "a" (vowel)
    ;; triggers retroactive correction: delete "せい", emit "っ", then process "sa" -> さ.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "ssa")
      (nskk-e2e-assert-buffer "っさ"))))

;;;;
;;;; Section 10: AZIK Same-Finger Alternatives via Key Dispatch
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
;;;; Section 11: AZIK Mixed Sequence Integration
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

;;;;
;;;; Property-Based Tests: AZIK Hatsuon Known Pairs
;;;;

(nskk-deftest-cases azik-hatsuon-pairs
  (("kz" . "かん")
   ("sz" . "さん")
   ("tz" . "たん"))
  :body
  (nskk-e2e-with-azik-buffer 'hiragana nil
    (nskk-e2e-type input)
    (nskk-e2e-assert-buffer expected)))

;;;;
;;;; Property-Based Tests: AZIK Double-Vowel Rules Table
;;;;

;; Double-vowel input in AZIK uses standard romaji vowel keys pressed twice.
;; Extension keys (q/h/w/p) act as consonant suffixes for diphthongs, not
;; standalone vowel triggers.
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
          (should (> (length (buffer-string)) 0)))))))

;;;
;;; AZIK @ Key in ▽ Preedit — Script Conversion (DDSKK-compatible)
;;;

(nskk-describe "AZIK @ key in ▽ preedit: script conversion"
  (nskk-it "hiragana preedit + @ → katakana committed, mode stays hiragana"
    ;; In AZIK mode the @ key is bound to nskk-toggle-japanese-mode.
    ;; When in ▽ preedit phase it must call nskk-henkan-kakutei-convert-script
    ;; (DDSKK-compatible: converts preedit script without toggling mode).
    ;; Use "Kana" (かな) to avoid AZIK nj→ぬん rule that fires for "Kanji".
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
;;;; Section 12: AZIK q Key in ▽ Preedit — Romaji Priority over Script Conversion
;;;;
;;
;; Regression tests for: typing q in AZIK ▽ preedit mode was calling
;; nskk-henkan-kakutei-convert-script (script toggle), which cleared ▽ mode.
;;
;; Root cause: nskk-handle-q short-circuited to nskk-henkan-kakutei-convert-script
;; for any preedit-japanese state without checking AZIK mode first.
;;
;; Fix: in nskk-handle-q, when preedit-japanese AND AZIK mode, delegate to
;; nskk-handle-q-key (which handles azik-complete→fire-romaji, and
;; empty/pending→insert-ん).  Standard mode retains the original script-toggle.
;;
;; In AZIK, the toggle key is @ (jp106) or [ (us101) — q is purely a romaji key.

(nskk-describe "AZIK q key in ▽ preedit: romaji rules take priority over script conversion"

  (nskk-it "Zdtq: ▽ mode stays active after q fires tq→たい rule"
    ;; Regression: "Zdtq" caused ▽ mode to change unexpectedly.
    ;; Z → ▽ with z pending; zd → ぜん (z-row hatsuon, d=E suffix);
    ;; t → t pending; q → in AZIK preedit fires tq→たい (NOT script toggle).
    ;; ▽ mode must remain active; mode must stay hiragana.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Zdtq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Zdtq")
      (nskk-e2e-assert-mode 'hiragana "mode must not change after q in AZIK ▽ preedit")))

  (nskk-it "Katq: q fires tq→たい in ▽ preedit, stays in ▽"
    ;; Ka → ▽ か (ka complete); t → t pending; q → tq→たい fired.
    ;; ▽ mode must remain active; mode must stay hiragana.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Katq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Katq")
      (nskk-e2e-assert-mode 'hiragana "mode must not change after tq in AZIK ▽ preedit")))

  (nskk-it "Kaq: standalone q in ▽ preedit inserts ん (AZIK empty-q rule), stays in ▽"
    ;; Ka → ▽ か; q → romaji buffer empty → AZIK empty-q → insert ん.
    ;; ▽ mode must remain active; mode must stay hiragana.
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (nskk-e2e-type "Kaq")
      (nskk-e2e-assert-henkan-phase 'on "▽ must remain active after Kaq")
      (nskk-e2e-assert-mode 'hiragana "standalone q in AZIK ▽ preedit must not toggle mode"))))

;;;;
;;;; Section 13: AZIK Okurigana with AZIK Kana Shortcuts — Double-* Regression
;;;;
;;
;; Regression tests for: typing XhSS or TukaTTe in AZIK ▽ preedit was inserting
;; a spurious second okurigana marker (*), producing e.g. 'しゅう**ss' or
;; 'つか**te' instead of triggering the AZIK kana shortcut as okurigana kana.
;;
;; Root cause: nskk--compute-effective-char's normalize-vowel-p predicate did
;; not account for the case where okurigana is already pending.  When okurigana
;; state was non-nil (first uppercase consonant had set okurigana), the second
;; uppercase consonant was treated as a new okurigana boundary trigger instead
;; of a plain romaji continuation.
;;
;; Fix: added condition (c) to normalize-vowel-p — when nskk-state-get-okurigana
;; is non-nil AND the romaji buffer is non-empty, uppercase chars are normalised
;; to lowercase (bypassing the okurigana trigger), allowing AZIK doubled-consonant
;; shortcuts (ss→せい, tt→たち→azik-deferred→っ+vowel) to fire correctly.
;;
;; The TukaTTe case exercises the same fix for the tt→azik-deferred path: the
;; second T (uppercase) must be normalised to lowercase, giving input "tt" which
;; fires AZIK's deferred sokuon mechanism.  A following vowel 'e' then retroactively
;; corrects the tentative "たち" to "っ" and appends "て" → okurigana kana "って".

(nskk-describe "AZIK okurigana with AZIK kana shortcuts: double-* regression"

  (nskk-it "XhSS triggers conversion (no spurious double * marker)"
    ;; Regression guard: the second S must NOT start a fresh okurigana boundary.
    ;; Xh→▽しゅう (AZIK x-row + h extension), first S→okurigana ?s (sets *),
    ;; second S→romaji "ss"→AZIK せい→okurigana kana→conversion fires with "しゅうs".
    (let ((dict '(("しゅうs" . ("修正")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "XhSS")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "修正"))))

  (nskk-it "XhSS commits to 修正せい after C-j"
    ;; Full end-to-end: preedit → okurigana conversion → commit appends okurigana kana.
    (let ((dict '(("しゅうs" . ("修正")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "XhSS")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "修正せい"))))

  (nskk-it "TukaTTe triggers conversion showing 使 (no spurious double * marker)"
    ;; Regression guard for Tuka+Te → 使って.
    ;; When shift is held across the okurigana boundary (TukaTTe instead of TukaTte),
    ;; the second uppercase T must be treated as lowercase (condition c in
    ;; normalize-vowel-p), giving input "tt".  AZIK "tt"→"たち" fires azik-deferred;
    ;; the following 'e' retroactively corrects to っ+て = って as okurigana kana.
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

  (nskk-it "TukaT;te triggers conversion showing 使 (AZIK ; sokuon in okurigana)"
    ;; When the user types ; (AZIK sokuon = っ) after the okurigana trigger T,
    ;; the pending 't' consonant in the romaji buffer must not absorb ';'.
    ;; The fix: non-alphabetic chars with a standalone complete match flush the
    ;; pending buffer and reprocess — so ';'→っ fires and triggers okuri conversion.
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type "T")
        (nskk-e2e-type ";")
        (nskk-e2e-type "t")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "使"))))

  (nskk-it "TukaT;te commits to 使って after C-j"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type "T")
        (nskk-e2e-type ";")
        (nskk-e2e-type "t")
        (nskk-e2e-type "e")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "使って"))))

  (nskk-it "Tuka:te triggers conversion showing 使 (AZIK Shift+; sokuon okurigana)"
    ;; In AZIK mode, `:' (Shift+;) in preedit acts as a combined okurigana +
    ;; sokuon trigger.  Tuka→▽つか, then `:' inserts * and arms the colon-okuri
    ;; trigger, `t' fires the dict lookup at preedit-end (after `*') with query
    ;; "つかt", and the following `e' retroactively inserts っ before te→て,
    ;; giving okurigana kana "って".
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type ":")
        (nskk-e2e-type "t")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "使"))))

  (nskk-it "Tuka:te commits to 使って after C-j"
    (let ((dict '(("つかt" . ("使")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Tuka")
        (nskk-e2e-type ":")
        (nskk-e2e-type "t")
        (nskk-e2e-type "e")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "使って")))))

;;;;
;;;; Section 14: Okurigana Conversion in AZIK Mode
;;;;
;;
;; Tests that okurigana input works correctly when AZIK romaji style is active.
;; Covers: basic consonant okurigana, multi-candidate cycling, commit,
;; vowel okurigana, AZIK-specific combos (hatsuon, double-vowel, same-finger,
;; youon), SPC during partial okurigana, and KaKi cycling regression guard.
;;
;; All tests use nskk-e2e-with-azik-buffer with test-specific inline dicts.

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
    ;; K starts henkan AND feeds "k" to romaji; z completes kz→かん;
    ;; K triggers okurigana (consonant "k"); u completes く → conversion fires.
    ;; Dict key: "かんk"
    (let ((dict '(("かんk" . ("換")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "z")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "換"))))

  (nskk-it "AZIK double-vowel kp + okurigana Ku: KpKu shows 耕"
    ;; K starts henkan AND feeds "k" to romaji; p completes kp→こう;
    ;; K triggers okurigana (consonant "k"); u completes く → conversion fires.
    ;; Dict key: "こうk"
    (let ((dict '(("こうk" . ("耕")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "p")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "耕"))))

  (nskk-it "AZIK same-finger hf + okurigana Ku: HfKu shows 吹"
    ;; H starts henkan AND feeds "h" to romaji; f completes hf→ふ;
    ;; K triggers okurigana (consonant "k"); u completes く → conversion fires.
    ;; Dict key: "ふk"
    (let ((dict '(("ふk" . ("吹")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "H")
        (nskk-e2e-type "f")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "吹"))))

  (nskk-it "AZIK youon kga + okurigana Ru: KgaRu shows 嫌"
    ;; K starts henkan AND feeds "k" to romaji; ga completes kga→きゃ;
    ;; R triggers okurigana (consonant "r"); u completes る → conversion fires.
    ;; Dict key: "きゃr"
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
    ;; SPC pressed with pending consonant R (no vowel typed yet).
    ;; Must trigger okurigana conversion using pending "r" as okuri-char.
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
    ;; Regression guard: verifies multi-candidate cycling works for okurigana
    ;; with vowel "i" (not just "u").  Dict key is "かk" shared across vowels.
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
;;
;; Regression tests for: typing YoNN in AZIK mode must trigger okurigana
;; conversion with ん (nn-double), producing 読ん — NOT inserting a spurious
;; second * marker to produce よ*ん*.
;;
;; Root cause (when auto-start-henkan=nil): the second uppercase N was
;; classified as `normal' (not `normalize-vowel'), so it reached
;; nskk-process-okurigana-input which triggered a SECOND okurigana boundary.
;;
;; Fix: (1) nskk-process-okurigana-input/k now skips when okurigana is already
;; pending.  (2) nskk--process-normal-japanese-input downcases the char when
;; okurigana is pending, ensuring the romaji converter sees "nn" not "nN".

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
    ;; Regression guard: without the okurigana re-entry guard, the second N
    ;; would trigger nskk-process-okurigana-input again, flushing romaji "n"
    ;; as ん and inserting a second * marker → よ*ん*.
    ;; With the fix, second N completes nn→ん in the okurigana zone.
    (let ((dict '(("dummy" . ("dummy")))))
      (nskk-e2e-with-azik-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "N")
        (nskk-e2e-type "N")
        ;; No candidates for "よn" → falls to registration.
        ;; The key assertion: buffer must NOT contain two * markers.
        (let ((buf (buffer-string)))
          (should-not (string-match-p "\\*.*\\*" buf)))))))

;;;;
;;;; Section 16: JP106 Keyboard Colon-Okurigana via + Key
;;;;
;;
;; On JP106 keyboards, Shift+; produces `+' (not `:'). The AZIK colon-okurigana
;; trigger must recognize `+' as equivalent to `:' when nskk-azik-keyboard-type
;; is jp106.  Without this, JP106 users cannot use the colon-okurigana feature
;; at all (e.g. Nao+ta → 治った would fail).

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
          ;; + should NOT arm colon-okurigana on US101
          (should-not nskk--azik-colon-okuri-pending))))))

;;;;
;;;; Section 17: JP106 + Key Immediate Sokuon Okurigana
;;;;
;;
;; On JP106 keyboards, + (Shift+;) in henkan preedit acts as an immediate
;; okurigana trigger with sokuon っ: flushes romaji, inserts * marker,
;; inserts っ, and triggers okurigana conversion with okuri-char=?t.
;; In idle mode (outside henkan preedit), + produces っ via AZIK romaji rules.

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
        ;; + should NOT produce っ on US101 (no romaji rule for +)
        (should-not (string-match-p "っ" (buffer-string))))))

  (nskk-it "Oku:te still works (colon-okurigana path preserved)"
    (let ((dict '(("おくt" . ("送")))))
      (let ((nskk-azik-keyboard-type 'jp106))
        (nskk-e2e-with-azik-buffer 'hiragana dict
          (nskk-e2e-type "Oku")
          (nskk-e2e--dispatch-event ?:)
          (nskk-e2e-type "te")
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-buffer "送って")))))

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
        (should-not (string-match-p "error" (downcase (buffer-string))))))))

(provide 'nskk-azik-e2e-test)

;;; nskk-azik-e2e-test.el ends here
