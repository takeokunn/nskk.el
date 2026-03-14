;;; nskk-azik-test.el --- Tests for AZIK extended romaji input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 takeokunn
;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: Japanese, input, method, test, azik
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides comprehensive tests for the AZIK extended romaji input
;; support in NSKK. AZIK is an efficient Japanese input method that extends
;; standard romaji with special key combinations.
;;
;; Test categories:
;; - Style switching tests
;; - Special keys tests
;; - 撥音拡張 (mora nasal extension) tests
;; - 二重母音拡張 (diphthong extension) tests
;; - 拗音互換キー (yō-on compatibility key) tests
;; - 同指打鍵互換キー (same-finger compatibility key) tests
;; - 特殊拡張 (special extension) tests
;; - Q-key behavior tests
;; - Compatibility tests
;; - Prolog predicate tests (azik-vowel-char/1, azik-key-extends/2,
;;   azik-nonvowel-ext/1, azik-vowel-shadow/1, azik-rule/2, bridge rule,
;;   azik-colon-trigger-char/1)
;; - CPS function tests (nskk--azik-classify-key/k, nskk--azik-finalize-hash-table)

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-converter)
(require 'nskk-azik)
(require 'nskk-prolog)
(require 'nskk-pbt-generators)


;;;;
;;;; Helper Macros for AZIK Tests
;;;;

(defmacro nskk-with-azik-style (&rest body)
  "Execute BODY with AZIK style loaded."
  (declare (indent 0) (debug t))
  `(nskk-prolog-test-with-isolated-db
     (let* ((nskk--saved-romaji-table (copy-hash-table nskk--romaji-table))
            (nskk--azik-toggle-key
             (if (and (boundp 'nskk-azik-keyboard-type)
                      (eq nskk-azik-keyboard-type 'us101)) "[" "@"))
            (nskk--azik-saved-binding
             (when (boundp 'nskk-mode-map)
               (lookup-key nskk-mode-map nskk--azik-toggle-key))))
       (unwind-protect
           (progn
             (nskk-converter-load-style 'azik)
             ,@body)
         (clrhash nskk--romaji-table)
         (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                  nskk--saved-romaji-table)
         ;; Restore the AZIK toggle key binding to prevent cross-test contamination.
         (when (boundp 'nskk-mode-map)
           (if nskk--azik-saved-binding
               (keymap-set nskk-mode-map nskk--azik-toggle-key
                           nskk--azik-saved-binding)
             (keymap-unset nskk-mode-map nskk--azik-toggle-key t)))))))

(defmacro nskk-with-standard-style (&rest body)
  "Execute BODY with standard style loaded, restoring Prolog DB after."
  (declare (indent 0) (debug t))
  `(nskk-prolog-test-with-isolated-db
     (let ((nskk--saved-romaji-table (copy-hash-table nskk--romaji-table)))
       (unwind-protect
           (progn
             (nskk-converter-load-style 'standard)
             ,@body)
         (clrhash nskk--romaji-table)
         (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                  nskk--saved-romaji-table)))))


;;;;
;;;; 1. Style Switching Tests
;;;;

(nskk-describe "AZIK style switching"
  (nskk-it "loading standard style returns standard and provides basic romaji"
    (nskk-prolog-test-with-isolated-db
      (should (eq (nskk-converter-load-style 'standard) 'standard))
      ;; Standard style should have basic romaji
      (should (equal (nskk-convert-romaji "ka") "か"))
      (should (equal (nskk-convert-romaji "shi") "し"))))

  (nskk-it "loading AZIK style returns azik and provides extended rules"
    ;; Use nskk-with-azik-style to save/restore nskk--romaji-table in addition
    ;; to the Prolog DB, preventing AZIK hash table entries from persisting.
    (nskk-with-azik-style
      ;; Verify the return value by reloading AZIK (idempotent — clears and reloads)
      (should (eq (nskk-converter-load-style 'azik) 'azik))
      ;; AZIK style should have extended rules
      (should (equal (nskk-convert-romaji "kz") "かん"))
      (should (equal (nskk-convert-romaji "kq") "かい"))))

  (nskk-it "can switch between standard and AZIK styles"
    (nskk-prolog-test-with-isolated-db
      ;; Load standard
      (nskk-converter-load-style 'standard)
      (should (equal (nskk-convert-romaji "ka") "か"))
      ;; In standard, "kz" should not convert to "かん"
      (should-not (equal (nskk-convert-romaji "kz") "かん"))
      ;; Switch to AZIK
      (nskk-converter-load-style 'azik)
      (should (equal (nskk-convert-romaji "kz") "かん"))
      ;; Switch back to standard
      (nskk-converter-load-style 'standard)
      (should (equal (nskk-convert-romaji "ka") "か"))))

  (nskk-it "nskk-azik-keyboard-type us101 uses [ as toggle key"
    ;; Use nskk-with-azik-style to save/restore nskk--romaji-table.
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-with-azik-style
        ;; With us101, the toggle key should be "[", not "@"
        (when (boundp 'nskk-mode-map)
          (should (lookup-key nskk-mode-map "[")))))))




;;;;
;;;; 2. Special Keys Tests
;;;;

(nskk-describe "AZIK special keys unit"
  (nskk-it "semicolon produces small tsu in AZIK mode"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji ";") "っ"))))

  (nskk-it "colon produces chouon (long vowel mark) in AZIK mode"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji ":") "ー"))))

  (nskk-it "special keys work in context with other characters"
    (nskk-with-azik-style
      ;; "っか" using semicolon
      (should (equal (nskk-convert-romaji ";ka") "っか"))
      ;; "かー" using colon
      (should (equal (nskk-convert-romaji "ka:") "かー")))))


;;;;
;;;; 3. 撥音拡張 (Mora Nasal Extension) Tests
;;;;

(nskk-describe "AZIK hatsuon (撥音拡張) rules"
  ;; NOTE: Tests below use `nskk-convert-romaji' (full pipeline) to verify
  ;; end-to-end behavior.  The `nskk-deftest-table' blocks at the bottom of
  ;; this file use `nskk-converter-lookup' (hash-only) to test rule existence
  ;; directly, bypassing pipeline rules that would interfere (e.g., the n-row
  ;; triggers the special ん rule; the z/d rows trigger double-consonant rules).
  ;; Both layers are intentionally tested independently.
  (nskk-context "k-row hatsuon"
    (nskk-it "kz kk kj kd kl convert to かん きん くん けん こん"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "kz") "かん"))
        (should (equal (nskk-converter-lookup "kk") "きん"))
        (should (equal (nskk-converter-lookup "kj") "くん"))
        (should (equal (nskk-converter-lookup "kd") "けん"))
        (should (equal (nskk-converter-lookup "kl") "こん"))
        ;; kz, kk, kj, kd, kl should work in conversion
        (should (equal (nskk-convert-romaji "kz") "かん"))
        (should (equal (nskk-convert-romaji "kk") "きん"))
        (should (equal (nskk-convert-romaji "kj") "くん"))
        (should (equal (nskk-convert-romaji "kd") "けん"))
        (should (equal (nskk-convert-romaji "kl") "こん")))))

  (nskk-context "s-row hatsuon"
    (nskk-it "sz sk sj sd sl convert to さん しん すん せん そん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "sz") "さん"))
        (should (equal (nskk-convert-romaji "sk") "しん"))
        (should (equal (nskk-convert-romaji "sj") "すん"))
        (should (equal (nskk-convert-romaji "sd") "せん"))
        (should (equal (nskk-convert-romaji "sl") "そん")))))

  (nskk-context "t-row hatsuon"
    (nskk-it "tz tk tj td tl convert to たん ちん つん てん とん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "tz") "たん"))
        (should (equal (nskk-convert-romaji "tk") "ちん"))
        (should (equal (nskk-convert-romaji "tj") "つん"))
        (should (equal (nskk-convert-romaji "td") "てん"))
        (should (equal (nskk-convert-romaji "tl") "とん")))))

  (nskk-context "n-row hatsuon"
    (nskk-it "nz nk nj nd nl rules exist in the lookup table"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "nz") "なん"))
        (should (equal (nskk-converter-lookup "nk") "にん"))
        (should (equal (nskk-converter-lookup "nj") "ぬん"))
        (should (equal (nskk-converter-lookup "nd") "ねん"))
        (should (equal (nskk-converter-lookup "nl") "のん")))))

  (nskk-context "h-row hatsuon"
    (nskk-it "hz hk hj hd hl convert to はん ひん ふん へん ほん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "hz") "はん"))
        (should (equal (nskk-convert-romaji "hk") "ひん"))
        (should (equal (nskk-convert-romaji "hj") "ふん"))
        (should (equal (nskk-convert-romaji "hd") "へん"))
        (should (equal (nskk-convert-romaji "hl") "ほん")))))

  (nskk-context "m-row hatsuon"
    (nskk-it "mz mk mj md ml convert to まん みん むん めん もん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "mz") "まん"))
        (should (equal (nskk-convert-romaji "mk") "みん"))
        (should (equal (nskk-convert-romaji "mj") "むん"))
        (should (equal (nskk-convert-romaji "md") "めん"))
        (should (equal (nskk-convert-romaji "ml") "もん")))))

  (nskk-context "y-row hatsuon"
    (nskk-it "yz yk yj yd yl convert to やん いん ゆん えん よん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "yz") "やん"))
        (should (equal (nskk-convert-romaji "yk") "いん"))
        (should (equal (nskk-convert-romaji "yj") "ゆん"))
        (should (equal (nskk-convert-romaji "yd") "えん"))
        (should (equal (nskk-convert-romaji "yl") "よん")))))

  (nskk-context "r-row hatsuon"
    (nskk-it "rz rk rj rd rl convert to らん りん るん れん ろん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "rz") "らん"))
        (should (equal (nskk-convert-romaji "rk") "りん"))
        (should (equal (nskk-convert-romaji "rj") "るん"))
        (should (equal (nskk-convert-romaji "rd") "れん"))
        (should (equal (nskk-convert-romaji "rl") "ろん")))))

  (nskk-context "w-row hatsuon"
    (nskk-it "wz wk wj wd wl convert to わん うぃん うん うぇん をん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "wz") "わん"))
        (should (equal (nskk-convert-romaji "wk") "うぃん"))
        (should (equal (nskk-convert-romaji "wj") "うん"))
        (should (equal (nskk-convert-romaji "wd") "うぇん"))
        (should (equal (nskk-convert-romaji "wl") "をん")))))

  (nskk-context "g-row hatsuon"
    (nskk-it "gz gk gj gd gl convert to がん ぎん ぐん げん ごん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "gz") "がん"))
        (should (equal (nskk-convert-romaji "gk") "ぎん"))
        (should (equal (nskk-convert-romaji "gj") "ぐん"))
        (should (equal (nskk-convert-romaji "gd") "げん"))
        (should (equal (nskk-convert-romaji "gl") "ごん")))))

  (nskk-context "z-row hatsuon"
    (nskk-it "zk zj zd zl convert to じん ずん ぜん ぞん and lookup table is complete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "zz") "ざん"))
        (should (equal (nskk-converter-lookup "zk") "じん"))
        (should (equal (nskk-converter-lookup "zj") "ずん"))
        (should (equal (nskk-converter-lookup "zd") "ぜん"))
        (should (equal (nskk-converter-lookup "zl") "ぞん"))
        ;; zk, zj, zd, zl should work in conversion
        (should (equal (nskk-convert-romaji "zk") "じん"))
        (should (equal (nskk-convert-romaji "zj") "ずん"))
        (should (equal (nskk-convert-romaji "zd") "ぜん"))
        (should (equal (nskk-convert-romaji "zl") "ぞん")))))

  (nskk-context "d-row hatsuon"
    (nskk-it "dz dk dj dl convert to だん ぢん づん どん and lookup table is complete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "dz") "だん"))
        (should (equal (nskk-converter-lookup "dk") "ぢん"))
        (should (equal (nskk-converter-lookup "dj") "づん"))
        (should (equal (nskk-converter-lookup "dd") "でん"))
        (should (equal (nskk-converter-lookup "dl") "どん"))
        ;; dz, dk, dj, dl should work in conversion
        (should (equal (nskk-convert-romaji "dz") "だん"))
        (should (equal (nskk-convert-romaji "dk") "ぢん"))
        (should (equal (nskk-convert-romaji "dj") "づん"))
        (should (equal (nskk-convert-romaji "dl") "どん")))))

  (nskk-context "b-row hatsuon"
    (nskk-it "bz bk bj bd bl convert to ばん びん ぶん べん ぼん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "bz") "ばん"))
        (should (equal (nskk-convert-romaji "bk") "びん"))
        (should (equal (nskk-convert-romaji "bj") "ぶん"))
        (should (equal (nskk-convert-romaji "bd") "べん"))
        (should (equal (nskk-convert-romaji "bl") "ぼん")))))

  (nskk-context "p-row hatsuon"
    (nskk-it "pz pk pj pd pl convert to ぱん ぴん ぷん ぺん ぽん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "pz") "ぱん"))
        (should (equal (nskk-convert-romaji "pk") "ぴん"))
        (should (equal (nskk-convert-romaji "pj") "ぷん"))
        (should (equal (nskk-convert-romaji "pd") "ぺん"))
        (should (equal (nskk-convert-romaji "pl") "ぽん")))))

  (nskk-context "x-row hatsuon extensions"
    (nskk-it "xz xk xj xd xl convert to しゃん しん しゅん しぇん しょん"
      (nskk-with-azik-style
        ;; "xk" is vowel-only-shadowed: kept complete for xk→しん (xka/xke still reachable)
        (should (equal (nskk-converter-lookup "xk") "しん"))
        (should (equal (nskk-convert-romaji "xk") "しん"))
        ;; Other rules should exist in the table
        (should (equal (nskk-converter-lookup "xz") "しゃん"))
        (should (equal (nskk-converter-lookup "xj") "しゅん"))
        (should (equal (nskk-converter-lookup "xd") "しぇん"))
        (should (equal (nskk-converter-lookup "xl") "しょん"))
        ;; Conversion should work for all rules
        (should (equal (nskk-convert-romaji "xz") "しゃん"))
        (should (equal (nskk-convert-romaji "xj") "しゅん"))
        (should (equal (nskk-convert-romaji "xd") "しぇん"))
        (should (equal (nskk-convert-romaji "xl") "しょん"))
        ;; Negative tests - should not equal incorrect values
        (should-not (equal (nskk-convert-romaji "xz") "しゃ"))
        (should-not (equal (nskk-convert-romaji "xj") "しゅ")))))

  (nskk-context "c-row hatsuon extensions"
    (nskk-it "cz ck cj cd cl convert to ちゃん ちん ちゅん ちぇん ちょん"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "cz") "ちゃん"))
        (should (equal (nskk-converter-lookup "ck") "ちん"))
        (should (equal (nskk-converter-lookup "cj") "ちゅん"))
        (should (equal (nskk-converter-lookup "cd") "ちぇん"))
        (should (equal (nskk-converter-lookup "cl") "ちょん"))
        ;; Conversion should work
        (should (equal (nskk-convert-romaji "cz") "ちゃん"))
        (should (equal (nskk-convert-romaji "ck") "ちん"))
        (should (equal (nskk-convert-romaji "cj") "ちゅん"))
        (should (equal (nskk-convert-romaji "cd") "ちぇん"))
        (should (equal (nskk-convert-romaji "cl") "ちょん"))
        ;; Negative tests - should not equal incorrect values
        (should-not (equal (nskk-convert-romaji "cz") "ちゃ"))
        (should-not (equal (nskk-convert-romaji "ck") "ちんん"))
        (should-not (equal (nskk-convert-romaji "cj") "ちゅ"))))))

  (nskk-context "f-row hatsuon extensions"
    (nskk-it "fz fk fj fd fl convert to ふぁん ふぃん ふん ふぇん ふぉん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "fz") "ふぁん"))
        (should (equal (nskk-convert-romaji "fk") "ふぃん"))
        (should (equal (nskk-convert-romaji "fj") "ふん"))
        (should (equal (nskk-convert-romaji "fd") "ふぇん"))
        (should (equal (nskk-convert-romaji "fl") "ふぉん")))))

  (nskk-context "j-row hatsuon extensions"
    (nskk-it "jz jk jj jd jl convert to じゃん じん じゅん じぇん じょん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "jz") "じゃん"))
        (should (equal (nskk-convert-romaji "jk") "じん"))
        (should (equal (nskk-convert-romaji "jj") "じゅん"))
        (should (equal (nskk-convert-romaji "jd") "じぇん"))
        (should (equal (nskk-convert-romaji "jl") "じょん")))))

  (nskk-context "v-row hatsuon extensions"
    (nskk-it "vz vk vj vd vl convert to ゔぁん ゔぃん ゔん ゔぇん ゔぉん"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "vz") "ゔぁん"))
        (should (equal (nskk-convert-romaji "vk") "ゔぃん"))
        (should (equal (nskk-convert-romaji "vj") "ゔん"))
        (should (equal (nskk-convert-romaji "vd") "ゔぇん"))
        (should (equal (nskk-convert-romaji "vl") "ゔぉん")))))


;;;;
;;;; 4. 二重母音拡張 (Diphthong Extension) Tests
;;;;

(nskk-describe "AZIK diphthong (二重母音拡張) rules"
  (nskk-context "k-row diphthong"
    (nskk-it "kq kh kw kp convert to かい くう けい こう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "kq") "かい"))
        (should (equal (nskk-convert-romaji "kh") "くう"))
        (should (equal (nskk-convert-romaji "kw") "けい"))
        (should (equal (nskk-convert-romaji "kp") "こう")))))

  (nskk-context "s-row diphthong"
    (nskk-it "sq sh sw sp convert to さい すう せい そう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "sq") "さい"))
        ;; "sh" is vowel-only-shadowed: kept complete (not :incomplete) for Sh→すう
        ;; sha/shi/shu/she/sho remain reachable via the azik-vowel-deferred mechanism
        (should (equal (nskk-converter-lookup "sh") "すう"))
        (should (equal (nskk-convert-romaji "sh") "すう"))
        (should (equal (nskk-convert-romaji "sw") "せい"))
        (should (equal (nskk-convert-romaji "sp") "そう")))))

  (nskk-context "t-row diphthong"
    (nskk-it "tq th tw tp convert to たい つう てい とう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "tq") "たい"))
        ;; "th" is vowel-only-shadowed: kept complete for Th→つう
        (should (equal (nskk-converter-lookup "th") "つう"))
        (should (equal (nskk-convert-romaji "th") "つう"))
        (should (equal (nskk-convert-romaji "tw") "てい"))
        (should (equal (nskk-convert-romaji "tp") "とう")))))

  (nskk-context "n-row diphthong"
    (nskk-it "nq nh nw np rules exist in the lookup table"
      (nskk-with-azik-style
        ;; Verify rules exist in the table (they are correctly defined)
        (should (equal (nskk-converter-lookup "nq") "ない"))
        (should (equal (nskk-converter-lookup "nh") "ぬう"))
        (should (equal (nskk-converter-lookup "nw") "ねい"))
        (should (equal (nskk-converter-lookup "np") "のう")))))

  (nskk-context "h-row diphthong"
    (nskk-it "hq hw hp convert to はい へい ほう and lookup table is complete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "hq") "はい"))
        (should (equal (nskk-converter-lookup "hh") "ふう"))
        (should (equal (nskk-converter-lookup "hw") "へい"))
        (should (equal (nskk-converter-lookup "hp") "ほう"))
        ;; hq, hw, hp should work in conversion
        (should (equal (nskk-convert-romaji "hq") "はい"))
        (should (equal (nskk-convert-romaji "hw") "へい"))
        (should (equal (nskk-convert-romaji "hp") "ほう")))))

  (nskk-context "m-row diphthong"
    (nskk-it "mq mh mw mp convert to まい むう めい もう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "mq") "まい"))
        (should (equal (nskk-convert-romaji "mh") "むう"))
        (should (equal (nskk-convert-romaji "mw") "めい"))
        (should (equal (nskk-convert-romaji "mp") "もう")))))

  (nskk-context "y-row diphthong"
    (nskk-it "yq yh yw yp convert to やい ゆう えい よう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "yq") "やい"))
        (should (equal (nskk-convert-romaji "yh") "ゆう"))
        (should (equal (nskk-convert-romaji "yw") "えい"))
        (should (equal (nskk-convert-romaji "yp") "よう")))))

  (nskk-context "r-row diphthong"
    (nskk-it "rq rh rw rp convert to らい るう れい ろう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "rq") "らい"))
        (should (equal (nskk-convert-romaji "rh") "るう"))
        (should (equal (nskk-convert-romaji "rw") "れい"))
        (should (equal (nskk-convert-romaji "rp") "ろう")))))

  (nskk-context "w-row diphthong"
    (nskk-it "wq wh wp convert to わい うう うぉう"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "wq") "わい"))
        ;; "wh" is vowel-only-shadowed: kept complete for Wh→うう
        (should (equal (nskk-converter-lookup "wh") "うう"))
        (should (equal (nskk-convert-romaji "wh") "うう"))
        (should (equal (nskk-converter-lookup "ww") "うぇい"))
        (should (equal (nskk-converter-lookup "wp") "うぉう"))
        ;; wq, wp should work in conversion
        (should (equal (nskk-convert-romaji "wq") "わい"))
        (should (equal (nskk-convert-romaji "wp") "うぉう")))))

  (nskk-context "g-row diphthong"
    (nskk-it "gq gh gw gp convert to がい ぐう げい ごう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "gq") "がい"))
        (should (equal (nskk-convert-romaji "gh") "ぐう"))
        (should (equal (nskk-convert-romaji "gw") "げい"))
        (should (equal (nskk-convert-romaji "gp") "ごう")))))

  (nskk-context "z-row diphthong"
    (nskk-it "zq zh zw zp convert to ざい ずう ぜい ぞう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "zq") "ざい"))
        (should (equal (nskk-convert-romaji "zh") "ずう"))
        (should (equal (nskk-convert-romaji "zw") "ぜい"))
        (should (equal (nskk-convert-romaji "zp") "ぞう")))))

  (nskk-context "d-row diphthong"
    (nskk-it "dq dh dw dp convert to だい づう でい どう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "dq") "だい"))
        ;; "dh" is vowel-only-shadowed: kept complete for Dh→づう
        (should (equal (nskk-converter-lookup "dh") "づう"))
        (should (equal (nskk-convert-romaji "dh") "づう"))
        (should (equal (nskk-convert-romaji "dw") "でい"))
        (should (equal (nskk-convert-romaji "dp") "どう")))))

  (nskk-context "b-row diphthong"
    (nskk-it "bq bh bw bp convert to ばい ぶう べい ぼう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "bq") "ばい"))
        (should (equal (nskk-convert-romaji "bh") "ぶう"))
        (should (equal (nskk-convert-romaji "bw") "べい"))
        (should (equal (nskk-convert-romaji "bp") "ぼう")))))

  (nskk-context "p-row diphthong"
    (nskk-it "pq ph pw convert to ぱい ぷう ぺい and lookup table is complete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "pq") "ぱい"))
        (should (equal (nskk-converter-lookup "ph") "ぷう"))
        (should (equal (nskk-converter-lookup "pw") "ぺい"))
        (should (equal (nskk-converter-lookup "pp") "ぽう"))
        ;; pq, ph, pw should work in conversion
        (should (equal (nskk-convert-romaji "pq") "ぱい"))
        (should (equal (nskk-convert-romaji "ph") "ぷう"))
        (should (equal (nskk-convert-romaji "pw") "ぺい")))))

  (nskk-context "x-row diphthong extensions"
    (nskk-it "xq xh xw xp convert to しゃい しゅう しぇい しょう"
      (nskk-with-azik-style
        ;; "xw" is vowel-only-shadowed: kept complete for xw→しぇい (xwa still reachable)
        (should (equal (nskk-converter-lookup "xw") "しぇい"))
        (should (equal (nskk-convert-romaji "xw") "しぇい"))
        ;; xq, xh, xp should work in lookup
        (should (equal (nskk-converter-lookup "xq") "しゃい"))
        (should (equal (nskk-converter-lookup "xh") "しゅう"))
        (should (equal (nskk-converter-lookup "xp") "しょう"))
        ;; Conversion should work for all rules
        (should (equal (nskk-convert-romaji "xq") "しゃい"))
        (should (equal (nskk-convert-romaji "xh") "しゅう"))
        (should (equal (nskk-convert-romaji "xp") "しょう"))
        ;; Negative tests - should not equal incorrect values
        (should-not (equal (nskk-convert-romaji "xh") "しゅ"))
        (should-not (equal (nskk-convert-romaji "xh") "しゅうう")))))

  (nskk-context "c-row diphthong extensions"
    (nskk-it "cq ch cw cp convert to ちゃい ちゅう ちぇい ちょう"
      (nskk-with-azik-style
        ;; "ch" is vowel-only-shadowed: kept complete for Ch→ちゅう
        (should (equal (nskk-converter-lookup "ch") "ちゅう"))
        (should (equal (nskk-convert-romaji "ch") "ちゅう"))
        ;; cq, cw, cp should work in lookup
        (should (equal (nskk-converter-lookup "cq") "ちゃい"))
        (should (equal (nskk-converter-lookup "cw") "ちぇい"))
        (should (equal (nskk-converter-lookup "cp") "ちょう"))
        ;; Conversion should work for all rules
        (should (equal (nskk-convert-romaji "cq") "ちゃい"))
        (should (equal (nskk-convert-romaji "cw") "ちぇい"))
        (should (equal (nskk-convert-romaji "cp") "ちょう")))))

  (nskk-context "f-row diphthong extensions"
    (nskk-it "fq fh fw fp convert to ふぁい ふう ふぇい ふぉう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "fq") "ふぁい"))
        (should (equal (nskk-convert-romaji "fh") "ふう"))
        (should (equal (nskk-convert-romaji "fw") "ふぇい"))
        (should (equal (nskk-convert-romaji "fp") "ふぉう")))))

  (nskk-context "j-row diphthong extensions"
    (nskk-it "jq jh jw jp convert to じゃい じゅう じぇい じょう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "jq") "じゃい"))
        (should (equal (nskk-convert-romaji "jh") "じゅう"))
        (should (equal (nskk-convert-romaji "jw") "じぇい"))
        (should (equal (nskk-convert-romaji "jp") "じょう")))))

  (nskk-context "v-row diphthong extensions"
    (nskk-it "vq vh vw vp convert to ゔぁい ゔう ゔぇい ゔぉう"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "vq") "ゔぁい"))
        (should (equal (nskk-convert-romaji "vh") "ゔう"))
        (should (equal (nskk-convert-romaji "vw") "ゔぇい"))
        (should (equal (nskk-convert-romaji "vp") "ゔぉう"))))))


;;;;
;;;; 5. 拗音互換キー (Yō-on Compatibility Key) Tests
;;;;

(nskk-describe "AZIK youon (拗音互換キー) rules"
  (nskk-context "kg-row youon"
    (nskk-it "kga kgu kge kgo convert to きゃ きゅ きぇ きょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "kga") "きゃ"))
        (should (equal (nskk-convert-romaji "kgu") "きゅ"))
        (should (equal (nskk-convert-romaji "kge") "きぇ"))
        (should (equal (nskk-convert-romaji "kgo") "きょ")))))

  (nskk-context "hg-row youon"
    (nskk-it "hga hgu hge hgo convert to ひゃ ひゅ ひぇ ひょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "hga") "ひゃ"))
        (should (equal (nskk-convert-romaji "hgu") "ひゅ"))
        (should (equal (nskk-convert-romaji "hge") "ひぇ"))
        (should (equal (nskk-convert-romaji "hgo") "ひょ")))))

  (nskk-context "mg-row youon"
    (nskk-it "mga mgu mge mgo convert to みゃ みゅ みぇ みょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "mga") "みゃ"))
        (should (equal (nskk-convert-romaji "mgu") "みゅ"))
        (should (equal (nskk-convert-romaji "mge") "みぇ"))
        (should (equal (nskk-convert-romaji "mgo") "みょ")))))

  (nskk-context "rg-row youon"
    (nskk-it "rga rgu rge rgo convert to りゃ りゅ りぇ りょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "rga") "りゃ"))
        (should (equal (nskk-convert-romaji "rgu") "りゅ"))
        (should (equal (nskk-convert-romaji "rge") "りぇ"))
        (should (equal (nskk-convert-romaji "rgo") "りょ")))))

  (nskk-context "ry-row youon (DDSKK-compatible y-prefix + AZIK extensions)"
    (nskk-it "rya ryu rye ryo convert to りゃ りゅ りぇ りょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "rya") "りゃ"))
        (should (equal (nskk-convert-romaji "ryu") "りゅ"))
        (should (equal (nskk-convert-romaji "rye") "りぇ"))
        (should (equal (nskk-convert-romaji "ryo") "りょ"))))

    (nskk-it "ryp ryh ryw ryq convert to りょう りゅう りぇい りゃい"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ryp") "りょう"))
        (should (equal (nskk-convert-romaji "ryh") "りゅう"))
        (should (equal (nskk-convert-romaji "ryw") "りぇい"))
        (should (equal (nskk-convert-romaji "ryq") "りゃい"))))

    (nskk-it "ryz ryk ryj ryd ryl convert to りゃん りぃん りゅん りぇん りょん (hatsuon extensions)"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ryz") "りゃん"))
        (should (equal (nskk-convert-romaji "ryk") "りぃん"))
        (should (equal (nskk-convert-romaji "ryj") "りゅん"))
        (should (equal (nskk-convert-romaji "ryd") "りぇん"))
        (should (equal (nskk-convert-romaji "ryl") "りょん")))))

  (nskk-context "gg-row youon"
    (nskk-it "gga ggu gge ggo rules exist in the lookup table"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "gga") "ぎゃ"))
        (should (equal (nskk-converter-lookup "ggu") "ぎゅ"))
        (should (equal (nskk-converter-lookup "gge") "ぎぇ"))
        (should (equal (nskk-converter-lookup "ggo") "ぎょ")))))

  (nskk-context "jg-row youon"
    (nskk-it "jga jgu jge jgo convert to じゃ じゅ じぇ じょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "jga") "じゃ"))
        (should (equal (nskk-convert-romaji "jgu") "じゅ"))
        (should (equal (nskk-convert-romaji "jge") "じぇ"))
        (should (equal (nskk-convert-romaji "jgo") "じょ")))))

  (nskk-context "bg-row youon"
    (nskk-it "bga bgu bge bgo convert to びゃ びゅ びぇ びょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "bga") "びゃ"))
        (should (equal (nskk-convert-romaji "bgu") "びゅ"))
        (should (equal (nskk-convert-romaji "bge") "びぇ"))
        (should (equal (nskk-convert-romaji "bgo") "びょ")))))

  (nskk-context "pg-row youon"
    (nskk-it "pga pgu pge pgo convert to ぴゃ ぴゅ ぴぇ ぴょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "pga") "ぴゃ"))
        (should (equal (nskk-convert-romaji "pgu") "ぴゅ"))
        (should (equal (nskk-convert-romaji "pge") "ぴぇ"))
        (should (equal (nskk-convert-romaji "pgo") "ぴょ")))))

  (nskk-context "youon with hatsuon extension"
    (nskk-it "kg + hatsuon codes produce youon followed by ん"
      (nskk-with-azik-style
        ;; kg + 撥音拡張
        (should (equal (nskk-convert-romaji "kgz") "きゃん"))
        (should (equal (nskk-convert-romaji "kgk") "きぃん"))
        (should (equal (nskk-convert-romaji "kgj") "きゅん"))
        (should (equal (nskk-convert-romaji "kgd") "きぇん"))
        (should (equal (nskk-convert-romaji "kgl") "きょん")))))

  (nskk-context "youon with diphthong extension"
    (nskk-it "kg + diphthong codes produce youon followed by double vowel"
      (nskk-with-azik-style
        ;; kg + 二重母音拡張
        (should (equal (nskk-convert-romaji "kgq") "きゃい"))
        (should (equal (nskk-convert-romaji "kgh") "きゅう"))
        (should (equal (nskk-convert-romaji "kgw") "きぇい"))
        (should (equal (nskk-convert-romaji "kgp") "きょう"))))))


;;;;
;;;; 6. 同指打鍵互換キー (Same-Finger Compatibility Key) Tests
;;;;

(nskk-describe "AZIK same-finger (同指打鍵互換キー) keys"
  (nskk-it "kf converts to き"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "kf") "き"))))

  (nskk-it "nf rule exists in the lookup table"
    (nskk-with-azik-style
      ;; Verify rule exists in the table
      (should (equal (nskk-converter-lookup "nf") "ぬ"))))

  (nskk-it "mf converts to む"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "mf") "む"))))

  (nskk-it "gf converts to ぐ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "gf") "ぐ"))))

  (nskk-it "pf converts to ぷ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "pf") "ぷ"))))

  (nskk-it "rf converts to る"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "rf") "る"))))

  (nskk-it "yf converts to ゆ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "yf") "ゆ")))))

  (nskk-it "hf converts to ふ (same-finger alt for hu/fu)"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "hf") "ふ"))))


;;;;
;;;; 7. 特殊拡張 (Special Extension) Tests
;;;;

(nskk-describe "AZIK special extension (特殊拡張) shortcuts"
  (nskk-context "k-row shortcuts"
    (nskk-it "km kr kt convert to かも から こと"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "km") "かも"))
        (should (equal (nskk-convert-romaji "kr") "から"))
        (should (equal (nskk-convert-romaji "kt") "こと")))))

  (nskk-context "g-row shortcuts"
    (nskk-it "gr gt convert to がら ごと"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "gr") "がら"))
        (should (equal (nskk-convert-romaji "gt") "ごと")))))

  (nskk-context "z-row shortcuts"
    (nskk-it "zr converts to ざる"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "zr") "ざる")))))

  (nskk-context "s-row shortcuts"
    (nskk-it "st sr convert to した する"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "st") "した"))
        (should (equal (nskk-convert-romaji "sr") "する")))))

  (nskk-context "t-row shortcuts"
    (nskk-it "tb tm tr convert to たび ため たら and lookup table is complete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "tt") "たち"))
        (should (equal (nskk-converter-lookup "tb") "たび"))
        (should (equal (nskk-converter-lookup "tm") "ため"))
        (should (equal (nskk-converter-lookup "tr") "たら"))
        ;; tb, tm, tr should work in conversion
        (should (equal (nskk-convert-romaji "tb") "たび"))
        (should (equal (nskk-convert-romaji "tm") "ため"))
        (should (equal (nskk-convert-romaji "tr") "たら")))))

  (nskk-context "d-row shortcuts"
    (nskk-it "dt ds dm convert to だち です でも"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "dt") "だち"))
        (should (equal (nskk-convert-romaji "ds") "です"))
        (should (equal (nskk-convert-romaji "dm") "でも")))))

  (nskk-context "n-row shortcuts"
    (nskk-it "nr nt nb rules exist in the lookup table"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "nr") "なる"))
        (should (equal (nskk-converter-lookup "nt") "にち"))
        (should (equal (nskk-converter-lookup "nb") "ねば")))))

  (nskk-context "h-row shortcuts"
    (nskk-it "ht converts to ひと"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ht") "ひと")))))

  (nskk-context "b-row shortcuts"
    (nskk-it "bt converts to びと"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "bt") "びと")))))

  (nskk-context "m-row shortcuts"
    (nskk-it "ms mt mn convert to ます また もの"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ms") "ます"))
        (should (equal (nskk-convert-romaji "mt") "また"))
        (should (equal (nskk-convert-romaji "mn") "もの")))))

  (nskk-context "y-row shortcuts"
    (nskk-it "yr converts to よる"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "yr") "よる")))))

  (nskk-context "r-row shortcuts"
    (nskk-it "rr rule exists in the lookup table"
      (nskk-with-azik-style
        ;; Verify rule exists in the table
        (should (equal (nskk-converter-lookup "rr") "られ")))))

  (nskk-context "w-row shortcuts"
    (nskk-it "wt wr convert to わた われ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "wt") "わた"))
        (should (equal (nskk-convert-romaji "wr") "われ"))))))


;;;;
;;;; 8. Q-key Behavior Tests
;;;;

(nskk-describe "AZIK q-key behavior"
  (nskk-it "q-key works as extension key for diphthongs without standalone definition"
    (nskk-with-azik-style
      ;; In AZIK, 'q' is used as an extension key for diphthongs
      ;; It should not have a standalone conversion
      ;; When used with consonants like 'kq', it produces 'かい'
      (should (equal (nskk-convert-romaji "kq") "かい"))
      (should (equal (nskk-convert-romaji "sq") "さい"))
      (should (equal (nskk-convert-romaji "tq") "たい"))))

  (nskk-it "standalone q has no conversion rule"
    (nskk-with-azik-style
      ;; When q is typed alone, it should not convert (no rule for standalone q)
      ;; The rule lookup should return nil for standalone 'q'
      (should-not (nskk-converter-lookup "q"))))

  (nskk-it "q-key with pending consonant input completes diphthong"
    (nskk-with-azik-style
      ;; When there's pending input like 'k', 'q' should complete to 'かい'
      (should (equal (nskk-convert-romaji "kq") "かい"))
      (should (equal (nskk-convert-romaji "hq") "はい"))
      (should (equal (nskk-convert-romaji "sq") "さい"))
      (should (equal (nskk-convert-romaji "tq") "たい")))))


;;;;
;;;; 9. Compatibility Tests
;;;;

(nskk-describe "AZIK compatibility with standard romaji"
  (nskk-context "basic vowels"
    (nskk-it "a i u e o convert to あ い う え お in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "a") "あ"))
        (should (equal (nskk-convert-romaji "i") "い"))
        (should (equal (nskk-convert-romaji "u") "う"))
        (should (equal (nskk-convert-romaji "e") "え"))
        (should (equal (nskk-convert-romaji "o") "お")))))

  (nskk-context "k-row standard romaji"
    (nskk-it "ka ki ku ke ko still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ka") "か"))
        (should (equal (nskk-convert-romaji "ki") "き"))
        (should (equal (nskk-convert-romaji "ku") "く"))
        (should (equal (nskk-convert-romaji "ke") "け"))
        (should (equal (nskk-convert-romaji "ko") "こ")))))

  (nskk-context "g-row standard romaji"
    (nskk-it "ga gi gu ge go still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ga") "が"))
        (should (equal (nskk-convert-romaji "gi") "ぎ"))
        (should (equal (nskk-convert-romaji "gu") "ぐ"))
        (should (equal (nskk-convert-romaji "ge") "げ"))
        (should (equal (nskk-convert-romaji "go") "ご")))))

  (nskk-context "s-row standard romaji"
    (nskk-it "sa shi su se so still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "sa") "さ"))
        (should (equal (nskk-convert-romaji "shi") "し"))
        (should (equal (nskk-convert-romaji "su") "す"))
        (should (equal (nskk-convert-romaji "se") "せ"))
        (should (equal (nskk-convert-romaji "so") "そ")))))

  (nskk-context "t-row standard romaji"
    (nskk-it "ta chi tsu te to still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ta") "た"))
        (should (equal (nskk-convert-romaji "chi") "ち"))
        (should (equal (nskk-convert-romaji "tsu") "つ"))
        (should (equal (nskk-convert-romaji "te") "て"))
        (should (equal (nskk-convert-romaji "to") "と")))))

  (nskk-context "n-row standard romaji"
    (nskk-it "na ni nu ne no nn still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "na") "な"))
        (should (equal (nskk-convert-romaji "ni") "に"))
        (should (equal (nskk-convert-romaji "nu") "ぬ"))
        (should (equal (nskk-convert-romaji "ne") "ね"))
        (should (equal (nskk-convert-romaji "no") "の"))
        (should (equal (nskk-convert-romaji "nn") "ん")))))

  (nskk-context "h-row standard romaji"
    (nskk-it "ha hi fu he ho still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ha") "は"))
        (should (equal (nskk-convert-romaji "hi") "ひ"))
        (should (equal (nskk-convert-romaji "fu") "ふ"))
        (should (equal (nskk-convert-romaji "he") "へ"))
        (should (equal (nskk-convert-romaji "ho") "ほ")))))

  (nskk-context "m-row standard romaji"
    (nskk-it "ma mi mu me mo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ma") "ま"))
        (should (equal (nskk-convert-romaji "mi") "み"))
        (should (equal (nskk-convert-romaji "mu") "む"))
        (should (equal (nskk-convert-romaji "me") "め"))
        (should (equal (nskk-convert-romaji "mo") "も")))))

  (nskk-context "y-row standard romaji"
    (nskk-it "ya yu yo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ya") "や"))
        (should (equal (nskk-convert-romaji "yu") "ゆ"))
        (should (equal (nskk-convert-romaji "yo") "よ")))))

  (nskk-context "r-row standard romaji"
    (nskk-it "ra ri ru re ro still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ra") "ら"))
        (should (equal (nskk-convert-romaji "ri") "り"))
        (should (equal (nskk-convert-romaji "ru") "る"))
        (should (equal (nskk-convert-romaji "re") "れ"))
        (should (equal (nskk-convert-romaji "ro") "ろ")))))

  (nskk-context "w-row standard romaji"
    (nskk-it "wa wo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "wa") "わ"))
        (should (equal (nskk-convert-romaji "wo") "を")))))

  (nskk-context "youon (digraphs) standard romaji"
    (nskk-it "kya kyu kyo sha shu sho cha chu cho still convert in AZIK mode"
      (nskk-with-azik-style
        ;; Standard kya, kyu, kyo
        (should (equal (nskk-convert-romaji "kya") "きゃ"))
        (should (equal (nskk-convert-romaji "kyu") "きゅ"))
        (should (equal (nskk-convert-romaji "kyo") "きょ"))
        ;; Standard sha, shu, sho
        (should (equal (nskk-convert-romaji "sha") "しゃ"))
        (should (equal (nskk-convert-romaji "shu") "しゅ"))
        (should (equal (nskk-convert-romaji "sho") "しょ"))
        ;; Standard cha, chu, cho
        (should (equal (nskk-convert-romaji "cha") "ちゃ"))
        (should (equal (nskk-convert-romaji "chu") "ちゅ"))
        (should (equal (nskk-convert-romaji "cho") "ちょ")))))

  (nskk-context "c-row compat (c=ちゃ行)"
    (nskk-it "ca ci cu ce co convert to ちゃ ち ちゅ ちぇ ちょ"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "ca") "ちゃ"))
        (should (equal (nskk-convert-romaji "ci") "ち"))
        (should (equal (nskk-convert-romaji "cu") "ちゅ"))
        (should (equal (nskk-convert-romaji "ce") "ちぇ"))
        (should (equal (nskk-convert-romaji "co") "ちょ")))))

  (nskk-context "complete word conversion"
    (nskk-it "can convert complete words using AZIK features"
      (nskk-with-azik-style
        ;; Using 撥音拡張
        (should (equal (nskk-convert-romaji "sz") "さん"))
        (should (equal (nskk-convert-romaji "tl") "とん"))
        ;; Using 二重母音拡張
        (should (equal (nskk-convert-romaji "tp") "とう"))
        (should (equal (nskk-convert-romaji "kw") "けい"))
        ;; Mixed conversion
        (should (equal (nskk-convert-romaji "kztp") "かんとう"))))))


;;;;
;;;; 10. Integration Tests
;;;;

(nskk-describe "AZIK integration"
  (nskk-it "basic AZIK input sequences convert correctly"
    (nskk-with-azik-style
      ;; Type "kz" -> "かん"
      (should (equal (nskk-convert-romaji "kz") "かん"))
      ;; Type "sz" -> "さん"
      (should (equal (nskk-convert-romaji "sz") "さん"))
      ;; Type "tp" -> "とう"
      (should (equal (nskk-convert-romaji "tp") "とう"))))

  (nskk-it "complex word conversion using AZIK combinations"
    (nskk-with-azik-style
      ;; "きょう" using kg + p (拗音 + 二重母音)
      (should (equal (nskk-convert-romaji "kgp") "きょう"))
      ;; "きょうと" using 拗音二重母音 + と
      (should (equal (nskk-convert-romaji "kgpto") "きょうと"))
      ;; "とうきょ" using tp (とう) + kyo (きょ) — note: no trailing う
      (should (equal (nskk-convert-romaji "tpkyo") "とうきょ"))
      ;; "さんぽ" using さん + ぽ
      (should (equal (nskk-convert-romaji "szpo") "さんぽ"))))

  (nskk-it "mixed standard and AZIK input converts correctly"
    (nskk-with-azik-style
      ;; Mix standard and AZIK rules
      (should (equal (nskk-convert-romaji "kakz") "かかん"))
      (should (equal (nskk-convert-romaji "sask") "さしん"))
      ;; Note: tatq = ta + tq = た + たい (not たちたい)
      (should (equal (nskk-convert-romaji "tatq") "たたい"))
      ;; For たちたい, use ta + chi + tq
      (should (equal (nskk-convert-romaji "tachitq") "たちたい"))))

  (nskk-context "x/c prefix compound sequences"
    (nskk-it "x prefix compounds with extension keys work correctly"
      (nskk-with-azik-style
        ;; xh + ka = しゅう + か = しゅうか (shuuka)
        (should (equal (nskk-convert-romaji "xhka") "しゅうか"))
        ;; xh + kak = しゅう + かく = しゅうかく (shuukaku)
        (should (equal (nskk-convert-romaji "xhkak") "しゅうかく"))
        ;; xh + kaq = しゅう + かい = しゅうかい (shuukai)
        (should (equal (nskk-convert-romaji "xhkaq") "しゅうかい"))
        ;; xp + to = しょう + と = しょうと
        (should (equal (nskk-convert-romaji "xpto") "しょうと"))
        ;; xz + ka = しゃん + か = しゃんか
        (should (equal (nskk-convert-romaji "xzka") "しゃんか"))))

    (nskk-it "c prefix compounds with extension keys work correctly"
      (nskk-with-azik-style
        ;; cp + ka = ちょう + か = ちょうか (chouka)
        (should (equal (nskk-convert-romaji "cpka") "ちょうか"))
        ;; cp + to = ちょう + と = ちょうと
        (should (equal (nskk-convert-romaji "cpto") "ちょうと"))
        ;; cz + ka = ちゃん + か = ちゃんか
        (should (equal (nskk-convert-romaji "czka") "ちゃんか"))
        ;; cj + ka = ちゅん + か = ちゅんか
        (should (equal (nskk-convert-romaji "cjka") "ちゅんか"))))

    (nskk-it "x/c prefixes in longer compound words"
      (nskk-with-azik-style
        ;; shuukakuki (しゅうかくき - harvest machine)
        (should (equal (nskk-convert-romaji "xhkakki") "しゅうかくき"))
        ;; choukahi (ちょうかひ - over-cost)
        (should (equal (nskk-convert-romaji "cpkahi") "ちょうかひ"))))))


;;;;
;;;; 11. Regression Tests
;;;;

(nskk-describe "AZIK regression tests"
  (nskk-it "kz produces かん not かn"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "kz") "かん"))
      (should-not (equal (nskk-convert-romaji "kz") "かn"))))

  (nskk-it "semicolon produces small tsu and works in context"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji ";") "っ"))
      ;; In context: ;ka -> っか
      (should (equal (nskk-convert-romaji ";ka") "っか"))))

  (nskk-it "colon produces chouon"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji ":") "ー")))))


;;;;
;;;; 10a. Prolog-level Tests
;;;;

(nskk-describe "AZIK Prolog-level rules"
  (nskk-it "azik-rule/2 facts are directly queryable via Prolog"
    (nskk-with-azik-style
      ;; Special key: ; → っ
      (let ((results (nskk-prolog-query '(azik-rule ";" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "っ")))
      ;; Hatsuon extension: kz → かん
      (let ((results (nskk-prolog-query '(azik-rule "kz" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "かん")))
      ;; Double vowel: kq → かい
      (let ((results (nskk-prolog-query '(azik-rule "kq" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "かい")))
      ;; ng youon: nga → にゃ
      (let ((results (nskk-prolog-query '(azik-rule "nga" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "にゃ")))
      ;; word shortcut: ss → せい
      (let ((results (nskk-prolog-query '(azik-rule "ss" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "せい")))))

  (nskk-it "bridge rule makes azik-rule/2 facts accessible via romaji-to-kana/2 enumeration"
    (nskk-with-azik-style
      ;; Variable query enumerates all romaji-to-kana mappings including
      ;; AZIK rules reached via the bridge rule
      (let ((all-romajis (nskk-prolog-query-all-values
                          '(romaji-to-kana \?r \?k) '\?r)))
        (should all-romajis)
        ;; AZIK-specific keys must appear via bridge rule traversal
        (should (member ";" all-romajis))
        (should (member "kz" all-romajis))
        (should (member "kq" all-romajis)))))

  (nskk-it "azik-rule/2 contains a substantial number of facts (at least 320)"
    (nskk-with-azik-style
      (let ((results (nskk-prolog-query '(azik-rule \?r \?k))))
        (should results)
        ;; 2 special + 10 compat + 19*9 extensions + 18*14 youon + 8 same-finger
        ;; + 28 shortcuts + 5 foreign + 5 foreign-hatsuon + 12 foreign-dv
        ;; = at least 470 rules
        (should (>= (length results) 470)))))

  (nskk-it "switching to standard style resets the hot-path hash cache"
    (nskk-with-azik-style
      ;; AZIK style: azik-rule/2 is populated
      (should (nskk-prolog-query '(azik-rule "kz" \?k)))
      ;; Hot-path lookup finds AZIK rules
      (should (equal (nskk-converter-lookup "kz") "かん")))
    ;; After switching to standard: hash cache is reset
    (nskk-with-standard-style
      ;; AZIK-specific keys are gone from the hot-path (hash-based) lookup
      (should-not (nskk-converter-lookup "kz"))
      ;; Standard romaji rules are still accessible
      (should (equal (nskk-convert-romaji "ka") "か"))))

  (nskk-it "2-char youon prefixes produce :incomplete in the hash table"
    (nskk-with-azik-style
      ;; Single-char consonant prefixes
      (should (eq (nskk-converter-lookup "k") :incomplete))
      (should (eq (nskk-converter-lookup "h") :incomplete))
      (should (eq (nskk-converter-lookup "m") :incomplete))
      ;; 2-char g-substitution youon prefixes
      (should (eq (nskk-converter-lookup "kg") :incomplete))
      (should (eq (nskk-converter-lookup "hg") :incomplete))
      (should (eq (nskk-converter-lookup "mg") :incomplete))
      (should (eq (nskk-converter-lookup "rg") :incomplete))
      (should (eq (nskk-converter-lookup "jg") :incomplete))
      ;; 2-char y-prefix youon prefixes (DDSKK-compatible)
      (should (eq (nskk-converter-lookup "ky") :incomplete))
      (should (eq (nskk-converter-lookup "hy") :incomplete))
      (should (eq (nskk-converter-lookup "my") :incomplete))
      (should (eq (nskk-converter-lookup "ry") :incomplete))
      (should (eq (nskk-converter-lookup "jy") :incomplete))
      (should (eq (nskk-converter-lookup "ny") :incomplete))
      (should (eq (nskk-converter-lookup "gy") :incomplete))
      (should (eq (nskk-converter-lookup "by") :incomplete))
      (should (eq (nskk-converter-lookup "py") :incomplete)))))


;;;;
;;;; 10b. Compile-time Macro Expansion Tests
;;;;

(nskk-describe "AZIK compile-time macro expansions"
  (nskk-it "nskk-azik-hatsuon expands to 5 prolog assertions"
    ;; macroexpand-1 should produce a (progn ...) with 5 nskk-prolog-<- calls
    (let ((expansion (macroexpand-1
                      '(nskk-azik-hatsuon "k" "か" "き" "く" "け" "こ"))))
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 5))
      ;; First assertion: kz → かん
      (should (equal (cadr (cadr (nth 0 (cdr expansion)))) "kz"))
      (should (equal (caddr (cadr (nth 0 (cdr expansion)))) "かん"))))

  (nskk-it "nskk-azik-double-vowel expands to 4 prolog assertions"
    (let ((expansion (macroexpand-1
                      '(nskk-azik-double-vowel "k" "か" "く" "け" "こ"))))
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 4))))

  (nskk-it "nskk-azik-extensions expands to 9 prolog assertions (hatsuon + double-vowel)"
    (let ((expansion (macroexpand-1
                      '(nskk-azik-extensions "k" "か" "き" "く" "け" "こ"))))
      ;; expands to (progn (nskk-azik-hatsuon ...) (nskk-azik-double-vowel ...))
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 2))
      (should (eq (car (nth 1 expansion)) 'nskk-azik-hatsuon))
      (should (eq (car (nth 2 expansion)) 'nskk-azik-double-vowel))))

  (nskk-it "nskk-azik-youon expands to 4 base rules + extensions"
    (let ((expansion (macroexpand-1
                      '(nskk-azik-youon "kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ"))))
      ;; (progn <4 nskk-prolog-<- calls> (nskk-azik-extensions ...))
      (should (eq (car expansion) 'progn))
      ;; 4 base rules (a/u/e/o) + 1 extensions call = 5 top-level forms
      (should (= (length (cdr expansion)) 5))
      (should (eq (car (nth 5 expansion)) 'nskk-azik-extensions)))))




;;;;
;;;; 12. 外来語拡張 (Foreign Word Extension) Tests
;;;;

(nskk-describe "AZIK foreign word extensions (外来語拡張)"
  (nskk-it "tgi converts to てぃ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgi") "てぃ"))))

  (nskk-it "tgu converts to とぅ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgu") "とぅ"))))

  (nskk-it "dci converts to でぃ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dci") "でぃ"))))

  (nskk-it "dcu converts to どぅ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dcu") "どぅ"))))

  (nskk-it "wso converts to うぉ"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wso") "うぉ"))))

  (nskk-it "all foreign extensions exist in azik-rule/2 Prolog predicate"
    (nskk-with-azik-style
      (dolist (rule '(("tgi" "てぃ") ("tgu" "とぅ") ("dci" "でぃ") ("dcu" "どぅ") ("wso" "うぉ")))
        (let* ((romaji (car rule))
               (kana   (cadr rule))
               (results (nskk-prolog-query `(azik-rule ,romaji \?k))))
          (should results)
          (should (equal (nskk-prolog-walk '\?k (car results)) kana))))))

  (nskk-it "foreign extensions in context produce correct compound output"
    (nskk-with-azik-style
      ;; てぃか (tika sound), どぅる (duru sound)
      (should (equal (nskk-convert-romaji "tgika") "てぃか"))
      (should (equal (nskk-convert-romaji "dcika") "でぃか")))))

;;;;
;;;; 12a. 外来語撥音拡張 (Foreign Word Hatsuon Extension) Tests
;;;;

(nskk-describe "AZIK foreign hatsuon extensions (外来語撥音拡張)"
  (nskk-it "tgk converts to てぃん"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgk") "てぃん"))))

  (nskk-it "tgj converts to とぅん"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgj") "とぅん"))))

  (nskk-it "dck converts to でぃん"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dck") "でぃん"))))

  (nskk-it "dcj converts to どぅん"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dcj") "どぅん"))))

  (nskk-it "wsok converts to うぉん"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wsok") "うぉん"))))

  (nskk-it "all foreign hatsuon rules exist in azik-rule/2 Prolog predicate"
    (nskk-with-azik-style
      (dolist (rule '(("tgk" "てぃん") ("tgj" "とぅん")
                      ("dck" "でぃん") ("dcj" "どぅん")
                      ("wsok" "うぉん")))
        (let* ((romaji (car rule))
               (kana   (cadr rule))
               (results (nskk-prolog-query `(azik-rule ,romaji \?k))))
          (should results)
          (should (equal (nskk-prolog-walk '\?k (car results)) kana)))))))

;;;;
;;;; 12b. 外来語二重母音拡張 (Foreign Word Double-Vowel Extension) Tests
;;;;

(nskk-describe "AZIK foreign double-vowel extensions (外来語二重母音拡張)"
  (nskk-it "tgq converts to てぃい"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgq") "てぃい"))))

  (nskk-it "tgh converts to てぃい"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgh") "てぃい"))))

  (nskk-it "tgw converts to とぅう"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgw") "とぅう"))))

  (nskk-it "tgp converts to とぅう"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "tgp") "とぅう"))))

  (nskk-it "dcq converts to でぃい"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dcq") "でぃい"))))

  (nskk-it "dch converts to でぃい"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dch") "でぃい"))))

  (nskk-it "dcw converts to どぅう"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dcw") "どぅう"))))

  (nskk-it "dcp converts to どぅう"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "dcp") "どぅう"))))

  (nskk-it "wsoq converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wsoq") "うぉお"))))

  (nskk-it "wsoh converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wsoh") "うぉお"))))

  (nskk-it "wsow converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wsow") "うぉお"))))

  (nskk-it "wsop converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-convert-romaji "wsop") "うぉお"))))

  (nskk-it "all foreign double-vowel rules exist in azik-rule/2 Prolog predicate"
    (nskk-with-azik-style
      (dolist (rule '(("tgq" "てぃい") ("tgh" "てぃい") ("tgw" "とぅう") ("tgp" "とぅう")
                      ("dcq" "でぃい") ("dch" "でぃい") ("dcw" "どぅう") ("dcp" "どぅう")
                      ("wsoq" "うぉお") ("wsoh" "うぉお") ("wsow" "うぉお") ("wsop" "うぉお")))
        (let* ((romaji (car rule))
               (kana   (cadr rule))
               (results (nskk-prolog-query `(azik-rule ,romaji \?k))))
          (should results)
          (should (equal (nskk-prolog-walk '\?k (car results)) kana)))))))


;;;;
;;;; 13. Compound Rules Tests
;;;;

(nskk-describe "AZIK compound rules (複合ルール)"
  (nskk-it "kak converts to かく"
    (nskk-with-azik-style
      (should (equal (nskk-converter-lookup "kak") "かく"))))

  (nskk-it "kaq converts to かい"
    (nskk-with-azik-style
      (should (equal (nskk-converter-lookup "kaq") "かい"))))

  (nskk-it "kakz converts to かかん"
    (nskk-with-azik-style
      (should (equal (nskk-converter-lookup "kakz") "かかん"))))

  (nskk-it "compound rules enable multi-segment parsing"
    (nskk-with-azik-style
      ;; xh + kak = しゅう + かく = しゅうかく (harvest)
      (should (equal (nskk-convert-romaji "xhkak") "しゅうかく"))
      ;; xh + kaq = しゅう + かい = しゅうかい (meeting)
      (should (equal (nskk-convert-romaji "xhkaq") "しゅうかい"))))

  (nskk-it "all compound rules are absent from azik-rule/2 (hash-only)"
    (nskk-with-azik-style
      ;; These are hash-only: not in Prolog.
      ;; Exception: "wso" is also in azik-rule/2 via foreign-extensions;
      ;; the compound entry only restores the hash mapping after finalize.
      (dolist (rule '(("kak" "かく") ("kaq" "かい") ("kakz" "かかん") ("wso" "うぉ")))
        (let ((romaji (car rule)))
          (unless (equal romaji "wso")
            (should-not (nskk-prolog-query `(azik-rule ,romaji \?k))))))))

  (nskk-it "wso hash entry is restored after finalize via compound rules"
    (nskk-with-azik-style
      ;; wso is demoted to :incomplete by finalize (non-vowel extensions
      ;; from wsok, wsoq, etc.), then restored by compound-rules step.
      (should (equal (nskk-converter-lookup "wso") "うぉ")))))


;;;;
;;;; 14. Property-Based Tests
;;;;

(nskk-describe "AZIK property-based: extension row consistency"
  ;; These properties use `nskk-converter-lookup' (hash-only) rather than
  ;; `nskk-convert-romaji' to bypass special pipeline rules that interfere:
  ;; - The n-row: "n" before a non-vowel triggers the special ん rule,
  ;;   so "nz" → "ん"+"z" in the pipeline rather than the AZIK "なん".
  ;; - The gg/jg rows: "gg" triggers the double-consonant rule in pipeline.
  ;; Hash lookup is the authoritative source for AZIK rule existence.

  (nskk-it "z-key hatsuon rule exists for every consonant row (A+ん)"
    (nskk-with-azik-style
      (dolist (row nskk--azik-extension-rows)
        (let* ((prefix   (car row))
               (a        (cadr row))
               (expected (concat a "ん"))
               (actual   (nskk-converter-lookup (concat prefix "z"))))
          (should (equal actual expected))))))

  (nskk-it "q-key diphthong rule exists for every consonant row (A+い)"
    (nskk-with-azik-style
      (dolist (row nskk--azik-extension-rows)
        (let* ((prefix   (car row))
               (a        (cadr row))
               (expected (concat a "い"))
               (actual   (nskk-converter-lookup (concat prefix "q"))))
          (should (equal actual expected))))))

  (nskk-it "youon a-key rule exists for every youon row"
    (nskk-with-azik-style
      (dolist (row nskk--azik-youon-rows)
        (let* ((prefix (car row))
               (a      (cadr row))
               (actual (nskk-converter-lookup (concat prefix "a"))))
          (should (equal actual a))))))

  (nskk-it "youon z-key hatsuon rule exists for every youon row (A+ん)"
    (nskk-with-azik-style
      (dolist (row nskk--azik-youon-rows)
        (let* ((prefix   (car row))
               (a        (cadr row))
               (expected (concat a "ん"))
               (actual   (nskk-converter-lookup (concat prefix "z"))))
          (should (equal actual expected))))))

  (nskk-it "all same-finger rules exist in azik-rule/2"
    (nskk-with-azik-style
      (dolist (rule '(("kf" "き") ("hf" "ふ") ("nf" "ぬ") ("mf" "む") ("gf" "ぐ")
                      ("pf" "ぷ") ("rf" "る") ("yf" "ゆ")))
        (let* ((romaji  (car rule))
               (kana    (cadr rule))
               (results (nskk-prolog-query `(azik-rule ,romaji \?k))))
          (should results)
          (should (equal (nskk-prolog-walk '\?k (car results)) kana))))))

  (nskk-it "k-row hatsuon: full pipeline converts correctly"
    (nskk-with-azik-style
      (dolist (pair '(("kz" . "かん") ("kk" . "きん") ("kj" . "くん")
                      ("kd" . "けん") ("kl" . "こん")))
        (should (equal (cdr pair) (nskk-convert-romaji (car pair)))))))

  (nskk-it "k-row diphthong: full pipeline converts correctly"
    (nskk-with-azik-style
      (dolist (pair '(("kq" . "かい") ("kh" . "くう") ("kw" . "けい")
                      ("kp" . "こう")))
        (should (equal (cdr pair) (nskk-convert-romaji (car pair))))))))


;;;;
;;;; 15. Data-Provider: AZIK hatsuon (撥音拡張) categories
;;;;

;; k-row: kz→かん, kk→きん, kj→くん, kd→けん, kl→こん
(nskk-deftest-table azik-hatsuon-k-row
  :description "AZIK k-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("kz" "かん")
         ("kk" "きん")
         ("kj" "くん")
         ("kd" "けん")
         ("kl" "こん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; s-row: sz→さん, sk→しん, sj→すん, sd→せん, sl→そん
(nskk-deftest-table azik-hatsuon-s-row
  :description "AZIK s-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("sz" "さん")
         ("sk" "しん")
         ("sj" "すん")
         ("sd" "せん")
         ("sl" "そん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; t-row: tz→たん, tk→ちん, tj→つん, td→てん, tl→とん
(nskk-deftest-table azik-hatsuon-t-row
  :description "AZIK t-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("tz" "たん")
         ("tk" "ちん")
         ("tj" "つん")
         ("td" "てん")
         ("tl" "とん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; g-row: gz→がん, gk→ぎん, gj→ぐん, gd→げん, gl→ごん
(nskk-deftest-table azik-hatsuon-g-row
  :description "AZIK g-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("gz" "がん")
         ("gk" "ぎん")
         ("gj" "ぐん")
         ("gd" "げん")
         ("gl" "ごん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; h-row: hz→はん, hk→ひん, hj→ふん, hd→へん, hl→ほん
(nskk-deftest-table azik-hatsuon-h-row
  :description "AZIK h-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("hz" "はん")
         ("hk" "ひん")
         ("hj" "ふん")
         ("hd" "へん")
         ("hl" "ほん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; m-row: mz→まん, mk→みん, mj→むん, md→めん, ml→もん
(nskk-deftest-table azik-hatsuon-m-row
  :description "AZIK m-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("mz" "まん")
         ("mk" "みん")
         ("mj" "むん")
         ("md" "めん")
         ("ml" "もん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; y-row: yz→やん, yk→いん, yj→ゆん, yd→えん, yl→よん
(nskk-deftest-table azik-hatsuon-y-row
  :description "AZIK y-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("yz" "やん")
         ("yk" "いん")
         ("yj" "ゆん")
         ("yd" "えん")
         ("yl" "よん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; r-row: rz→らん, rk→りん, rj→るん, rd→れん, rl→ろん
(nskk-deftest-table azik-hatsuon-r-row
  :description "AZIK r-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("rz" "らん")
         ("rk" "りん")
         ("rj" "るん")
         ("rd" "れん")
         ("rl" "ろん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; w-row: wz→わん, wk→うぃん, wj→うん, wd→うぇん, wl→をん
(nskk-deftest-table azik-hatsuon-w-row
  :description "AZIK w-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("wz" "わん")
         ("wk" "うぃん")
         ("wj" "うん")
         ("wd" "うぇん")
         ("wl" "をん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; b-row: bz→ばん, bk→びん, bj→ぶん, bd→べん, bl→ぼん
(nskk-deftest-table azik-hatsuon-b-row
  :description "AZIK b-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("bz" "ばん")
         ("bk" "びん")
         ("bj" "ぶん")
         ("bd" "べん")
         ("bl" "ぼん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; p-row: pz→ぱん, pk→ぴん, pj→ぷん, pd→ぺん, pl→ぽん
(nskk-deftest-table azik-hatsuon-p-row
  :description "AZIK p-row 撥音拡張: each pattern converts correctly via hash lookup"
  :columns (input expected)
  :rows (("pz" "ぱん")
         ("pk" "ぴん")
         ("pj" "ぷん")
         ("pd" "ぺん")
         ("pl" "ぽん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; z-row: zz→ざん, zk→じん, zj→ずん, zd→ぜん, zl→ぞん
(nskk-deftest-table azik-hatsuon-z-row
  :description "AZIK z-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("zz" "ざん")
         ("zk" "じん")
         ("zj" "ずん")
         ("zd" "ぜん")
         ("zl" "ぞん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; d-row: dz→だん, dk→ぢん, dj→づん, dd→でん, dl→どん
(nskk-deftest-table azik-hatsuon-d-row
  :description "AZIK d-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("dz" "だん")
         ("dk" "ぢん")
         ("dj" "づん")
         ("dd" "でん")
         ("dl" "どん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; x-row: xz→しゃん, xj→しゅん, xd→しぇん, xl→しょん
;; (xk is demoted to :incomplete due to standard-romaji xka/xke entries)
(nskk-deftest-table azik-hatsuon-x-row
  :description "AZIK x-row 撥音拡張: complete (non-demoted) entries in hash lookup"
  :columns (input expected)
  :rows (("xz" "しゃん")
         ("xj" "しゅん")
         ("xd" "しぇん")
         ("xl" "しょん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; c-row: cz→ちゃん, ck→ちん, cj→ちゅん, cd→ちぇん, cl→ちょん
(nskk-deftest-table azik-hatsuon-c-row
  :description "AZIK c-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("cz" "ちゃん")
         ("ck" "ちん")
         ("cj" "ちゅん")
         ("cd" "ちぇん")
         ("cl" "ちょん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; f-row: fz→ふぁん, fk→ふぃん, fj→ふん, fd→ふぇん, fl→ふぉん
(nskk-deftest-table azik-hatsuon-f-row
  :description "AZIK f-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("fz" "ふぁん")
         ("fk" "ふぃん")
         ("fj" "ふん")
         ("fd" "ふぇん")
         ("fl" "ふぉん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; j-row: jz→じゃん, jk→じん, jj→じゅん, jd→じぇん, jl→じょん
(nskk-deftest-table azik-hatsuon-j-row
  :description "AZIK j-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("jz" "じゃん")
         ("jk" "じん")
         ("jj" "じゅん")
         ("jd" "じぇん")
         ("jl" "じょん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

;; v-row: vz→ゔぁん, vk→ゔぃん, vj→ゔん, vd→ゔぇん, vl→ゔぉん
(nskk-deftest-table azik-hatsuon-v-row
  :description "AZIK v-row 撥音拡張: each pattern exists in hash lookup"
  :columns (input expected)
  :rows (("vz" "ゔぁん")
         ("vk" "ゔぃん")
         ("vj" "ゔん")
         ("vd" "ゔぇん")
         ("vl" "ゔぉん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))


;;;;
;;;; 16. Data-Provider: AZIK youon (拗音互換キー) table
;;;;

;; Rows: g-substitution (consonant + "g") and y-prefix (consonant + "y") youon.
;; Columns: a/u/e/o keys produce the contracted-sound forms.
(nskk-deftest-table azik-youon-rules
  :columns (prefix a-expected u-expected e-expected o-expected)
  :rows    (("ng" "にゃ" "にゅ" "にぇ" "にょ")
            ("kg" "きゃ" "きゅ" "きぇ" "きょ")
            ("hg" "ひゃ" "ひゅ" "ひぇ" "ひょ")
            ("mg" "みゃ" "みゅ" "みぇ" "みょ")
            ("rg" "りゃ" "りゅ" "りぇ" "りょ")
            ("gg" "ぎゃ" "ぎゅ" "ぎぇ" "ぎょ")
            ("jg" "じゃ" "じゅ" "じぇ" "じょ")
            ("bg" "びゃ" "びゅ" "びぇ" "びょ")
            ("pg" "ぴゃ" "ぴゅ" "ぴぇ" "ぴょ")
            ;; y-prefix rows (DDSKK-compatible standard romaji youon + AZIK extensions)
            ("ny" "にゃ" "にゅ" "にぇ" "にょ")
            ("ky" "きゃ" "きゅ" "きぇ" "きょ")
            ("hy" "ひゃ" "ひゅ" "ひぇ" "ひょ")
            ("my" "みゃ" "みゅ" "みぇ" "みょ")
            ("ry" "りゃ" "りゅ" "りぇ" "りょ")
            ("gy" "ぎゃ" "ぎゅ" "ぎぇ" "ぎょ")
            ("jy" "じゃ" "じゅ" "じぇ" "じょ")
            ("by" "びゃ" "びゅ" "びぇ" "びょ")
            ("py" "ぴゃ" "ぴゅ" "ぴぇ" "ぴょ"))
  :description "AZIK youon rules: a/u/e/o keys produce correct contracted-sound forms"
  :body (nskk-with-azik-style
          (should (equal a-expected (nskk-converter-lookup (concat prefix "a"))))
          (should (equal u-expected (nskk-converter-lookup (concat prefix "u"))))
          (should (equal e-expected (nskk-converter-lookup (concat prefix "e"))))
          (should (equal o-expected (nskk-converter-lookup (concat prefix "o"))))))


;;;;
;;;; Data-Provider: AZIK diphthong (二重母音拡張) table
;;;;

;; Each row represents a consonant prefix; columns are the four diphthong keys.
;; q → A+い, h → U+う, w → E+い, p → O+う
;; Note: "s" and "t" rows use the azik-vowel-deferred mechanism for the h-key
;; because "sh"/"th" are proper prefixes of vowel-only-shadowed entries.
;; The h-key rules are COMPLETE (not :incomplete) and use the deferred correction.
(nskk-deftest-table azik-diphthong-rules
  :columns (prefix q-expected h-expected w-expected p-expected)
  :rows    (("k" "かい" "くう" "けい" "こう")
            ("s" "さい" "すう" "せい" "そう")
            ("t" "たい" "つう" "てい" "とう")
            ("n" "ない" "ぬう" "ねい" "のう")
            ("m" "まい" "むう" "めい" "もう")
            ("r" "らい" "るう" "れい" "ろう"))
  :description "AZIK diphthong rules: q/h/w/p keys produce correct vowel extensions"
  :body (nskk-with-azik-style
          (should (equal q-expected (nskk-converter-lookup (concat prefix "q"))))
          (should (equal h-expected (nskk-converter-lookup (concat prefix "h"))))
          (should (equal w-expected (nskk-converter-lookup (concat prefix "w"))))
          (should (equal p-expected (nskk-converter-lookup (concat prefix "p"))))))


;;;;
;;;; 17. Data-Provider: AZIK diphthong (二重母音拡張) table
;;;;

;; Columns: q=A+い, h=U+う, w=E+い, p=O+う
;; Rows where h-col is :demoted mean the h-key is :incomplete (shadowed by standard romaji).
;; Use nskk-converter-lookup for all columns to bypass pipeline interference.
(nskk-deftest-table azik-diphthong-full-rows
  :columns (prefix q-expected h-expected w-expected p-expected)
  :rows    (("k" "かい" "くう" "けい" "こう")
            ("m" "まい" "むう" "めい" "もう")
            ("y" "やい" "ゆう" "えい" "よう")
            ("r" "らい" "るう" "れい" "ろう")
            ("g" "がい" "ぐう" "げい" "ごう")
            ("z" "ざい" "ずう" "ぜい" "ぞう")
            ("b" "ばい" "ぶう" "べい" "ぼう")
            ("f" "ふぁい" "ふう" "ふぇい" "ふぉう")
            ("j" "じゃい" "じゅう" "じぇい" "じょう")
            ("v" "ゔぁい" "ゔう" "ゔぇい" "ゔぉう"))
  :description "AZIK diphthong rules: q/h/w/p keys produce correct double-vowel forms"
  :body (nskk-with-azik-style
          (should (equal q-expected (nskk-converter-lookup (concat prefix "q"))))
          (should (equal h-expected (nskk-converter-lookup (concat prefix "h"))))
          (should (equal w-expected (nskk-converter-lookup (concat prefix "w"))))
          (should (equal p-expected (nskk-converter-lookup (concat prefix "p"))))))


;;;;
;;;; Contract-Based PBT: nskk-convert-romaji
;;;;

;; Verify that nskk-convert-romaji always returns a string for any AZIK pattern.
;; Each iteration isolates Prolog DB + romaji table + keymap via nskk-with-azik-style
;; to prevent AZIK's keymap binding (@ → nskk-toggle-japanese-mode) from leaking
;; into subsequent non-AZIK tests.
(nskk-describe "nskk-convert-romaji AZIK contract"
  (nskk-it "should return a non-empty string for any AZIK pattern"
    (let ((failures nil))
      (dotimes (_ 50)
        (nskk-with-azik-style
          (let* ((input  (nskk--pbt-random-choice (nskk--pbt-get-all-azik-patterns)))
                 (result (nskk-convert-romaji input)))
            (unless (stringp result)
              (push (list :postcondition-failed :input input :result result) failures))
            (unless (> (length result) 0)
              (push (list :invariant-failed "result is empty string" input) failures)))))
      (when failures
        (ert-fail (format "Contract test `nskk-convert-romaji' AZIK: %d failures:\n%S"
                          (length failures) failures))))))


;;;;
;;;; Property-Based Tests: AZIK-wide invariants
;;;;

(nskk-property-test azik-any-rule-lookup-returns-valid-type
  ((pattern azik-rule))
  ;; Any sampled AZIK pattern must return a string, :incomplete, or nil from the
  ;; hash — never an unexpected type.  The test isolates state via nskk-with-azik-style.
  (nskk-with-azik-style
    (let ((result (nskk-converter-lookup pattern)))
      (or (stringp result)
          (eq result :incomplete)
          (null result))))
  50)

(nskk-property-test azik-hatsuon-z-suffix-produces-string-ending-in-ん
  ((row-pattern azik-rule))  ; used to drive random iteration count only
  ;; nskk-with-azik-style saves/restores nskk--romaji-table so that AZIK
  ;; rules do not leak into subsequent tests (unlike bare nskk-prolog-test-
  ;; with-isolated-db which only restores the Prolog clause database).
  (nskk-with-azik-style
    ;; Sample specifically from hatsuon categories for meaningful coverage
    (let* ((hatsuon-categories (cl-remove-if-not
                                (lambda (cat) (string-prefix-p "hatsuon-" (symbol-name (car cat))))
                                nskk--pbt-azik-categories))
           (category (nskk--pbt-random-choice hatsuon-categories))
           (patterns (cdr category))
           ;; z-key patterns are the FIRST entry in each hatsuon category
           (z-pattern (car patterns))
           (result (nskk-converter-lookup z-pattern)))
      (and (stringp result)
           (string-suffix-p "ん" result))))
  50)


;;;;
;;;; Exhaustive Property Test: entire AZIK rule set never crashes on lookup
;;;;

;; Iterate every pattern in nskk--pbt-azik-categories and assert that
;; nskk-converter-lookup returns without signaling an error.
;; The bound variable in nskk-property-test-exhaustive is `item'.
(nskk-property-test-exhaustive azik-all-rules-lookup-non-crashing
  (nskk-with-azik-style
    (nskk--pbt-get-all-azik-patterns))
  (nskk-with-azik-style
    (let ((result (condition-case err
                      (nskk-converter-lookup item)
                    (error (cons :error err)))))
      (not (and (consp result) (eq (car result) :error))))))


;;;;
;;;; CPS Tests: /k suffix functions
;;;;

(nskk-describe "AZIK CPS initialization: nskk--init-azik-rules/k"
  (nskk-it "calls on-done continuation exactly once"
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (let ((call-count 0))
              (nskk--init-azik-rules/k
               (lambda ()
                 (cl-incf call-count)))
              (should (= call-count 1))))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table))))

  (nskk-it "on-done is called after hash table is populated"
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (let ((hash-populated nil))
              (nskk--init-azik-rules/k
               (lambda ()
                 ;; At call time the AZIK rules must already be in the hash
                 (setq hash-populated
                       (stringp (nskk-converter-lookup "kz")))))
              (should hash-populated)))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table)))))

(nskk-describe "azik-vowel-char/1 Prolog predicate"
  (nskk-it "succeeds for each of the five romaji vowel character codes"
    (nskk-prolog-test-with-isolated-db
      (nskk--azik-init-char-facts)
      (should (nskk-prolog-holds-p `(azik-vowel-char ,?a)))
      (should (nskk-prolog-holds-p `(azik-vowel-char ,?i)))
      (should (nskk-prolog-holds-p `(azik-vowel-char ,?u)))
      (should (nskk-prolog-holds-p `(azik-vowel-char ,?e)))
      (should (nskk-prolog-holds-p `(azik-vowel-char ,?o)))))

  (nskk-it "fails for consonant character codes"
    (nskk-prolog-test-with-isolated-db
      (nskk--azik-init-char-facts)
      (should-not (nskk-prolog-holds-p `(azik-vowel-char ,?k)))
      (should-not (nskk-prolog-holds-p `(azik-vowel-char ,?s)))
      (should-not (nskk-prolog-holds-p `(azik-vowel-char ,?n)))))

  (nskk-it "enumerates exactly 5 solutions"
    (nskk-prolog-test-with-isolated-db
      (nskk--azik-init-char-facts)
      (should (= 5 (length (nskk-prolog-query-all-values
                            '(azik-vowel-char \?ch) '\?ch)))))))

(nskk-describe "azik-key-extends/2 Prolog predicate"
  (nskk-it "asserts prefix-extension pairs from the hash after AZIK init"
    ;; "sh"→"すう" in hash; "sha","shi","shu","she","sho" also in hash
    ;; so azik-key-extends("sh", ?a) etc. should hold
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?a)))
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?i)))
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?u)))))

  (nskk-it "does not assert pairs for keys with no longer hash entry"
    ;; "kz"→"かん" has no "kza","kzb",... in hash
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p `(azik-key-extends "kz" \?ch)))))

  (nskk-it "deduplicates: same (prefix, ch) pair asserted at most once"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table nskk--romaji-table)))
        (unwind-protect
            (progn
              ;; Two hash keys share prefix "k" + char ?a: "ka" and "kab"
              ;; both contribute azik-key-extends("k", ?a).
              (puthash "ka"  "か"   nskk--romaji-table)
              (puthash "kab" "かあ" nskk--romaji-table)
              (nskk--azik-init-key-extend-facts)
              ;; Must yield exactly 1 solution for ("k", ?a), not 2.
              (should (= 1 (length (nskk-prolog-query
                                    `(azik-key-extends "k" ,?a))))))
          (clrhash nskk--romaji-table)
          (maphash (lambda (k v) (puthash k v nskk--romaji-table)) saved))))))

(nskk-describe "azik-nonvowel-ext/1 Prolog rule"
  (nskk-it "succeeds for a prefix that has at least one non-vowel extension"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table nskk--romaji-table)))
        (unwind-protect
            (progn
              (nskk--azik-init-char-facts)
              (puthash "tx"  "てすと"  nskk--romaji-table)
              (puthash "txk" "てすとん" nskk--romaji-table)
              (nskk--azik-init-key-extend-facts)
              (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
              (nskk-prolog-<- (azik-nonvowel-ext \?k)
                (azik-key-extends \?k \?ch)
                (not (azik-vowel-char \?ch)))
              (should (nskk-prolog-holds-p '(azik-nonvowel-ext "tx"))))
          (clrhash nskk--romaji-table)
          (maphash (lambda (k v) (puthash k v nskk--romaji-table)) saved)))))

  (nskk-it "fails for a prefix whose extensions are all vowels"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table nskk--romaji-table)))
        (unwind-protect
            (progn
              (nskk--azik-init-char-facts)
              (puthash "sh"  "すう" nskk--romaji-table)
              (puthash "sha" "しゃ" nskk--romaji-table)
              (puthash "shi" "し"   nskk--romaji-table)
              (nskk--azik-init-key-extend-facts)
              (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
              (nskk-prolog-<- (azik-nonvowel-ext \?k)
                (azik-key-extends \?k \?ch)
                (not (azik-vowel-char \?ch)))
              (should-not (nskk-prolog-holds-p '(azik-nonvowel-ext "sh"))))
          (clrhash nskk--romaji-table)
          (maphash (lambda (k v) (puthash k v nskk--romaji-table)) saved))))))

(nskk-describe "azik-vowel-shadow/1 Prolog rule"
  (nskk-it "holds for \"sh\" after AZIK init — vowel-only extensions"
    ;; "sh"→"すう" is shadowed by "sha"/"shi"/"shu"/"she"/"sho" (all vowels)
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-vowel-shadow "sh")))))

  (nskk-it "holds for \"ch\" after AZIK init — vowel-only extensions"
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-vowel-shadow "ch")))))

  (nskk-it "does not hold for keys with no extensions (e.g. \"kz\")"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-vowel-shadow "kz")))))

  (nskk-it "does not hold for non-AZIK complete rules"
    ;; "ka"→"か" is in the hash but is not in azik-rule/2 as an AZIK-specific
    ;; rule; azik-vowel-shadow requires azik-rule(K, _) to hold.
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-vowel-shadow "ka"))))))

(nskk-describe "nskk--azik-classify-key/k"
  (nskk-it "calls succeed with :vowel-shadow for \"sh\""
    ;; "sh"→"すう" with vowel-only longer entries (sha/shi/shu/she/sho)
    (nskk-with-azik-style
      (let ((result :not-called))
        (nskk--azik-classify-key/k "sh"
          (lambda (kind) (setq result kind))
          #'ignore)
        (should (eq result :vowel-shadow)))))

  (nskk-it "calls fail for \"kz\" which has no longer extensions"
    (nskk-with-azik-style
      (let ((fail-called nil))
        (nskk--azik-classify-key/k "kz"
          (lambda (_kind) nil)
          (lambda () (setq fail-called t)))
        (should fail-called))))

  (nskk-it "calls succeed with :incomplete for a key with non-vowel extensions"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table nskk--romaji-table)))
        (unwind-protect
            (progn
              (nskk-prolog-retract-all 'azik-rule 2)
              (nskk-prolog-set-index 'azik-rule 2 :hash)
              (nskk-prolog-assert '((azik-rule "tx" "てすと")))
              (puthash "tx"  "てすと"  nskk--romaji-table)
              (puthash "txk" "てすとん" nskk--romaji-table)
              (nskk--azik-init-char-facts)
              (nskk--azik-init-key-extend-facts)
              (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
              (nskk-prolog-<- (azik-nonvowel-ext \?k)
                (azik-key-extends \?k \?ch)
                (not (azik-vowel-char \?ch)))
              (nskk-prolog-retract-all 'azik-vowel-shadow 1)
              (nskk-prolog-<- (azik-vowel-shadow \?k)
                (azik-rule \?k \?_kana)
                (azik-key-extends \?k \?_ext)
                (not (azik-nonvowel-ext \?k)))
              (let ((result :not-called))
                (nskk--azik-classify-key/k "tx"
                  (lambda (kind) (setq result kind))
                  #'ignore)
                (should (eq result :incomplete))))
          (clrhash nskk--romaji-table)
          (maphash (lambda (k v) (puthash k v nskk--romaji-table)) saved))))))

(nskk-describe "nskk--azik-finalize-hash-table"
  (nskk-it "calls on-done continuation exactly once"
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (let ((call-count 0))
              (nskk--azik-finalize-hash-table/k
               (lambda () (cl-incf call-count)))
              (should (= call-count 1))))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table))))

  (nskk-it "\"sh\" remains complete in hash after finalize (vowel-shadow)"
    ;; azik-vowel-deferred mechanism handles sha→しゃ at input time,
    ;; so "sh" stays as "すう" in the hash — never demoted to :incomplete.
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (equal (nskk-converter-lookup "sh") "すう")))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table))))

  (nskk-it "\"sh\" is recorded in nskk--azik-vowel-shadow-set after finalize"
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (gethash "sh" nskk--azik-vowel-shadow-set)))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table))))

  (nskk-it "registers :incomplete for proper prefixes of AZIK rules"
    ;; "kg" is a prefix of "kga","kgu","kge","kgo","kgz",... produced by
    ;; nskk-azik-youon.  It must be :incomplete so the converter waits.
    (let ((saved-romaji-table (copy-hash-table nskk--romaji-table)))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (eq (gethash "kg" nskk--romaji-table) :incomplete)))
        (clrhash nskk--romaji-table)
        (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                 saved-romaji-table)))))

(nskk-property-test converter-convert/k-azik-exactly-one-branch-called
  ((pattern azik-rule))
  ;; For every sampled AZIK pattern, nskk-converter-convert/k must call exactly
  ;; one of the three continuations (on-match, on-incomplete, on-fail).
  (nskk-with-azik-style
    (let ((branch-called nil))
      (nskk-converter-convert/k
       pattern
       (lambda (_kana _rest)  (setq branch-called 'match))
       (lambda (_romaji)      (setq branch-called 'incomplete))
       (lambda ()             (setq branch-called 'fail)))
      ;; branch-called must be one of the three valid symbols
      (memq branch-called '(match incomplete fail))))
  50)





;;;
;;; AZIK macro API (nskk-azik-hatsuon, nskk-azik-double-vowel,
;;;                  nskk-azik-extensions, nskk-azik-youon,
;;;                  nskk--azik-init-extension-rows, nskk--azik-init-youon-rows)
;;;

(nskk-describe "nskk-azik-hatsuon"
  (nskk-it "is a macro"
    (should (macrop 'nskk-azik-hatsuon)))

  (nskk-it "asserts hatsuon rules as azik-rule/2 Prolog facts"
    (nskk-prolog-test-with-isolated-db
      ;; ka-row: a=か i=き u=く e=け o=こ
      (nskk-azik-hatsuon "k" "か" "き" "く" "け" "こ")
      ;; prefix+z → a+ん = かん
      (should (equal (nskk-prolog-query-value '(azik-rule "kz" \?v) '\?v) "かん"))
      ;; prefix+k → i+ん = きん
      (should (equal (nskk-prolog-query-value '(azik-rule "kk" \?v) '\?v) "きん"))
      ;; prefix+l → o+ん = こん
      (should (equal (nskk-prolog-query-value '(azik-rule "kl" \?v) '\?v) "こん"))))

  (nskk-it "generates exactly 5 hatsuon rules per prefix"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'azik-rule 2)
      (nskk-azik-hatsuon "s" "さ" "し" "す" "せ" "そ")
      (let ((rules (nskk-prolog-query-all-values '(azik-rule \?k \?v) '\?k)))
        (should (= (length rules) 5))))))

(nskk-describe "nskk-azik-double-vowel"
  (nskk-it "is a macro"
    (should (macrop 'nskk-azik-double-vowel)))

  (nskk-it "asserts double vowel rules as azik-rule/2 Prolog facts"
    (nskk-prolog-test-with-isolated-db
      ;; ka-row: a=か u=く e=け o=こ
      (nskk-azik-double-vowel "k" "か" "く" "け" "こ")
      ;; prefix+q → a+い = かい
      (should (equal (nskk-prolog-query-value '(azik-rule "kq" \?v) '\?v) "かい"))
      ;; prefix+h → u+う = くう
      (should (equal (nskk-prolog-query-value '(azik-rule "kh" \?v) '\?v) "くう"))))

  (nskk-it "generates exactly 4 double vowel rules per prefix"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'azik-rule 2)
      (nskk-azik-double-vowel "t" "た" "つ" "て" "と")
      (let ((rules (nskk-prolog-query-all-values '(azik-rule \?k \?v) '\?k)))
        (should (= (length rules) 4))))))

(nskk-describe "nskk-azik-extensions"
  (nskk-it "is a macro"
    (should (macrop 'nskk-azik-extensions)))

  (nskk-it "generates 9 rules: 5 hatsuon + 4 double vowel"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'azik-rule 2)
      (nskk-azik-extensions "n" "な" "に" "ぬ" "ね" "の")
      (let ((rules (nskk-prolog-query-all-values '(azik-rule \?k \?v) '\?k)))
        (should (= (length rules) 9))))))

(nskk-describe "nskk-azik-youon"
  (nskk-it "is a macro"
    (should (macrop 'nskk-azik-youon)))

  (nskk-it "asserts base rules for a/u/e/o positions"
    (nskk-prolog-test-with-isolated-db
      (nskk-azik-youon "kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
      ;; Base rules: kga→きゃ, kgu→きゅ, kge→きぇ, kgo→きょ
      (should (equal (nskk-prolog-query-value '(azik-rule "kga" \?v) '\?v) "きゃ"))
      (should (equal (nskk-prolog-query-value '(azik-rule "kgu" \?v) '\?v) "きゅ"))
      (should (equal (nskk-prolog-query-value '(azik-rule "kgo" \?v) '\?v) "きょ"))))

  (nskk-it "also generates hatsuon and double vowel extension rules"
    (nskk-prolog-test-with-isolated-db
      (nskk-azik-youon "kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
      ;; hatsuon: kgz→きゃん
      (should (equal (nskk-prolog-query-value '(azik-rule "kgz" \?v) '\?v) "きゃん"))
      ;; double vowel: kgq→きゃい
      (should (equal (nskk-prolog-query-value '(azik-rule "kgq" \?v) '\?v) "きゃい")))))

(nskk-describe "nskk--azik-init-extension-rows"
  (nskk-it "is a macro"
    (should (macrop 'nskk--azik-init-extension-rows))))

(nskk-describe "nskk--azik-init-youon-rows"
  (nskk-it "is a macro"
    (should (macrop 'nskk--azik-init-youon-rows))))

;;;;
;;;; Section: JP106 + Key Rules
;;;;

(nskk-describe "AZIK JP106 + key rules"
  (nskk-it "JP106 rules include + → っ"
    (let ((nskk-azik-keyboard-type 'jp106))
      ;; Reinitialize to pick up JP106 rules
      (nskk-with-azik-style
        (should (equal "っ" (gethash "+" nskk--romaji-table))))))

  (nskk-it "US101 rules do not include + → っ"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-with-azik-style
        (should-not (gethash "+" nskk--romaji-table))))))

(nskk-describe "azik-removed-internals"
  (nskk-it "nskk--azik-assert-rules should not exist (removed in refactoring)"
    (should-not (fboundp 'nskk--azik-assert-rules))))

;;;;
;;;; azik-colon-trigger-char/1 Prolog fact Tests
;;;;

(nskk-describe "azik-colon-trigger-char/1 Prolog fact"
  (nskk-it "should register colon as a trigger char"
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-colon-trigger-char ?:)))))
  (nskk-it "should register plus as a trigger char"
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-colon-trigger-char ?+)))))
  (nskk-it "should NOT register other chars like a"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-colon-trigger-char ?a))))))

(provide 'nskk-azik-test)

;;; nskk-azik-test.el ends here
