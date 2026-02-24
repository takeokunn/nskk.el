;;; nskk-azik-test.el --- Tests for AZIK extended romaji input -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 takeokunn
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

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-converter)
(require 'nskk-azik)
(eval-when-compile (require 'cl-lib))


;;;;
;;;; Helper Macros for AZIK Tests
;;;;

(defmacro nskk-with-azik-style (&rest body)
  "Execute BODY with AZIK style loaded."
  (declare (indent 0))
  `(progn
     (nskk-converter-load-style 'azik)
     ,@body))

(defmacro nskk-with-standard-style (&rest body)
  "Execute BODY with standard style loaded."
  (declare (indent 0))
  `(progn
     (nskk-converter-load-style 'standard)
     ,@body))


;;;;
;;;; 1. Style Switching Tests
;;;;

(ert-deftest nskk-azik-load-standard-style ()
  "Test loading standard romaji style."
  (should (eq (nskk-converter-load-style 'standard) 'standard))
  ;; Standard style should have basic romaji
  (should (equal (nskk-convert-romaji "ka") "か"))
  (should (equal (nskk-convert-romaji "shi") "し")))

(ert-deftest nskk-azik-load-azik-style ()
  "Test loading AZIK romaji style."
  (should (eq (nskk-converter-load-style 'azik) 'azik))
  ;; AZIK style should have extended rules
  (should (equal (nskk-convert-romaji "kz") "かん"))
  (should (equal (nskk-convert-romaji "kq") "かい")))

(ert-deftest nskk-azik-style-switching ()
  "Test switching between standard and AZIK styles."
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
  (should (equal (nskk-convert-romaji "ka") "か")))


;;;;
;;;; 2. Special Keys Tests
;;;;

(ert-deftest nskk-azik-special-key-small-tsu ()
  "Test that ';' produces small tsu in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji ";") "っ"))))

(ert-deftest nskk-azik-special-key-chouon ()
  "Test that ':' produces chouon (long vowel mark) in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji ":") "ー"))))

(ert-deftest nskk-azik-special-keys-in-context ()
  "Test special keys in context with other characters."
  (nskk-with-azik-style
    ;; "っか" using semicolon
    (should (equal (nskk-convert-romaji ";ka") "っか"))
    ;; "かー" using colon
    (should (equal (nskk-convert-romaji "ka:") "かー"))))


;;;;
;;;; 3. 撥音拡張 (Mora Nasal Extension) Tests
;;;;

(ert-deftest nskk-azik-hatsuon-k-row ()
  "Test 撥音拡張 for k-row (kz, kk, kj, kd, kl).
Note: kk is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "kz") "かん"))
    (should (equal (nskk-converter-lookup "kk") "きん"))
    (should (equal (nskk-converter-lookup "kj") "くん"))
    (should (equal (nskk-converter-lookup "kd") "けん"))
    (should (equal (nskk-converter-lookup "kl") "こん"))
    ;; kz, kj, kd, kl should work in conversion
    (should (equal (nskk-convert-romaji "kz") "かん"))
    (should (equal (nskk-convert-romaji "kj") "くん"))
    (should (equal (nskk-convert-romaji "kd") "けん"))
    (should (equal (nskk-convert-romaji "kl") "こん"))))

(ert-deftest nskk-azik-hatsuon-s-row ()
  "Test 撥音拡張 for s-row (sz, sk, sj, sd, sl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "sz") "さん"))
    (should (equal (nskk-convert-romaji "sk") "しん"))
    (should (equal (nskk-convert-romaji "sj") "すん"))
    (should (equal (nskk-convert-romaji "sd") "せん"))
    (should (equal (nskk-convert-romaji "sl") "そん"))))

(ert-deftest nskk-azik-hatsuon-t-row ()
  "Test 撥音拡張 for t-row (tz, tk, tj, td, tl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "tz") "たん"))
    (should (equal (nskk-convert-romaji "tk") "ちん"))
    (should (equal (nskk-convert-romaji "tj") "つん"))
    (should (equal (nskk-convert-romaji "td") "てん"))
    (should (equal (nskk-convert-romaji "tl") "とん"))))

(ert-deftest nskk-azik-hatsuon-n-row ()
  "Test 撥音拡張 for n-row (nz, nk, nj, nd, nl).
Note: The converter has special handling for 'n' before consonants.
Rules exist in the table but 'n' + consonant is converted to ん first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "nz") "なん"))
    (should (equal (nskk-converter-lookup "nk") "にん"))
    (should (equal (nskk-converter-lookup "nj") "ぬん"))
    (should (equal (nskk-converter-lookup "nd") "ねん"))
    (should (equal (nskk-converter-lookup "nl") "のん"))))

(ert-deftest nskk-azik-hatsuon-h-row ()
  "Test 撥音拡張 for h-row (hz, hk, hj, hd, hl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "hz") "はん"))
    (should (equal (nskk-convert-romaji "hk") "ひん"))
    (should (equal (nskk-convert-romaji "hj") "ふん"))
    (should (equal (nskk-convert-romaji "hd") "へん"))
    (should (equal (nskk-convert-romaji "hl") "ほん"))))

(ert-deftest nskk-azik-hatsuon-m-row ()
  "Test 撥音拡張 for m-row (mz, mk, mj, md, ml)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "mz") "まん"))
    (should (equal (nskk-convert-romaji "mk") "みん"))
    (should (equal (nskk-convert-romaji "mj") "むん"))
    (should (equal (nskk-convert-romaji "md") "めん"))
    (should (equal (nskk-convert-romaji "ml") "もん"))))

(ert-deftest nskk-azik-hatsuon-y-row ()
  "Test 撥音拡張 for y-row (yz, yk, yj, yd, yl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "yz") "やん"))
    (should (equal (nskk-convert-romaji "yk") "いん"))
    (should (equal (nskk-convert-romaji "yj") "ゆん"))
    (should (equal (nskk-convert-romaji "yd") "えん"))
    (should (equal (nskk-convert-romaji "yl") "よん"))))

(ert-deftest nskk-azik-hatsuon-r-row ()
  "Test 撥音拡張 for r-row (rz, rk, rj, rd, rl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "rz") "らん"))
    (should (equal (nskk-convert-romaji "rk") "りん"))
    (should (equal (nskk-convert-romaji "rj") "るん"))
    (should (equal (nskk-convert-romaji "rd") "れん"))
    (should (equal (nskk-convert-romaji "rl") "ろん"))))

(ert-deftest nskk-azik-hatsuon-w-row ()
  "Test 撥音拡張 for w-row (wz, wk, wj, wd, wl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "wz") "わん"))
    (should (equal (nskk-convert-romaji "wk") "うぃん"))
    (should (equal (nskk-convert-romaji "wj") "うん"))
    (should (equal (nskk-convert-romaji "wd") "うぇん"))
    (should (equal (nskk-convert-romaji "wl") "をん"))))

(ert-deftest nskk-azik-hatsuon-g-row ()
  "Test 撥音拡張 for g-row (gz, gk, gj, gd, gl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "gz") "がん"))
    (should (equal (nskk-convert-romaji "gk") "ぎん"))
    (should (equal (nskk-convert-romaji "gj") "ぐん"))
    (should (equal (nskk-convert-romaji "gd") "げん"))
    (should (equal (nskk-convert-romaji "gl") "ごん"))))

(ert-deftest nskk-azik-hatsuon-z-row ()
  "Test 撥音拡張 for z-row (zz, zk, zj, zd, zl).
Note: zz is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
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
    (should (equal (nskk-convert-romaji "zl") "ぞん"))))

(ert-deftest nskk-azik-hatsuon-d-row ()
  "Test 撥音拡張 for d-row (dz, dk, dj, dd, dl).
Note: dd is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
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
    (should (equal (nskk-convert-romaji "dl") "どん"))))

(ert-deftest nskk-azik-hatsuon-b-row ()
  "Test 撥音拡張 for b-row (bz, bk, bj, bd, bl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "bz") "ばん"))
    (should (equal (nskk-convert-romaji "bk") "びん"))
    (should (equal (nskk-convert-romaji "bj") "ぶん"))
    (should (equal (nskk-convert-romaji "bd") "べん"))
    (should (equal (nskk-convert-romaji "bl") "ぼん"))))

(ert-deftest nskk-azik-hatsuon-p-row ()
  "Test 撥音拡張 for p-row (pz, pk, pj, pd, pl)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "pz") "ぱん"))
    (should (equal (nskk-convert-romaji "pk") "ぴん"))
    (should (equal (nskk-convert-romaji "pj") "ぷん"))
    (should (equal (nskk-convert-romaji "pd") "ぺん"))
    (should (equal (nskk-convert-romaji "pl") "ぽん"))))


;;;;
;;;; 4. 二重母音拡張 (Diphthong Extension) Tests
;;;;

(ert-deftest nskk-azik-diphthong-k-row ()
  "Test 二重母音拡張 for k-row (kq, kh, kw, kp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "kq") "かい"))
    (should (equal (nskk-convert-romaji "kh") "くう"))
    (should (equal (nskk-convert-romaji "kw") "けい"))
    (should (equal (nskk-convert-romaji "kp") "こう"))))

(ert-deftest nskk-azik-diphthong-s-row ()
  "Test 二重母音拡張 for s-row (sq, sh, sw, sp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "sq") "さい"))
    (should (equal (nskk-convert-romaji "sh") "すう"))
    (should (equal (nskk-convert-romaji "sw") "せい"))
    (should (equal (nskk-convert-romaji "sp") "そう"))))

(ert-deftest nskk-azik-diphthong-t-row ()
  "Test 二重母音拡張 for t-row (tq, th, tw, tp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "tq") "たい"))
    (should (equal (nskk-convert-romaji "th") "つう"))
    (should (equal (nskk-convert-romaji "tw") "てい"))
    (should (equal (nskk-convert-romaji "tp") "とう"))))

(ert-deftest nskk-azik-diphthong-n-row ()
  "Test 二重母音拡張 for n-row (nq, nh, nw, np).
Note: The converter has special handling for 'n' before consonants.
Rules exist in the table but 'n' + consonant is converted to ん first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table (they are correctly defined)
    (should (equal (nskk-converter-lookup "nq") "ない"))
    (should (equal (nskk-converter-lookup "nh") "ぬう"))
    (should (equal (nskk-converter-lookup "nw") "ねい"))
    (should (equal (nskk-converter-lookup "np") "のう"))))

(ert-deftest nskk-azik-diphthong-h-row ()
  "Test 二重母音拡張 for h-row (hq, hh, hw, hp).
Note: hh is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "hq") "はい"))
    (should (equal (nskk-converter-lookup "hh") "ふう"))
    (should (equal (nskk-converter-lookup "hw") "へい"))
    (should (equal (nskk-converter-lookup "hp") "ほう"))
    ;; hq, hw, hp should work in conversion
    (should (equal (nskk-convert-romaji "hq") "はい"))
    (should (equal (nskk-convert-romaji "hw") "へい"))
    (should (equal (nskk-convert-romaji "hp") "ほう"))))

(ert-deftest nskk-azik-diphthong-m-row ()
  "Test 二重母音拡張 for m-row (mq, mh, mw, mp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "mq") "まい"))
    (should (equal (nskk-convert-romaji "mh") "むう"))
    (should (equal (nskk-convert-romaji "mw") "めい"))
    (should (equal (nskk-convert-romaji "mp") "もう"))))

(ert-deftest nskk-azik-diphthong-y-row ()
  "Test 二重母音拡張 for y-row (yq, yh, yw, yp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "yq") "やい"))
    (should (equal (nskk-convert-romaji "yh") "ゆう"))
    (should (equal (nskk-convert-romaji "yw") "えい"))
    (should (equal (nskk-convert-romaji "yp") "よう"))))

(ert-deftest nskk-azik-diphthong-r-row ()
  "Test 二重母音拡張 for r-row (rq, rh, rw, rp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "rq") "らい"))
    (should (equal (nskk-convert-romaji "rh") "るう"))
    (should (equal (nskk-convert-romaji "rw") "れい"))
    (should (equal (nskk-convert-romaji "rp") "ろう"))))

(ert-deftest nskk-azik-diphthong-w-row ()
  "Test 二重母音拡張 for w-row (wq, wh, ww, wp).
Note: ww is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "wq") "わい"))
    (should (equal (nskk-converter-lookup "wh") "うう"))
    (should (equal (nskk-converter-lookup "ww") "うぇい"))
    (should (equal (nskk-converter-lookup "wp") "うぉう"))
    ;; wq, wh, wp should work in conversion
    (should (equal (nskk-convert-romaji "wq") "わい"))
    (should (equal (nskk-convert-romaji "wh") "うう"))
    (should (equal (nskk-convert-romaji "wp") "うぉう"))))

(ert-deftest nskk-azik-diphthong-g-row ()
  "Test 二重母音拡張 for g-row (gq, gh, gw, gp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "gq") "がい"))
    (should (equal (nskk-convert-romaji "gh") "ぐう"))
    (should (equal (nskk-convert-romaji "gw") "げい"))
    (should (equal (nskk-convert-romaji "gp") "ごう"))))

(ert-deftest nskk-azik-diphthong-z-row ()
  "Test 二重母音拡張 for z-row (zq, zh, zw, zp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "zq") "ざい"))
    (should (equal (nskk-convert-romaji "zh") "ずう"))
    (should (equal (nskk-convert-romaji "zw") "ぜい"))
    (should (equal (nskk-convert-romaji "zp") "ぞう"))))

(ert-deftest nskk-azik-diphthong-d-row ()
  "Test 二重母音拡張 for d-row (dq, dh, dw, dp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "dq") "だい"))
    (should (equal (nskk-convert-romaji "dh") "づう"))
    (should (equal (nskk-convert-romaji "dw") "でい"))
    (should (equal (nskk-convert-romaji "dp") "どう"))))

(ert-deftest nskk-azik-diphthong-b-row ()
  "Test 二重母音拡張 for b-row (bq, bh, bw, bp)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "bq") "ばい"))
    (should (equal (nskk-convert-romaji "bh") "ぶう"))
    (should (equal (nskk-convert-romaji "bw") "べい"))
    (should (equal (nskk-convert-romaji "bp") "ぼう"))))

(ert-deftest nskk-azik-diphthong-p-row ()
  "Test 二重母音拡張 for p-row (pq, ph, pw, pp).
Note: pp is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "pq") "ぱい"))
    (should (equal (nskk-converter-lookup "ph") "ぷう"))
    (should (equal (nskk-converter-lookup "pw") "ぺい"))
    (should (equal (nskk-converter-lookup "pp") "ぽう"))
    ;; pq, ph, pw should work in conversion
    (should (equal (nskk-convert-romaji "pq") "ぱい"))
    (should (equal (nskk-convert-romaji "ph") "ぷう"))
    (should (equal (nskk-convert-romaji "pw") "ぺい"))))


;;;;
;;;; 5. 拗音互換キー (Yō-on Compatibility Key) Tests
;;;;

(ert-deftest nskk-azik-youon-kg-row ()
  "Test 拗音互換キー for kg-row (kga, kgu, kge, kgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "kga") "きゃ"))
    (should (equal (nskk-convert-romaji "kgu") "きゅ"))
    (should (equal (nskk-convert-romaji "kge") "きぇ"))
    (should (equal (nskk-convert-romaji "kgo") "きょ"))))

(ert-deftest nskk-azik-youon-hg-row ()
  "Test 拗音互換キー for hg-row (hga, hgu, hge, hgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "hga") "ひゃ"))
    (should (equal (nskk-convert-romaji "hgu") "ひゅ"))
    (should (equal (nskk-convert-romaji "hge") "ひぇ"))
    (should (equal (nskk-convert-romaji "hgo") "ひょ"))))

(ert-deftest nskk-azik-youon-mg-row ()
  "Test 拗音互換キー for mg-row (mga, mgu, mge, mgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "mga") "みゃ"))
    (should (equal (nskk-convert-romaji "mgu") "みゅ"))
    (should (equal (nskk-convert-romaji "mge") "みぇ"))
    (should (equal (nskk-convert-romaji "mgo") "みょ"))))

(ert-deftest nskk-azik-youon-rg-row ()
  "Test 拗音互換キー for rg-row (rga, rgu, rge, rgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "rga") "りゃ"))
    (should (equal (nskk-convert-romaji "rgu") "りゅ"))
    (should (equal (nskk-convert-romaji "rge") "りぇ"))
    (should (equal (nskk-convert-romaji "rgo") "りょ"))))

(ert-deftest nskk-azik-youon-gg-row ()
  "Test 拗音互換キー for gg-row (gga, ggu, gge, ggo).
Note: gg is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "gga") "ぎゃ"))
    (should (equal (nskk-converter-lookup "ggu") "ぎゅ"))
    (should (equal (nskk-converter-lookup "gge") "ぎぇ"))
    (should (equal (nskk-converter-lookup "ggo") "ぎょ"))))

(ert-deftest nskk-azik-youon-jg-row ()
  "Test 拗音互換キー for jg-row (jga, jgu, jge, jgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "jga") "じゃ"))
    (should (equal (nskk-convert-romaji "jgu") "じゅ"))
    (should (equal (nskk-convert-romaji "jge") "じぇ"))
    (should (equal (nskk-convert-romaji "jgo") "じょ"))))

(ert-deftest nskk-azik-youon-bg-row ()
  "Test 拗音互換キー for bg-row (bga, bgu, bge, bgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "bga") "びゃ"))
    (should (equal (nskk-convert-romaji "bgu") "びゅ"))
    (should (equal (nskk-convert-romaji "bge") "びぇ"))
    (should (equal (nskk-convert-romaji "bgo") "びょ"))))

(ert-deftest nskk-azik-youon-pg-row ()
  "Test 拗音互換キー for pg-row (pga, pgu, pge, pgo)."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "pga") "ぴゃ"))
    (should (equal (nskk-convert-romaji "pgu") "ぴゅ"))
    (should (equal (nskk-convert-romaji "pge") "ぴぇ"))
    (should (equal (nskk-convert-romaji "pgo") "ぴょ"))))

(ert-deftest nskk-azik-youon-hatsuon ()
  "Test 拗音互換キー with 撥音拡張."
  (nskk-with-azik-style
    ;; kg + 撥音拡張
    (should (equal (nskk-convert-romaji "kgz") "きゃん"))
    (should (equal (nskk-convert-romaji "kgk") "きぃん"))
    (should (equal (nskk-convert-romaji "kgj") "きゅん"))
    (should (equal (nskk-convert-romaji "kgd") "きぇん"))
    (should (equal (nskk-convert-romaji "kgl") "きょん"))))

(ert-deftest nskk-azik-youon-diphthong ()
  "Test 拗音互換キー with 二重母音拡張."
  (nskk-with-azik-style
    ;; kg + 二重母音拡張
    (should (equal (nskk-convert-romaji "kgq") "きゃい"))
    (should (equal (nskk-convert-romaji "kgh") "きゅう"))
    (should (equal (nskk-convert-romaji "kgw") "きぇい"))
    (should (equal (nskk-convert-romaji "kgp") "きょう"))))


;;;;
;;;; 6. 同指打鍵互換キー (Same-Finger Compatibility Key) Tests
;;;;

(ert-deftest nskk-azik-same-finger-k ()
  "Test 同指打鍵互換キー kf -> ki."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "kf") "き"))))

(ert-deftest nskk-azik-same-finger-n ()
  "Test 同指打鍵互換キー nf -> nu.
Note: The converter has special handling for 'n' before consonants.
Rule exists in the table but 'n' + 'f' is converted to ん first."
  (nskk-with-azik-style
    ;; Verify rule exists in the table
    (should (equal (nskk-converter-lookup "nf") "ぬ"))))

(ert-deftest nskk-azik-same-finger-m ()
  "Test 同指打鍵互換キー mf -> mu."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "mf") "む"))))

(ert-deftest nskk-azik-same-finger-g ()
  "Test 同指打鍵互換キー gf -> gu."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "gf") "ぐ"))))

(ert-deftest nskk-azik-same-finger-p ()
  "Test 同指打鍵互換キー pf -> pu."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "pf") "ぷ"))))

(ert-deftest nskk-azik-same-finger-r ()
  "Test 同指打鍵互換キー rf -> ru."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "rf") "る"))))

(ert-deftest nskk-azik-same-finger-y ()
  "Test 同指打鍵互換キー yf -> yu."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "yf") "ゆ"))))


;;;;
;;;; 7. 特殊拡張 (Special Extension) Tests
;;;;

(ert-deftest nskk-azik-special-extension-k ()
  "Test 特殊拡張 for k-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "km") "かも"))
    (should (equal (nskk-convert-romaji "kr") "から"))
    (should (equal (nskk-convert-romaji "kt") "こと"))))

(ert-deftest nskk-azik-special-extension-g ()
  "Test 特殊拡張 for g-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "gr") "がら"))
    (should (equal (nskk-convert-romaji "gt") "ごと"))))

(ert-deftest nskk-azik-special-extension-z ()
  "Test 特殊拡張 for z-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "zr") "ざる"))))

(ert-deftest nskk-azik-special-extension-s ()
  "Test 特殊拡張 for s-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "st") "した"))
    (should (equal (nskk-convert-romaji "sr") "する"))))

(ert-deftest nskk-azik-special-extension-t ()
  "Test 特殊拡張 for t-row shortcuts.
Note: tt is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "tt") "たち"))
    (should (equal (nskk-converter-lookup "tb") "たび"))
    (should (equal (nskk-converter-lookup "tm") "ため"))
    (should (equal (nskk-converter-lookup "tr") "たら"))
    ;; tb, tm, tr should work in conversion
    (should (equal (nskk-convert-romaji "tb") "たび"))
    (should (equal (nskk-convert-romaji "tm") "ため"))
    (should (equal (nskk-convert-romaji "tr") "たら"))))

(ert-deftest nskk-azik-special-extension-d ()
  "Test 特殊拡張 for d-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "dt") "だち"))
    (should (equal (nskk-convert-romaji "ds") "です"))
    (should (equal (nskk-convert-romaji "dm") "でも"))))

(ert-deftest nskk-azik-special-extension-n ()
  "Test 特殊拡張 for n-row shortcuts.
Note: The converter has special handling for 'n' before consonants.
Rules exist in the table but 'n' + consonant is converted to ん first."
  (nskk-with-azik-style
    ;; Verify rules exist in the table
    (should (equal (nskk-converter-lookup "nr") "なる"))
    (should (equal (nskk-converter-lookup "nt") "にち"))
    (should (equal (nskk-converter-lookup "nb") "ねば"))))

(ert-deftest nskk-azik-special-extension-h ()
  "Test 特殊拡張 for h-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ht") "ひと"))))

(ert-deftest nskk-azik-special-extension-b ()
  "Test 特殊拡張 for b-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "bt") "びと"))))

(ert-deftest nskk-azik-special-extension-m ()
  "Test 特殊拡張 for m-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ms") "ます"))
    (should (equal (nskk-convert-romaji "mt") "また"))
    (should (equal (nskk-convert-romaji "mn") "もの"))))

(ert-deftest nskk-azik-special-extension-y ()
  "Test 特殊拡張 for y-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "yr") "よる"))))

(ert-deftest nskk-azik-special-extension-r ()
  "Test 特殊拡張 for r-row shortcuts.
Note: rr is handled as double consonant (sokuon) by the converter.
The rule exists in the table but sokuon check happens first."
  (nskk-with-azik-style
    ;; Verify rule exists in the table
    (should (equal (nskk-converter-lookup "rr") "られ"))))

(ert-deftest nskk-azik-special-extension-w ()
  "Test 特殊拡張 for w-row shortcuts."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "wt") "わた"))
    (should (equal (nskk-convert-romaji "wr") "われ"))))


;;;;
;;;; 8. Q-key Behavior Tests
;;;;

(ert-deftest nskk-azik-q-key-does-not-conflict ()
  "Test that q-key can be used as extension key without standalone definition."
  (nskk-with-azik-style
    ;; In AZIK, 'q' is used as an extension key for diphthongs
    ;; It should not have a standalone conversion
    ;; When used with consonants like 'kq', it produces 'かい'
    (should (equal (nskk-convert-romaji "kq") "かい"))
    (should (equal (nskk-convert-romaji "sq") "さい"))
    (should (equal (nskk-convert-romaji "tq") "たい"))))

(ert-deftest nskk-azik-q-key-with-empty-buffer ()
  "Test q-key behavior with empty buffer context."
  (nskk-with-azik-style
    ;; When q is typed alone, it should not convert (no rule for standalone q)
    ;; The rule lookup should return nil for standalone 'q'
    (should-not (nskk-converter-lookup "q"))))

(ert-deftest nskk-azik-q-key-with-pending-input ()
  "Test q-key behavior with pending input.
Note: 'nq' has special handling - 'n' before consonant becomes ん first."
  (nskk-with-azik-style
    ;; When there's pending input like 'k', 'q' should complete to 'かい'
    (should (equal (nskk-convert-romaji "kq") "かい"))
    (should (equal (nskk-convert-romaji "hq") "はい"))
    (should (equal (nskk-convert-romaji "sq") "さい"))
    (should (equal (nskk-convert-romaji "tq") "たい"))))


;;;;
;;;; 9. Compatibility Tests
;;;;

(ert-deftest nskk-azik-standard-romaji-still-works ()
  "Test that standard romaji still works in AZIK mode."
  (nskk-with-azik-style
    ;; Basic vowels
    (should (equal (nskk-convert-romaji "a") "あ"))
    (should (equal (nskk-convert-romaji "i") "い"))
    (should (equal (nskk-convert-romaji "u") "う"))
    (should (equal (nskk-convert-romaji "e") "え"))
    (should (equal (nskk-convert-romaji "o") "お"))
    ;; K-row
    (should (equal (nskk-convert-romaji "ka") "か"))
    (should (equal (nskk-convert-romaji "ki") "き"))
    (should (equal (nskk-convert-romaji "ku") "く"))
    (should (equal (nskk-convert-romaji "ke") "け"))
    (should (equal (nskk-convert-romaji "ko") "こ"))))

(ert-deftest nskk-azik-standard-g-row-works ()
  "Test that standard g-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ga") "が"))
    (should (equal (nskk-convert-romaji "gi") "ぎ"))
    (should (equal (nskk-convert-romaji "gu") "ぐ"))
    (should (equal (nskk-convert-romaji "ge") "げ"))
    (should (equal (nskk-convert-romaji "go") "ご"))))

(ert-deftest nskk-azik-standard-s-row-works ()
  "Test that standard s-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "sa") "さ"))
    (should (equal (nskk-convert-romaji "shi") "し"))
    (should (equal (nskk-convert-romaji "su") "す"))
    (should (equal (nskk-convert-romaji "se") "せ"))
    (should (equal (nskk-convert-romaji "so") "そ"))))

(ert-deftest nskk-azik-standard-t-row-works ()
  "Test that standard t-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ta") "た"))
    (should (equal (nskk-convert-romaji "chi") "ち"))
    (should (equal (nskk-convert-romaji "tsu") "つ"))
    (should (equal (nskk-convert-romaji "te") "て"))
    (should (equal (nskk-convert-romaji "to") "と"))))

(ert-deftest nskk-azik-standard-n-row-works ()
  "Test that standard n-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "na") "な"))
    (should (equal (nskk-convert-romaji "ni") "に"))
    (should (equal (nskk-convert-romaji "nu") "ぬ"))
    (should (equal (nskk-convert-romaji "ne") "ね"))
    (should (equal (nskk-convert-romaji "no") "の"))
    (should (equal (nskk-convert-romaji "nn") "ん"))))

(ert-deftest nskk-azik-standard-h-row-works ()
  "Test that standard h-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ha") "は"))
    (should (equal (nskk-convert-romaji "hi") "ひ"))
    (should (equal (nskk-convert-romaji "fu") "ふ"))
    (should (equal (nskk-convert-romaji "he") "へ"))
    (should (equal (nskk-convert-romaji "ho") "ほ"))))

(ert-deftest nskk-azik-standard-m-row-works ()
  "Test that standard m-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ma") "ま"))
    (should (equal (nskk-convert-romaji "mi") "み"))
    (should (equal (nskk-convert-romaji "mu") "む"))
    (should (equal (nskk-convert-romaji "me") "め"))
    (should (equal (nskk-convert-romaji "mo") "も"))))

(ert-deftest nskk-azik-standard-y-row-works ()
  "Test that standard y-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ya") "や"))
    (should (equal (nskk-convert-romaji "yu") "ゆ"))
    (should (equal (nskk-convert-romaji "yo") "よ"))))

(ert-deftest nskk-azik-standard-r-row-works ()
  "Test that standard r-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "ra") "ら"))
    (should (equal (nskk-convert-romaji "ri") "り"))
    (should (equal (nskk-convert-romaji "ru") "る"))
    (should (equal (nskk-convert-romaji "re") "れ"))
    (should (equal (nskk-convert-romaji "ro") "ろ"))))

(ert-deftest nskk-azik-standard-w-row-works ()
  "Test that standard w-row romaji still works in AZIK mode."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "wa") "わ"))
    (should (equal (nskk-convert-romaji "wo") "を"))))

(ert-deftest nskk-azik-standard-youon-works ()
  "Test that standard yō-on (digraphs) still work in AZIK mode."
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
    (should (equal (nskk-convert-romaji "cho") "ちょ"))))

(ert-deftest nskk-azik-word-conversion ()
  "Test complete word conversion using AZIK features."
  (nskk-with-azik-style
    ;; Using 撥音拡張
    (should (equal (nskk-convert-romaji "sz") "さん"))
    (should (equal (nskk-convert-romaji "tl") "とん"))
    ;; Using 二重母音拡張
    (should (equal (nskk-convert-romaji "tp") "とう"))
    (should (equal (nskk-convert-romaji "kw") "けい"))
    ;; Mixed conversion
    (should (equal (nskk-convert-romaji "kztp") "かんとう"))))


;;;;
;;;; Integration Tests
;;;;

(ert-deftest nskk-azik-integration-basic-input ()
  "Test basic AZIK input integration."
  (nskk-with-azik-style
    ;; Type "kz" -> "かん"
    (should (equal (nskk-convert-romaji "kz") "かん"))
    ;; Type "sz" -> "さん"
    (should (equal (nskk-convert-romaji "sz") "さん"))
    ;; Type "tp" -> "とう"
    (should (equal (nskk-convert-romaji "tp") "とう"))))

(ert-deftest nskk-azik-integration-complex-words ()
  "Test complex word conversion using AZIK."
  (nskk-with-azik-style
    ;; "きょう" using kg + p (拗音 + 二重母音)
    (should (equal (nskk-convert-romaji "kgp") "きょう"))
    ;; "きょうと" using 拗音二重母音 + と
    (should (equal (nskk-convert-romaji "kgpto") "きょうと"))
    ;; "とうきょう" using tp + きょう
    (should (equal (nskk-convert-romaji "tpkyo") "とうきょ"))
    ;; "さんぽ" using さん + ぽ
    (should (equal (nskk-convert-romaji "szpo") "さんぽ"))))

(ert-deftest nskk-azik-integration-mixed-input ()
  "Test mixed standard and AZIK input."
  (nskk-with-azik-style
    ;; Mix standard and AZIK rules
    (should (equal (nskk-convert-romaji "kakz") "かかん"))
    (should (equal (nskk-convert-romaji "sask") "さしん"))
    ;; Note: tatq = ta + tq = た + たい (not たちたい)
    (should (equal (nskk-convert-romaji "tatq") "たたい"))
    ;; For たちたい, use ta + chi + tq
    (should (equal (nskk-convert-romaji "tachitq") "たちたい"))))


;;;;
;;;; Regression Tests
;;;;

(ert-deftest nskk-azik-regression-kz-not-kan ()
  "Regression test: kz should produce かん, not かn."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji "kz") "かん"))
    (should-not (equal (nskk-convert-romaji "kz") "かn"))))

(ert-deftest nskk-azik-regression-semi-colon-tsu ()
  "Regression test: semicolon should produce small tsu."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji ";") "っ"))
    ;; In context: ;ka -> っか
    (should (equal (nskk-convert-romaji ";ka") "っか"))))

(ert-deftest nskk-azik-regression-colon-chouon ()
  "Regression test: colon should produce chouon."
  (nskk-with-azik-style
    (should (equal (nskk-convert-romaji ":") "ー"))))


;;;;
;;;; Test Suite Organization
;;;;

(defun nskk-azik-test-run-all ()
  "Run all AZIK tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-"))

(defun nskk-azik-test-run-style ()
  "Run AZIK style switching tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-load\\|^nskk-azik-style-"))

(defun nskk-azik-test-run-hatsuon ()
  "Run AZIK 撥音拡張 tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-hatsuon-"))

(defun nskk-azik-test-run-diphthong ()
  "Run AZIK 二重母音拡張 tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-diphthong-"))

(defun nskk-azik-test-run-youon ()
  "Run AZIK 拗音互換キー tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-youon-"))

(defun nskk-azik-test-run-compatibility ()
  "Run AZIK compatibility tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-standard\\|^nskk-azik-compatibility"))


(provide 'nskk-azik-test)

;;; nskk-azik-test.el ends here
