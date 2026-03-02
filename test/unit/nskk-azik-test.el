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
;; - Prolog-level tests (azik-rule/2 direct queries, bridge rule)

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-converter)
(require 'nskk-azik)
(require 'nskk-prolog)


;;;;
;;;; Helper Macros for AZIK Tests
;;;;

(defmacro nskk-with-azik-style (&rest body)
  "Execute BODY with AZIK style loaded."
  (declare (indent 0) (debug t))
  `(nskk-prolog-test-with-isolated-db
     (let ((nskk--saved-romaji-table (copy-hash-table nskk--romaji-table)))
       (unwind-protect
           (progn
             (nskk-converter-load-style 'azik)
             ,@body)
         (clrhash nskk--romaji-table)
         (maphash (lambda (k v) (puthash k v nskk--romaji-table))
                  nskk--saved-romaji-table)))))

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
    (nskk-prolog-test-with-isolated-db
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
      (should (equal (nskk-convert-romaji "ka") "か")))))


;;;;
;;;; 2. Special Keys Tests
;;;;

(nskk-describe "AZIK special keys"
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
  (nskk-context "k-row hatsuon"
    (nskk-it "kz kj kd kl convert to かん くん けん こん"
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
        (should (equal (nskk-convert-romaji "pl") "ぽん"))))))


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
    (nskk-it "sq sw sp convert to さい せい そう; sh demoted to :incomplete"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "sq") "さい"))
        ;; "sh" is demoted to :incomplete: standard romaji has sha/shi/shu/she/sho
        (should (eq (nskk-converter-lookup "sh") :incomplete))
        (should (equal (nskk-convert-romaji "sw") "せい"))
        (should (equal (nskk-convert-romaji "sp") "そう")))))

  (nskk-context "t-row diphthong"
    (nskk-it "tq tw tp convert to たい てい とう; th demoted to :incomplete"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "tq") "たい"))
        ;; "th" is demoted to :incomplete: standard romaji has tha/thi/thu/the/tho
        (should (eq (nskk-converter-lookup "th") :incomplete))
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
    (nskk-it "wq wp convert to わい うぉう; wh demoted to :incomplete"
      (nskk-with-azik-style
        ;; Verify rules exist in the table
        (should (equal (nskk-converter-lookup "wq") "わい"))
        ;; "wh" is demoted to :incomplete: standard romaji has wha/whi/whu/whe/who
        (should (eq (nskk-converter-lookup "wh") :incomplete))
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
    (nskk-it "dq dw dp convert to だい でい どう; dh demoted to :incomplete"
      (nskk-with-azik-style
        (should (equal (nskk-convert-romaji "dq") "だい"))
        ;; "dh" is demoted to :incomplete: standard romaji has dha/dhi/dhu/dhe/dho
        (should (eq (nskk-converter-lookup "dh") :incomplete))
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
        (should (equal (nskk-convert-romaji "pw") "ぺい"))))))


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
;;;; Integration Tests
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
      ;; "とうきょう" using tp + きょう
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
      (should (equal (nskk-convert-romaji "tachitq") "たちたい")))))


;;;;
;;;; Regression Tests
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
;;;; 10. Prolog-level Tests
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
        (should (equal (nskk-prolog-walk '\?k (car results)) "かい")))))

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

  (nskk-it "azik-rule/2 contains a substantial number of facts (at least 200)"
    (nskk-with-azik-style
      (let ((results (nskk-prolog-query '(azik-rule \?r \?k))))
        (should results)
        ;; 2 special + 10 compat + 14*9 extensions + 8*13 youon + 7 same-finger
        ;; + 27 shortcuts + 5 foreign = at least 200 rules
        (should (>= (length results) 200)))))

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
      ;; 2-char youon prefixes (derived from azik-rule/2, not hardcoded)
      (should (eq (nskk-converter-lookup "kg") :incomplete))
      (should (eq (nskk-converter-lookup "hg") :incomplete))
      (should (eq (nskk-converter-lookup "mg") :incomplete))
      (should (eq (nskk-converter-lookup "rg") :incomplete))
      (should (eq (nskk-converter-lookup "jg") :incomplete)))))


;;;;
;;;; Test Suite Organization
;;;;

(defun nskk-azik-test-run-all ()
  "Run all AZIK tests."
  (interactive)
  (ert-run-tests-batch "^nskk-azik-\\|^nskk-it/azik"))

(defun nskk-azik-test-run-style ()
  "Run AZIK style switching tests."
  (interactive)
  (ert-run-tests-batch "^nskk-it/azik-style-switching"))

(defun nskk-azik-test-run-hatsuon ()
  "Run AZIK 撥音拡張 tests."
  (interactive)
  (ert-run-tests-batch "^nskk-it/azik-hatsuon"))

(defun nskk-azik-test-run-diphthong ()
  "Run AZIK 二重母音拡張 tests."
  (interactive)
  (ert-run-tests-batch "^nskk-it/azik-diphthong"))

(defun nskk-azik-test-run-youon ()
  "Run AZIK 拗音互換キー tests."
  (interactive)
  (ert-run-tests-batch "^nskk-it/azik-youon"))

(defun nskk-azik-test-run-compatibility ()
  "Run AZIK compatibility tests."
  (interactive)
  (ert-run-tests-batch "^nskk-it/azik-compatibility"))


(provide 'nskk-azik-test)

;;; nskk-azik-test.el ends here
