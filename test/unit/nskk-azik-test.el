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

;; Tests for AZIK extended romaji input.

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
     (let* ((nskk--saved-romaji-table (copy-hash-table (nskk-romaji-table)))
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
         (clrhash (nskk-romaji-table))
         (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                  nskk--saved-romaji-table)
         (when (boundp 'nskk-mode-map)
           (if nskk--azik-saved-binding
               (keymap-set nskk-mode-map nskk--azik-toggle-key
                           nskk--azik-saved-binding)
             (keymap-unset nskk-mode-map nskk--azik-toggle-key t)))))))

(defmacro nskk-with-standard-style (&rest body)
  "Execute BODY with standard style loaded, restoring Prolog DB after."
  (declare (indent 0) (debug t))
  `(nskk-prolog-test-with-isolated-db
     (let ((nskk--saved-romaji-table (copy-hash-table (nskk-romaji-table))))
       (unwind-protect
           (progn
             (nskk-converter-load-style 'standard)
             ,@body)
         (clrhash (nskk-romaji-table))
         (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                  nskk--saved-romaji-table)))))


;;;;
;;;; 1. Style Switching Tests
;;;;

(nskk-describe "AZIK style switching"
  (nskk-it "loading standard style returns standard and provides basic romaji"
    (nskk-prolog-test-with-isolated-db
      (should (eq (nskk-converter-load-style 'standard) 'standard))
      (should (equal (nskk-test-convert-romaji "ka") "か"))
      (should (equal (nskk-test-convert-romaji "shi") "し"))))

  (nskk-it "loading AZIK style returns azik and provides extended rules"
    (nskk-with-azik-style
      (should (eq (nskk-converter-load-style 'azik) 'azik))
      (should (equal (nskk-test-convert-romaji "kz") "かん"))
      (should (equal (nskk-test-convert-romaji "kq") "かい"))))

  (nskk-it "can switch between standard and AZIK styles"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-load-style 'standard)
      (should (equal (nskk-test-convert-romaji "ka") "か"))
      (should-not (equal (nskk-test-convert-romaji "kz") "かん"))
      (nskk-converter-load-style 'azik)
      (should (equal (nskk-test-convert-romaji "kz") "かん"))
      (nskk-converter-load-style 'standard)
      (should (equal (nskk-test-convert-romaji "ka") "か"))))

  (nskk-it "nskk-azik-keyboard-type us101 uses [ as toggle key"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-with-azik-style
        (when (boundp 'nskk-mode-map)
          (should (lookup-key nskk-mode-map "["))))))

  (nskk-it "moves toggle binding bidirectionally and restores displaced bindings"
    (let ((nskk-mode-map (make-sparse-keymap))
          (nskk--azik-toggle-key-state nil))
      (cl-progv '(nskk-mode-map) (list nskk-mode-map)
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-assert '((azik-toggle-key jp106 "@")))
          (nskk-prolog-assert '((azik-toggle-key us101 "[")))
          (keymap-set nskk-mode-map "@" #'beginning-of-line)
          (keymap-set nskk-mode-map "[" #'end-of-line)
          (keymap-set nskk-mode-map "C-c u" #'ignore)
          (let ((nskk-azik-keyboard-type 'jp106))
            (nskk--setup-azik-toggle-key)
            (nskk--setup-azik-toggle-key))
          (should (eq (lookup-key nskk-mode-map "@")
                      #'nskk-toggle-japanese-mode))
          (should (eq (lookup-key nskk-mode-map "[") #'end-of-line))
          (should (eq (lookup-key nskk-mode-map (kbd "C-c u")) #'ignore))
          (let ((nskk-azik-keyboard-type 'us101))
            (nskk--setup-azik-toggle-key)
            (nskk--setup-azik-toggle-key))
          (should (eq (lookup-key nskk-mode-map "@") #'beginning-of-line))
          (should (eq (lookup-key nskk-mode-map "[")
                      #'nskk-toggle-japanese-mode))
          (should (eq (lookup-key nskk-mode-map (kbd "C-c u")) #'ignore))
          (let ((nskk-azik-keyboard-type 'jp106))
            (nskk--setup-azik-toggle-key))
          (should (eq (lookup-key nskk-mode-map "@")
                      #'nskk-toggle-japanese-mode))
          (should (eq (lookup-key nskk-mode-map "[") #'end-of-line))
          (should (eq (lookup-key nskk-mode-map (kbd "C-c u")) #'ignore))))))

  (nskk-it "preserves a user replacement made while a toggle key is active"
    (let ((nskk-mode-map (make-sparse-keymap))
          (nskk--azik-toggle-key-state nil))
      (cl-progv '(nskk-mode-map) (list nskk-mode-map)
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-assert '((azik-toggle-key jp106 "@")))
          (nskk-prolog-assert '((azik-toggle-key us101 "[")))
          (keymap-set nskk-mode-map "@" #'beginning-of-line)
          (let ((nskk-azik-keyboard-type 'jp106))
            (nskk--setup-azik-toggle-key)
            (keymap-set nskk-mode-map "@" #'forward-char)
            (nskk--setup-azik-toggle-key))
          (let ((nskk-azik-keyboard-type 'us101))
            (nskk--setup-azik-toggle-key))
          (should (eq (lookup-key nskk-mode-map "@") #'forward-char))
          (should (eq (lookup-key nskk-mode-map "[")
                      #'nskk-toggle-japanese-mode))))))

  (nskk-it "falls back to @ for a keyboard type with no azik-toggle-key fact"
    (let ((nskk-mode-map (make-sparse-keymap))
          (nskk--azik-toggle-key-state nil))
      (cl-progv '(nskk-mode-map) (list nskk-mode-map)
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-assert '((azik-toggle-key jp106 "@")))
          (nskk-prolog-assert '((azik-toggle-key us101 "[")))
          (let ((nskk-azik-keyboard-type 'unregistered-keyboard))
            (nskk--setup-azik-toggle-key))
          (should (eq (lookup-key nskk-mode-map "@")
                      #'nskk-toggle-japanese-mode)))))))

(nskk-describe "AZIK custom conversion table"
  (nskk-it "is a customizable variable with no extra rules by default"
    (should (custom-variable-p 'nskk-azik-conversion-table))
    (should (null (default-value 'nskk-azik-conversion-table))))

  (nskk-it "user rules override built-in mappings and add new ones"
    (let ((nskk-azik-conversion-table '(("ka" "カスタム")
                                       ("qz" "くす"))))
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ka") "カスタム"))
        (should (equal (nskk-test-convert-romaji "qz") "くす"))
        (should (equal (nskk-test-convert-romaji "ki") "き")))))

  (nskk-it "normalizes a new rule behind the generic AZIK bridge"
    (let ((nskk-azik-conversion-table '(("qz" "くす"))))
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "qz") "くす"))
        (should (equal (nskk-converter-lookup "qz") "くす"))
        (should (equal (nskk-prolog-query-value
                        '(azik-rule "qz" \?kana) '\?kana)
                       "くす"))))))

(nskk-describe "AZIK custom conversion table robustness"
  (nskk-it "ignores malformed user entries while still applying valid ones"
    (let ((nskk-azik-conversion-table '((42 "bad")
                                       ("ka" "カスタム")
                                       ("qz" "くす")
                                       ("broken"))))
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ka") "カスタム"))
        (should (equal (nskk-test-convert-romaji "qz") "くす"))
        (should (equal (nskk-test-convert-romaji "ki") "き"))))))

(nskk-describe "AZIK custom conversion table re-initialization"
  (nskk-it "user rules are re-applied after style reload"
    (let ((nskk-azik-conversion-table '(("ka" "カスタム"))))
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ka") "カスタム"))
        (nskk-converter-load-style 'azik)
        (should (equal (nskk-test-convert-romaji "ka") "カスタム"))
        (should (equal (nskk-test-convert-romaji "ki") "き"))))))


;;;;
;;;; 2. Special Keys Tests
;;;;

(nskk-describe "AZIK special keys unit"
  (nskk-it "semicolon produces small tsu in AZIK mode"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji ";") "っ"))))

  (nskk-it "colon produces chouon (long vowel mark) in AZIK mode"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji ":") "ー"))))

  (nskk-it "special keys work in context with other characters"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji ";ka") "っか"))
      (should (equal (nskk-test-convert-romaji "ka:") "かー")))))


;;;;
;;;; 3. 撥音拡張 (Mora Nasal Extension) Tests
;;;;

(nskk-describe "AZIK hatsuon (撥音拡張) rules"
  (nskk-context "k-row hatsuon"
    (nskk-it "kz kk kj kd kl convert to かん きん くん けん こん"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "kz") "かん"))
        (should (equal (nskk-converter-lookup "kk") "きん"))
        (should (equal (nskk-converter-lookup "kj") "くん"))
        (should (equal (nskk-converter-lookup "kd") "けん"))
        (should (equal (nskk-converter-lookup "kl") "こん"))
        (should (equal (nskk-test-convert-romaji "kz") "かん"))
        (should (equal (nskk-test-convert-romaji "kk") "きん"))
        (should (equal (nskk-test-convert-romaji "kj") "くん"))
        (should (equal (nskk-test-convert-romaji "kd") "けん"))
        (should (equal (nskk-test-convert-romaji "kl") "こん")))))

  (nskk-context "s-row hatsuon"
    (nskk-it "sz sk sj sd sl convert to さん しん すん せん そん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "sz") "さん"))
        (should (equal (nskk-test-convert-romaji "sk") "しん"))
        (should (equal (nskk-test-convert-romaji "sj") "すん"))
        (should (equal (nskk-test-convert-romaji "sd") "せん"))
        (should (equal (nskk-test-convert-romaji "sl") "そん")))))

  (nskk-context "t-row hatsuon"
    (nskk-it "tz tk tj td tl convert to たん ちん つん てん とん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "tz") "たん"))
        (should (equal (nskk-test-convert-romaji "tk") "ちん"))
        (should (equal (nskk-test-convert-romaji "tj") "つん"))
        (should (equal (nskk-test-convert-romaji "td") "てん"))
        (should (equal (nskk-test-convert-romaji "tl") "とん")))))

  (nskk-context "n-row hatsuon"
    (nskk-it "nz nk nj nd nl rules exist in the lookup table"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "nz") "なん"))
        (should (equal (nskk-converter-lookup "nk") "にん"))
        (should (equal (nskk-converter-lookup "nj") "ぬん"))
        (should (equal (nskk-converter-lookup "nd") "ねん"))
        (should (equal (nskk-converter-lookup "nl") "のん")))))

  (nskk-context "h-row hatsuon"
    (nskk-it "hz hk hj hd hl convert to はん ひん ふん へん ほん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "hz") "はん"))
        (should (equal (nskk-test-convert-romaji "hk") "ひん"))
        (should (equal (nskk-test-convert-romaji "hj") "ふん"))
        (should (equal (nskk-test-convert-romaji "hd") "へん"))
        (should (equal (nskk-test-convert-romaji "hl") "ほん")))))

  (nskk-context "m-row hatsuon"
    (nskk-it "mz mk mj md ml convert to まん みん むん めん もん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "mz") "まん"))
        (should (equal (nskk-test-convert-romaji "mk") "みん"))
        (should (equal (nskk-test-convert-romaji "mj") "むん"))
        (should (equal (nskk-test-convert-romaji "md") "めん"))
        (should (equal (nskk-test-convert-romaji "ml") "もん")))))

  (nskk-context "y-row hatsuon"
    (nskk-it "yz yk yj yd yl convert to やん いん ゆん えん よん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "yz") "やん"))
        (should (equal (nskk-test-convert-romaji "yk") "いん"))
        (should (equal (nskk-test-convert-romaji "yj") "ゆん"))
        (should (equal (nskk-test-convert-romaji "yd") "えん"))
        (should (equal (nskk-test-convert-romaji "yl") "よん")))))

  (nskk-context "r-row hatsuon"
    (nskk-it "rz rk rj rd rl convert to らん りん るん れん ろん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "rz") "らん"))
        (should (equal (nskk-test-convert-romaji "rk") "りん"))
        (should (equal (nskk-test-convert-romaji "rj") "るん"))
        (should (equal (nskk-test-convert-romaji "rd") "れん"))
        (should (equal (nskk-test-convert-romaji "rl") "ろん")))))

  (nskk-context "w-row hatsuon"
    (nskk-it "wz wk wj wd wl convert to わん うぃん うん うぇん をん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "wz") "わん"))
        (should (equal (nskk-test-convert-romaji "wk") "うぃん"))
        (should (equal (nskk-test-convert-romaji "wj") "うん"))
        (should (equal (nskk-test-convert-romaji "wd") "うぇん"))
        (should (equal (nskk-test-convert-romaji "wl") "をん")))))

  (nskk-context "g-row hatsuon"
    (nskk-it "gz gk gj gd gl convert to がん ぎん ぐん げん ごん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "gz") "がん"))
        (should (equal (nskk-test-convert-romaji "gk") "ぎん"))
        (should (equal (nskk-test-convert-romaji "gj") "ぐん"))
        (should (equal (nskk-test-convert-romaji "gd") "げん"))
        (should (equal (nskk-test-convert-romaji "gl") "ごん")))))

  (nskk-context "z-row hatsuon"
    (nskk-it "zk zj zd zl convert to じん ずん ぜん ぞん and lookup table is complete"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "zz") "ざん"))
        (should (equal (nskk-converter-lookup "zk") "じん"))
        (should (equal (nskk-converter-lookup "zj") "ずん"))
        (should (equal (nskk-converter-lookup "zd") "ぜん"))
        (should (equal (nskk-converter-lookup "zl") "ぞん"))
        (should (equal (nskk-test-convert-romaji "zk") "じん"))
        (should (equal (nskk-test-convert-romaji "zj") "ずん"))
        (should (equal (nskk-test-convert-romaji "zd") "ぜん"))
        (should (equal (nskk-test-convert-romaji "zl") "ぞん")))))

  (nskk-context "d-row hatsuon"
    (nskk-it "dz dk dj dl convert to だん ぢん づん どん and lookup table is complete"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "dz") "だん"))
        (should (equal (nskk-converter-lookup "dk") "ぢん"))
        (should (equal (nskk-converter-lookup "dj") "づん"))
        (should (equal (nskk-converter-lookup "dd") "でん"))
        (should (equal (nskk-converter-lookup "dl") "どん"))
        (should (equal (nskk-test-convert-romaji "dz") "だん"))
        (should (equal (nskk-test-convert-romaji "dk") "ぢん"))
        (should (equal (nskk-test-convert-romaji "dj") "づん"))
        (should (equal (nskk-test-convert-romaji "dl") "どん")))))

  (nskk-context "b-row hatsuon"
    (nskk-it "bz bk bj bd bl convert to ばん びん ぶん べん ぼん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "bz") "ばん"))
        (should (equal (nskk-test-convert-romaji "bk") "びん"))
        (should (equal (nskk-test-convert-romaji "bj") "ぶん"))
        (should (equal (nskk-test-convert-romaji "bd") "べん"))
        (should (equal (nskk-test-convert-romaji "bl") "ぼん")))))

  (nskk-context "p-row hatsuon"
    (nskk-it "pz pk pj pd pl convert to ぱん ぴん ぷん ぺん ぽん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "pz") "ぱん"))
        (should (equal (nskk-test-convert-romaji "pk") "ぴん"))
        (should (equal (nskk-test-convert-romaji "pj") "ぷん"))
        (should (equal (nskk-test-convert-romaji "pd") "ぺん"))
        (should (equal (nskk-test-convert-romaji "pl") "ぽん")))))

  (nskk-context "x-row hatsuon extensions"
    (nskk-it "xz xk xj xd xl convert to しゃん しん しゅん しぇん しょん"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "xk") "しん"))
        (should (equal (nskk-test-convert-romaji "xk") "しん"))
        (should (equal (nskk-converter-lookup "xz") "しゃん"))
        (should (equal (nskk-converter-lookup "xj") "しゅん"))
        (should (equal (nskk-converter-lookup "xd") "しぇん"))
        (should (equal (nskk-converter-lookup "xl") "しょん"))
        (should (equal (nskk-test-convert-romaji "xz") "しゃん"))
        (should (equal (nskk-test-convert-romaji "xj") "しゅん"))
        (should (equal (nskk-test-convert-romaji "xd") "しぇん"))
        (should (equal (nskk-test-convert-romaji "xl") "しょん"))
        (should-not (equal (nskk-test-convert-romaji "xz") "しゃ"))
        (should-not (equal (nskk-test-convert-romaji "xj") "しゅ")))))

  (nskk-context "c-row hatsuon extensions"
    (nskk-it "cz ck cj cd cl convert to ちゃん ちん ちゅん ちぇん ちょん"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "cz") "ちゃん"))
        (should (equal (nskk-converter-lookup "ck") "ちん"))
        (should (equal (nskk-converter-lookup "cj") "ちゅん"))
        (should (equal (nskk-converter-lookup "cd") "ちぇん"))
        (should (equal (nskk-converter-lookup "cl") "ちょん"))
        (should (equal (nskk-test-convert-romaji "cz") "ちゃん"))
        (should (equal (nskk-test-convert-romaji "ck") "ちん"))
        (should (equal (nskk-test-convert-romaji "cj") "ちゅん"))
        (should (equal (nskk-test-convert-romaji "cd") "ちぇん"))
        (should (equal (nskk-test-convert-romaji "cl") "ちょん"))
        (should-not (equal (nskk-test-convert-romaji "cz") "ちゃ"))
        (should-not (equal (nskk-test-convert-romaji "ck") "ちんん"))
        (should-not (equal (nskk-test-convert-romaji "cj") "ちゅ"))))))

  (nskk-context "f-row hatsuon extensions"
    (nskk-it "fz fk fj fd fl convert to ふぁん ふぃん ふん ふぇん ふぉん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "fz") "ふぁん"))
        (should (equal (nskk-test-convert-romaji "fk") "ふぃん"))
        (should (equal (nskk-test-convert-romaji "fj") "ふん"))
        (should (equal (nskk-test-convert-romaji "fd") "ふぇん"))
        (should (equal (nskk-test-convert-romaji "fl") "ふぉん")))))

  (nskk-context "j-row hatsuon extensions"
    (nskk-it "jz jk jj jd jl convert to じゃん じん じゅん じぇん じょん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "jz") "じゃん"))
        (should (equal (nskk-test-convert-romaji "jk") "じん"))
        (should (equal (nskk-test-convert-romaji "jj") "じゅん"))
        (should (equal (nskk-test-convert-romaji "jd") "じぇん"))
        (should (equal (nskk-test-convert-romaji "jl") "じょん")))))

  (nskk-context "v-row hatsuon extensions"
    (nskk-it "vz vk vj vd vl convert to ゔぁん ゔぃん ゔん ゔぇん ゔぉん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "vz") "ゔぁん"))
        (should (equal (nskk-test-convert-romaji "vk") "ゔぃん"))
        (should (equal (nskk-test-convert-romaji "vj") "ゔん"))
        (should (equal (nskk-test-convert-romaji "vd") "ゔぇん"))
        (should (equal (nskk-test-convert-romaji "vl") "ゔぉん")))))


;;;;
;;;; 4. 二重母音拡張 (Diphthong Extension) Tests
;;;;

(nskk-describe "AZIK diphthong (二重母音拡張) rules"
  (nskk-context "k-row diphthong"
    (nskk-it "kq kh kw kp convert to かい くう けい こう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "kq") "かい"))
        (should (equal (nskk-test-convert-romaji "kh") "くう"))
        (should (equal (nskk-test-convert-romaji "kw") "けい"))
        (should (equal (nskk-test-convert-romaji "kp") "こう")))))

  (nskk-context "s-row diphthong"
    (nskk-it "sq sh sw sp convert to さい すう せい そう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "sq") "さい"))
        (should (equal (nskk-converter-lookup "sh") "すう"))
        (should (equal (nskk-test-convert-romaji "sh") "すう"))
        (should (equal (nskk-test-convert-romaji "sw") "せい"))
        (should (equal (nskk-test-convert-romaji "sp") "そう")))))

  (nskk-context "t-row diphthong"
    (nskk-it "tq th tw tp convert to たい つう てい とう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "tq") "たい"))
        (should (equal (nskk-converter-lookup "th") "つう"))
        (should (equal (nskk-test-convert-romaji "th") "つう"))
        (should (equal (nskk-test-convert-romaji "tw") "てい"))
        (should (equal (nskk-test-convert-romaji "tp") "とう")))))

  (nskk-context "n-row diphthong"
    (nskk-it "nq nh nw np rules exist in the lookup table"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "nq") "ない"))
        (should (equal (nskk-converter-lookup "nh") "ぬう"))
        (should (equal (nskk-converter-lookup "nw") "ねい"))
        (should (equal (nskk-converter-lookup "np") "のう")))))

  (nskk-context "h-row diphthong"
    (nskk-it "hq hw hp convert to はい へい ほう and lookup table is complete"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "hq") "はい"))
        (should (equal (nskk-converter-lookup "hh") "ふう"))
        (should (equal (nskk-converter-lookup "hw") "へい"))
        (should (equal (nskk-converter-lookup "hp") "ほう"))
        (should (equal (nskk-test-convert-romaji "hq") "はい"))
        (should (equal (nskk-test-convert-romaji "hw") "へい"))
        (should (equal (nskk-test-convert-romaji "hp") "ほう")))))

  (nskk-context "m-row diphthong"
    (nskk-it "mq mh mw mp convert to まい むう めい もう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "mq") "まい"))
        (should (equal (nskk-test-convert-romaji "mh") "むう"))
        (should (equal (nskk-test-convert-romaji "mw") "めい"))
        (should (equal (nskk-test-convert-romaji "mp") "もう")))))

  (nskk-context "y-row diphthong"
    (nskk-it "yq yh yw yp convert to やい ゆう えい よう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "yq") "やい"))
        (should (equal (nskk-test-convert-romaji "yh") "ゆう"))
        (should (equal (nskk-test-convert-romaji "yw") "えい"))
        (should (equal (nskk-test-convert-romaji "yp") "よう")))))

  (nskk-context "r-row diphthong"
    (nskk-it "rq rh rw rp convert to らい るう れい ろう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "rq") "らい"))
        (should (equal (nskk-test-convert-romaji "rh") "るう"))
        (should (equal (nskk-test-convert-romaji "rw") "れい"))
        (should (equal (nskk-test-convert-romaji "rp") "ろう")))))

  (nskk-context "w-row diphthong"
    (nskk-it "wq wh wp convert to わい うう うぉー"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "wq") "わい"))
        (should (equal (nskk-converter-lookup "wh") "うう"))
        (should (equal (nskk-test-convert-romaji "wh") "うう"))
        (should (equal (nskk-converter-lookup "ww") "うぇい"))
        (should (equal (nskk-converter-lookup "wp") "うぉー"))
        (should (equal (nskk-test-convert-romaji "wq") "わい"))
        (should (equal (nskk-test-convert-romaji "wp") "うぉー")))))

  (nskk-context "g-row diphthong"
    (nskk-it "gq gh gw gp convert to がい ぐう げい ごう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "gq") "がい"))
        (should (equal (nskk-test-convert-romaji "gh") "ぐう"))
        (should (equal (nskk-test-convert-romaji "gw") "げい"))
        (should (equal (nskk-test-convert-romaji "gp") "ごう")))))

  (nskk-context "z-row diphthong"
    (nskk-it "zq zh zw zp convert to ざい ずう ぜい ぞう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "zq") "ざい"))
        (should (equal (nskk-test-convert-romaji "zh") "ずう"))
        (should (equal (nskk-test-convert-romaji "zw") "ぜい"))
        (should (equal (nskk-test-convert-romaji "zp") "ぞう")))))

  (nskk-context "d-row diphthong"
    (nskk-it "dq dh dw dp convert to だい づう でい どう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "dq") "だい"))
        (should (equal (nskk-converter-lookup "dh") "づう"))
        (should (equal (nskk-test-convert-romaji "dh") "づう"))
        (should (equal (nskk-test-convert-romaji "dw") "でい"))
        (should (equal (nskk-test-convert-romaji "dp") "どう")))))

  (nskk-context "b-row diphthong"
    (nskk-it "bq bh bw bp convert to ばい ぶう べい ぼう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "bq") "ばい"))
        (should (equal (nskk-test-convert-romaji "bh") "ぶう"))
        (should (equal (nskk-test-convert-romaji "bw") "べい"))
        (should (equal (nskk-test-convert-romaji "bp") "ぼう")))))

  (nskk-context "p-row diphthong"
    (nskk-it "pq ph pw convert to ぱい ぷう ぺい and lookup table is complete"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "pq") "ぱい"))
        (should (equal (nskk-converter-lookup "ph") "ぷう"))
        (should (equal (nskk-converter-lookup "pw") "ぺい"))
        (should (equal (nskk-converter-lookup "pp") "ぽう"))
        (should (equal (nskk-test-convert-romaji "pq") "ぱい"))
        (should (equal (nskk-test-convert-romaji "ph") "ぷう"))
        (should (equal (nskk-test-convert-romaji "pw") "ぺい")))))

  (nskk-context "x-row diphthong extensions"
    (nskk-it "xq xh xw xp convert to しゃい しゅう しぇい しょう"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "xw") "しぇい"))
        (should (equal (nskk-test-convert-romaji "xw") "しぇい"))
        (should (equal (nskk-converter-lookup "xq") "しゃい"))
        (should (equal (nskk-converter-lookup "xh") "しゅう"))
        (should (equal (nskk-converter-lookup "xp") "しょう"))
        (should (equal (nskk-test-convert-romaji "xq") "しゃい"))
        (should (equal (nskk-test-convert-romaji "xh") "しゅう"))
        (should (equal (nskk-test-convert-romaji "xp") "しょう"))
        (should-not (equal (nskk-test-convert-romaji "xh") "しゅ"))
        (should-not (equal (nskk-test-convert-romaji "xh") "しゅうう")))))

  (nskk-context "c-row diphthong extensions"
    (nskk-it "cq ch cw cp convert to ちゃい ちゅう ちぇい ちょう"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "ch") "ちゅう"))
        (should (equal (nskk-test-convert-romaji "ch") "ちゅう"))
        (should (equal (nskk-converter-lookup "cq") "ちゃい"))
        (should (equal (nskk-converter-lookup "cw") "ちぇい"))
        (should (equal (nskk-converter-lookup "cp") "ちょう"))
        (should (equal (nskk-test-convert-romaji "cq") "ちゃい"))
        (should (equal (nskk-test-convert-romaji "cw") "ちぇい"))
        (should (equal (nskk-test-convert-romaji "cp") "ちょう")))))

  (nskk-context "f-row diphthong extensions"
    (nskk-it "fq fh fw fp convert to ふぁい ふう ふぇい ふぉー"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "fq") "ふぁい"))
        (should (equal (nskk-test-convert-romaji "fh") "ふう"))
        (should (equal (nskk-test-convert-romaji "fw") "ふぇい"))
        (should (equal (nskk-test-convert-romaji "fp") "ふぉー")))))

  (nskk-context "j-row diphthong extensions"
    (nskk-it "jq jh jw jp convert to じゃい じゅう じぇい じょう"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "jq") "じゃい"))
        (should (equal (nskk-test-convert-romaji "jh") "じゅう"))
        (should (equal (nskk-test-convert-romaji "jw") "じぇい"))
        (should (equal (nskk-test-convert-romaji "jp") "じょう")))))

  (nskk-context "v-row diphthong extensions"
    (nskk-it "vq vh vw vp convert to ゔぁい ゔう ゔぇい ゔぉー"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "vq") "ゔぁい"))
        (should (equal (nskk-test-convert-romaji "vh") "ゔう"))
        (should (equal (nskk-test-convert-romaji "vw") "ゔぇい"))
        (should (equal (nskk-test-convert-romaji "vp") "ゔぉー"))))))


;;;;
;;;; 5. 拗音互換キー (Yō-on Compatibility Key) Tests
;;;;

(nskk-describe "AZIK youon (拗音互換キー) rules"
  (nskk-context "kg-row youon"
    (nskk-it "kga kgu kge kgo convert to きゃ きゅ きぇ きょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "kga") "きゃ"))
        (should (equal (nskk-test-convert-romaji "kgu") "きゅ"))
        (should (equal (nskk-test-convert-romaji "kge") "きぇ"))
        (should (equal (nskk-test-convert-romaji "kgo") "きょ")))))

  (nskk-context "hg-row youon"
    (nskk-it "hga hgu hge hgo convert to ひゃ ひゅ ひぇ ひょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "hga") "ひゃ"))
        (should (equal (nskk-test-convert-romaji "hgu") "ひゅ"))
        (should (equal (nskk-test-convert-romaji "hge") "ひぇ"))
        (should (equal (nskk-test-convert-romaji "hgo") "ひょ")))))

  (nskk-context "mg-row youon"
    (nskk-it "mga mgu mge mgo convert to みゃ みゅ みぇ みょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "mga") "みゃ"))
        (should (equal (nskk-test-convert-romaji "mgu") "みゅ"))
        (should (equal (nskk-test-convert-romaji "mge") "みぇ"))
        (should (equal (nskk-test-convert-romaji "mgo") "みょ")))))

  (nskk-context "rg-row youon"
    (nskk-it "rga rgu rge rgo convert to りゃ りゅ りぇ りょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "rga") "りゃ"))
        (should (equal (nskk-test-convert-romaji "rgu") "りゅ"))
        (should (equal (nskk-test-convert-romaji "rge") "りぇ"))
        (should (equal (nskk-test-convert-romaji "rgo") "りょ")))))

  (nskk-context "ry-row youon (DDSKK-compatible y-prefix + AZIK extensions)"
    (nskk-it "rya ryu rye ryo convert to りゃ りゅ りぇ りょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "rya") "りゃ"))
        (should (equal (nskk-test-convert-romaji "ryu") "りゅ"))
        (should (equal (nskk-test-convert-romaji "rye") "りぇ"))
        (should (equal (nskk-test-convert-romaji "ryo") "りょ"))))

    (nskk-it "ryp ryh ryw ryq convert to りょう りゅう りぇい りゃい"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ryp") "りょう"))
        (should (equal (nskk-test-convert-romaji "ryh") "りゅう"))
        (should (equal (nskk-test-convert-romaji "ryw") "りぇい"))
        (should (equal (nskk-test-convert-romaji "ryq") "りゃい"))))

    (nskk-it "ryz ryk ryj ryd ryl convert to りゃん りぃん りゅん りぇん りょん (hatsuon extensions)"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ryz") "りゃん"))
        (should (equal (nskk-test-convert-romaji "ryk") "りぃん"))
        (should (equal (nskk-test-convert-romaji "ryj") "りゅん"))
        (should (equal (nskk-test-convert-romaji "ryd") "りぇん"))
        (should (equal (nskk-test-convert-romaji "ryl") "りょん")))))

  (nskk-context "gg-row youon"
    (nskk-it "gga ggu gge ggo rules exist in the lookup table"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "gga") "ぎゃ"))
        (should (equal (nskk-converter-lookup "ggu") "ぎゅ"))
        (should (equal (nskk-converter-lookup "gge") "ぎぇ"))
        (should (equal (nskk-converter-lookup "ggo") "ぎょ")))))

  (nskk-context "jg-row youon"
    (nskk-it "jga jgu jge jgo convert to じゃ じゅ じぇ じょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "jga") "じゃ"))
        (should (equal (nskk-test-convert-romaji "jgu") "じゅ"))
        (should (equal (nskk-test-convert-romaji "jge") "じぇ"))
        (should (equal (nskk-test-convert-romaji "jgo") "じょ")))))

  (nskk-context "bg-row youon"
    (nskk-it "bga bgu bge bgo convert to びゃ びゅ びぇ びょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "bga") "びゃ"))
        (should (equal (nskk-test-convert-romaji "bgu") "びゅ"))
        (should (equal (nskk-test-convert-romaji "bge") "びぇ"))
        (should (equal (nskk-test-convert-romaji "bgo") "びょ")))))

  (nskk-context "pg-row youon"
    (nskk-it "pga pgu pge pgo convert to ぴゃ ぴゅ ぴぇ ぴょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "pga") "ぴゃ"))
        (should (equal (nskk-test-convert-romaji "pgu") "ぴゅ"))
        (should (equal (nskk-test-convert-romaji "pge") "ぴぇ"))
        (should (equal (nskk-test-convert-romaji "pgo") "ぴょ")))))

  (nskk-context "youon with hatsuon extension"
    (nskk-it "kg + hatsuon codes produce youon followed by ん"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "kgz") "きゃん"))
        (should (equal (nskk-test-convert-romaji "kgk") "きぃん"))
        (should (equal (nskk-test-convert-romaji "kgj") "きゅん"))
        (should (equal (nskk-test-convert-romaji "kgd") "きぇん"))
        (should (equal (nskk-test-convert-romaji "kgl") "きょん")))))

  (nskk-context "youon with diphthong extension"
    (nskk-it "kg + diphthong codes produce youon followed by double vowel"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "kgq") "きゃい"))
        (should (equal (nskk-test-convert-romaji "kgh") "きゅう"))
        (should (equal (nskk-test-convert-romaji "kgw") "きぇい"))
        (should (equal (nskk-test-convert-romaji "kgp") "きょう"))))))


;;;;
;;;; 6. 同指打鍵互換キー (Same-Finger Compatibility Key) Tests
;;;;

(nskk-describe "AZIK same-finger (同指打鍵互換キー) keys"
  (nskk-it "kf converts to き"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kf") "き"))))

  (nskk-it "nf rule exists in the lookup table"
    (nskk-with-azik-style
      (should (equal (nskk-converter-lookup "nf") "ぬ"))))

  (nskk-it "mf converts to む"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "mf") "む"))))

  (nskk-it "gf converts to ぐ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "gf") "ぐ"))))

  (nskk-it "pf converts to ぷ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "pf") "ぷ"))))

  (nskk-it "rf converts to る"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "rf") "る"))))

  (nskk-it "yf converts to ゆ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "yf") "ゆ")))))

  (nskk-it "hf converts to ふ (same-finger alt for hu/fu)"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "hf") "ふ"))))


;;;;
;;;; 7. 特殊拡張 (Special Extension) Tests
;;;;

(nskk-describe "AZIK special extension (特殊拡張) shortcuts"
  (nskk-context "k-row shortcuts"
    (nskk-it "km kr kt convert to かも から こと"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "km") "かも"))
        (should (equal (nskk-test-convert-romaji "kr") "から"))
        (should (equal (nskk-test-convert-romaji "kt") "こと")))))

  (nskk-context "g-row shortcuts"
    (nskk-it "gr gt convert to がら ごと"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "gr") "がら"))
        (should (equal (nskk-test-convert-romaji "gt") "ごと")))))

  (nskk-context "z-row shortcuts"
    (nskk-it "zr converts to ざる"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "zr") "ざる")))))

  (nskk-context "s-row shortcuts"
    (nskk-it "st sr convert to した する"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "st") "した"))
        (should (equal (nskk-test-convert-romaji "sr") "する")))))

  (nskk-context "t-row shortcuts"
    (nskk-it "tb tm tr convert to たび ため たら and lookup table is complete"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "tt") "たち"))
        (should (equal (nskk-converter-lookup "tb") "たび"))
        (should (equal (nskk-converter-lookup "tm") "ため"))
        (should (equal (nskk-converter-lookup "tr") "たら"))
        (should (equal (nskk-test-convert-romaji "tb") "たび"))
        (should (equal (nskk-test-convert-romaji "tm") "ため"))
        (should (equal (nskk-test-convert-romaji "tr") "たら")))))

  (nskk-context "d-row shortcuts"
    (nskk-it "dt ds dm convert to だち です でも"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "dt") "だち"))
        (should (equal (nskk-test-convert-romaji "ds") "です"))
        (should (equal (nskk-test-convert-romaji "dm") "でも")))))

  (nskk-context "n-row shortcuts"
    (nskk-it "nr nt nb rules exist in the lookup table"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "nr") "なる"))
        (should (equal (nskk-converter-lookup "nt") "にち"))
        (should (equal (nskk-converter-lookup "nb") "ねば")))))

  (nskk-context "h-row shortcuts"
    (nskk-it "ht converts to ひと"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ht") "ひと")))))

  (nskk-context "b-row shortcuts"
    (nskk-it "bt converts to びと"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "bt") "びと")))))

  (nskk-context "m-row shortcuts"
    (nskk-it "ms mt mn convert to ます また もの"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ms") "ます"))
        (should (equal (nskk-test-convert-romaji "mt") "また"))
        (should (equal (nskk-test-convert-romaji "mn") "もの")))))

  (nskk-context "y-row shortcuts"
    (nskk-it "yr converts to よる"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "yr") "よる")))))

  (nskk-context "r-row shortcuts"
    (nskk-it "rr rule exists in the lookup table"
      (nskk-with-azik-style
        (should (equal (nskk-converter-lookup "rr") "られ")))))

  (nskk-context "w-row shortcuts"
    (nskk-it "wt wr convert to わた われ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "wt") "わた"))
        (should (equal (nskk-test-convert-romaji "wr") "われ"))))))


;;;;
;;;; 8. Q-key Behavior Tests
;;;;

(nskk-describe "AZIK q-key behavior"
  (nskk-it "q-key works as extension key for diphthongs without standalone definition"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kq") "かい"))
      (should (equal (nskk-test-convert-romaji "sq") "さい"))
      (should (equal (nskk-test-convert-romaji "tq") "たい"))))

  (nskk-it "standalone q has no conversion rule"
    (nskk-with-azik-style
      (should-not (nskk-converter-lookup "q"))))

  (nskk-it "q-key with pending consonant input completes diphthong"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kq") "かい"))
      (should (equal (nskk-test-convert-romaji "hq") "はい"))
      (should (equal (nskk-test-convert-romaji "sq") "さい"))
      (should (equal (nskk-test-convert-romaji "tq") "たい")))))


;;;;
;;;; 9. Compatibility Tests
;;;;

(nskk-describe "AZIK compatibility with standard romaji"
  (nskk-context "basic vowels"
    (nskk-it "a i u e o convert to あ い う え お in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "a") "あ"))
        (should (equal (nskk-test-convert-romaji "i") "い"))
        (should (equal (nskk-test-convert-romaji "u") "う"))
        (should (equal (nskk-test-convert-romaji "e") "え"))
        (should (equal (nskk-test-convert-romaji "o") "お")))))

  (nskk-context "k-row standard romaji"
    (nskk-it "ka ki ku ke ko still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ka") "か"))
        (should (equal (nskk-test-convert-romaji "ki") "き"))
        (should (equal (nskk-test-convert-romaji "ku") "く"))
        (should (equal (nskk-test-convert-romaji "ke") "け"))
        (should (equal (nskk-test-convert-romaji "ko") "こ")))))

  (nskk-context "g-row standard romaji"
    (nskk-it "ga gi gu ge go still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ga") "が"))
        (should (equal (nskk-test-convert-romaji "gi") "ぎ"))
        (should (equal (nskk-test-convert-romaji "gu") "ぐ"))
        (should (equal (nskk-test-convert-romaji "ge") "げ"))
        (should (equal (nskk-test-convert-romaji "go") "ご")))))

  (nskk-context "s-row standard romaji"
    (nskk-it "sa shi su se so still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "sa") "さ"))
        (should (equal (nskk-test-convert-romaji "shi") "し"))
        (should (equal (nskk-test-convert-romaji "su") "す"))
        (should (equal (nskk-test-convert-romaji "se") "せ"))
        (should (equal (nskk-test-convert-romaji "so") "そ")))))

  (nskk-context "t-row standard romaji"
    (nskk-it "ta chi tsu te to still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ta") "た"))
        (should (equal (nskk-test-convert-romaji "chi") "ち"))
        (should (equal (nskk-test-convert-romaji "tsu") "つ"))
        (should (equal (nskk-test-convert-romaji "te") "て"))
        (should (equal (nskk-test-convert-romaji "to") "と")))))

  (nskk-context "n-row standard romaji"
    (nskk-it "na ni nu ne no nn still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "na") "な"))
        (should (equal (nskk-test-convert-romaji "ni") "に"))
        (should (equal (nskk-test-convert-romaji "nu") "ぬ"))
        (should (equal (nskk-test-convert-romaji "ne") "ね"))
        (should (equal (nskk-test-convert-romaji "no") "の"))
        (should (equal (nskk-test-convert-romaji "nn") "ん")))))

  (nskk-context "h-row standard romaji"
    (nskk-it "ha hi fu he ho still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ha") "は"))
        (should (equal (nskk-test-convert-romaji "hi") "ひ"))
        (should (equal (nskk-test-convert-romaji "fu") "ふ"))
        (should (equal (nskk-test-convert-romaji "he") "へ"))
        (should (equal (nskk-test-convert-romaji "ho") "ほ")))))

  (nskk-context "m-row standard romaji"
    (nskk-it "ma mi mu me mo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ma") "ま"))
        (should (equal (nskk-test-convert-romaji "mi") "み"))
        (should (equal (nskk-test-convert-romaji "mu") "む"))
        (should (equal (nskk-test-convert-romaji "me") "め"))
        (should (equal (nskk-test-convert-romaji "mo") "も")))))

  (nskk-context "y-row standard romaji"
    (nskk-it "ya yu yo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ya") "や"))
        (should (equal (nskk-test-convert-romaji "yu") "ゆ"))
        (should (equal (nskk-test-convert-romaji "yo") "よ")))))

  (nskk-context "r-row standard romaji"
    (nskk-it "ra ri ru re ro still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ra") "ら"))
        (should (equal (nskk-test-convert-romaji "ri") "り"))
        (should (equal (nskk-test-convert-romaji "ru") "る"))
        (should (equal (nskk-test-convert-romaji "re") "れ"))
        (should (equal (nskk-test-convert-romaji "ro") "ろ")))))

  (nskk-context "w-row standard romaji"
    (nskk-it "wa wo still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "wa") "わ"))
        (should (equal (nskk-test-convert-romaji "wo") "を")))))

  (nskk-context "youon (digraphs) standard romaji"
    (nskk-it "kya kyu kyo sha shu sho cha chu cho still convert in AZIK mode"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "kya") "きゃ"))
        (should (equal (nskk-test-convert-romaji "kyu") "きゅ"))
        (should (equal (nskk-test-convert-romaji "kyo") "きょ"))
        (should (equal (nskk-test-convert-romaji "sha") "しゃ"))
        (should (equal (nskk-test-convert-romaji "shu") "しゅ"))
        (should (equal (nskk-test-convert-romaji "sho") "しょ"))
        (should (equal (nskk-test-convert-romaji "cha") "ちゃ"))
        (should (equal (nskk-test-convert-romaji "chu") "ちゅ"))
        (should (equal (nskk-test-convert-romaji "cho") "ちょ")))))

  (nskk-context "c-row compat (c=ちゃ行)"
    (nskk-it "ca ci cu ce co convert to ちゃ ち ちゅ ちぇ ちょ"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "ca") "ちゃ"))
        (should (equal (nskk-test-convert-romaji "ci") "ち"))
        (should (equal (nskk-test-convert-romaji "cu") "ちゅ"))
        (should (equal (nskk-test-convert-romaji "ce") "ちぇ"))
        (should (equal (nskk-test-convert-romaji "co") "ちょ")))))

  (nskk-context "complete word conversion"
    (nskk-it "can convert complete words using AZIK features"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "sz") "さん"))
        (should (equal (nskk-test-convert-romaji "tl") "とん"))
        (should (equal (nskk-test-convert-romaji "tp") "とう"))
        (should (equal (nskk-test-convert-romaji "kw") "けい"))
        (should (equal (nskk-test-convert-romaji "kztp") "かんとう"))))))


;;;;
;;;; 10. Integration Tests
;;;;

(nskk-describe "AZIK integration"
  (nskk-it "basic AZIK input sequences convert correctly"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kz") "かん"))
      (should (equal (nskk-test-convert-romaji "sz") "さん"))
      (should (equal (nskk-test-convert-romaji "tp") "とう"))))

  (nskk-it "complex word conversion using AZIK combinations"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kgp") "きょう"))
      (should (equal (nskk-test-convert-romaji "kgpto") "きょうと"))
      (should (equal (nskk-test-convert-romaji "tpkyo") "とうきょ"))
      (should (equal (nskk-test-convert-romaji "szpo") "さんぽ"))))

  (nskk-it "mixed standard and AZIK input converts correctly"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kakz") "かかん"))
      (should (equal (nskk-test-convert-romaji "sask") "さしん"))
      (should (equal (nskk-test-convert-romaji "tatq") "たたい"))
      (should (equal (nskk-test-convert-romaji "tachitq") "たちたい"))))

  (nskk-context "x/c prefix compound sequences"
    (nskk-it "x prefix compounds with extension keys work correctly"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "xhka") "しゅうか"))
        (should (equal (nskk-test-convert-romaji "xhkak") "しゅうかく"))
        (should (equal (nskk-test-convert-romaji "xhkaq") "しゅうかい"))
        (should (equal (nskk-test-convert-romaji "xpto") "しょうと"))
        (should (equal (nskk-test-convert-romaji "xzka") "しゃんか"))))

    (nskk-it "c prefix compounds with extension keys work correctly"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "cpka") "ちょうか"))
        (should (equal (nskk-test-convert-romaji "cpto") "ちょうと"))
        (should (equal (nskk-test-convert-romaji "czka") "ちゃんか"))
        (should (equal (nskk-test-convert-romaji "cjka") "ちゅんか"))))

    (nskk-it "x/c prefixes in longer compound words"
      (nskk-with-azik-style
        (should (equal (nskk-test-convert-romaji "xhkakki") "しゅうかくき"))
        (should (equal (nskk-test-convert-romaji "cpkahi") "ちょうかひ"))))))


;;;;
;;;; 11. Regression Tests
;;;;

(nskk-describe "AZIK regression tests"
  (nskk-it "kz produces かん not かn"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "kz") "かん"))
      (should-not (equal (nskk-test-convert-romaji "kz") "かn"))))

  (nskk-it "semicolon produces small tsu and works in context"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji ";") "っ"))
      (should (equal (nskk-test-convert-romaji ";ka") "っか"))))

  (nskk-it "colon produces chouon"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji ":") "ー")))))


;;;;
;;;; 10a. Prolog-level Tests
;;;;

(nskk-describe "AZIK Prolog-level rules"
  (nskk-it "azik-rule/2 facts are directly queryable via Prolog"
    (nskk-with-azik-style
      (let ((results (nskk-prolog-query '(azik-rule ";" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "っ")))
      (let ((results (nskk-prolog-query '(azik-rule "kz" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "かん")))
      (let ((results (nskk-prolog-query '(azik-rule "kq" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "かい")))
      (let ((results (nskk-prolog-query '(azik-rule "nga" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "にゃ")))
      (let ((results (nskk-prolog-query '(azik-rule "ss" \?k))))
        (should results)
        (should (equal (nskk-prolog-walk '\?k (car results)) "せい")))))

  (nskk-it "bridge rule makes azik-rule/2 facts accessible via romaji-to-kana/2 enumeration"
    (nskk-with-azik-style
      (let ((all-romajis (nskk-prolog-query-all-values
                          '(romaji-to-kana \?r \?k) '\?r)))
        (should all-romajis)
        (should (member ";" all-romajis))
        (should (member "kz" all-romajis))
        (should (member "kq" all-romajis)))))

  (nskk-it "azik-rule/2 contains a substantial number of facts (at least 534)"
    (nskk-with-azik-style
      (let ((results (nskk-prolog-query '(azik-rule \?r \?k))))
        (should results)
        (should (>= (length results) 534)))))

  (nskk-it "switching to standard style resets the hot-path hash cache"
    (nskk-with-azik-style
      (should (nskk-prolog-query '(azik-rule "kz" \?k)))
      (should (equal (nskk-converter-lookup "kz") "かん")))
    (nskk-with-standard-style
      (should-not (nskk-converter-lookup "kz"))
      (should (equal (nskk-test-convert-romaji "ka") "か"))))

  (nskk-it "2-char youon prefixes produce :incomplete in the hash table"
    (nskk-with-azik-style
      (should (eq (nskk-converter-lookup "k") :incomplete))
      (should (eq (nskk-converter-lookup "h") :incomplete))
      (should (eq (nskk-converter-lookup "m") :incomplete))
      (should (eq (nskk-converter-lookup "kg") :incomplete))
      (should (eq (nskk-converter-lookup "hg") :incomplete))
      (should (eq (nskk-converter-lookup "mg") :incomplete))
      (should (eq (nskk-converter-lookup "rg") :incomplete))
      (should (eq (nskk-converter-lookup "jg") :incomplete))
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
    (let ((expansion (macroexpand-1
                      '(nskk-azik-hatsuon "k" "か" "き" "く" "け" "こ"))))
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 5))
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
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 2))
      (should (eq (car (nth 1 expansion)) 'nskk-azik-hatsuon))
      (should (eq (car (nth 2 expansion)) 'nskk-azik-double-vowel))))

  (nskk-it "nskk-azik-youon expands to 4 base rules + extensions"
    (let ((expansion (macroexpand-1
                      '(nskk-azik-youon "kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ"))))
      (should (eq (car expansion) 'progn))
      (should (= (length (cdr expansion)) 5))
      (should (eq (car (nth 5 expansion)) 'nskk-azik-extensions)))))




;;;;
;;;; 12. 外来語拡張 (Foreign Word Extension) Tests
;;;;

(nskk-describe "AZIK foreign word extensions (外来語拡張)"
  (nskk-it "tgi converts to てぃ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgi") "てぃ"))))

  (nskk-it "tgu converts to てゅ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgu") "てゅ"))))

  (nskk-it "dci converts to でぃ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dci") "でぃ"))))

  (nskk-it "dcu converts to でゅ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dcu") "でゅ"))))

  (nskk-it "wso converts to うぉ"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wso") "うぉ"))))

  (nskk-it "all foreign extensions exist in azik-rule/2 Prolog predicate"
    (nskk-with-azik-style
      (dolist (rule '(("tgi" "てぃ") ("tgu" "てゅ") ("dci" "でぃ") ("dcu" "でゅ") ("wso" "うぉ")))
        (let* ((romaji (car rule))
               (kana   (cadr rule))
               (results (nskk-prolog-query `(azik-rule ,romaji \?k))))
          (should results)
          (should (equal (nskk-prolog-walk '\?k (car results)) kana))))))

  (nskk-it "foreign extensions in context produce correct compound output"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgika") "てぃか"))
      (should (equal (nskk-test-convert-romaji "dcika") "でぃか")))))

;;;;
;;;; 12a. 外来語撥音拡張 (Foreign Word Hatsuon Extension) Tests
;;;;

(nskk-describe "AZIK foreign hatsuon extensions (外来語撥音拡張)"
  (nskk-it "tgk converts to てぃん"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgk") "てぃん"))))

  (nskk-it "tgj converts to とぅん"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgj") "とぅん"))))

  (nskk-it "dck converts to でぃん"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dck") "でぃん"))))

  (nskk-it "dcj converts to どぅん"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dcj") "どぅん"))))

  (nskk-it "wsok converts to うぉん"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wsok") "うぉん"))))

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
      (should (equal (nskk-test-convert-romaji "tgq") "てぃい"))))

  (nskk-it "tgh converts to てゅー"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgh") "てゅー"))))

  (nskk-it "tgw converts to とぅう"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgw") "とぅう"))))

  (nskk-it "tgp converts to とぅー"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "tgp") "とぅー"))))

  (nskk-it "dcq converts to でぃい"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dcq") "でぃい"))))

  (nskk-it "dch converts to でゅー"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dch") "でゅー"))))

  (nskk-it "dcw converts to どぅう"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dcw") "どぅう"))))

  (nskk-it "dcp converts to どぅー"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "dcp") "どぅー"))))

  (nskk-it "wsoq converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wsoq") "うぉお"))))

  (nskk-it "wsoh converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wsoh") "うぉお"))))

  (nskk-it "wsow converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wsow") "うぉお"))))

  (nskk-it "wsop converts to うぉお"
    (nskk-with-azik-style
      (should (equal (nskk-test-convert-romaji "wsop") "うぉお"))))

  (nskk-it "all foreign double-vowel rules exist in azik-rule/2 Prolog predicate"
    (nskk-with-azik-style
      (dolist (rule '(("tgq" "てぃい") ("tgh" "てゅー") ("tgw" "とぅう") ("tgp" "とぅー")
                      ("dcq" "でぃい") ("dch" "でゅー") ("dcw" "どぅう") ("dcp" "どぅー")
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
      (should (equal (nskk-test-convert-romaji "xhkak") "しゅうかく"))
      (should (equal (nskk-test-convert-romaji "xhkaq") "しゅうかい"))))

  (nskk-it "all compound rules are absent from azik-rule/2 (hash-only)"
    (nskk-with-azik-style
      (dolist (rule '(("kak" "かく") ("kaq" "かい") ("kakz" "かかん") ("wso" "うぉ")))
        (let ((romaji (car rule)))
          (unless (equal romaji "wso")
            (should-not (nskk-prolog-query `(azik-rule ,romaji \?k))))))))

  (nskk-it "wso hash entry is restored after finalize via compound rules"
    (nskk-with-azik-style
      (should (equal (nskk-converter-lookup "wso") "うぉ")))))


;;;;
;;;; 14. Property-Based Tests
;;;;

(nskk-describe "AZIK property-based: extension row consistency"

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
        (should (equal (cdr pair) (nskk-test-convert-romaji (car pair)))))))

  (nskk-it "k-row diphthong: full pipeline converts correctly"
    (nskk-with-azik-style
      (dolist (pair '(("kq" . "かい") ("kh" . "くう") ("kw" . "けい")
                      ("kp" . "こう")))
        (should (equal (cdr pair) (nskk-test-convert-romaji (car pair))))))))


;;;;
;;;; 15. Data-Provider: AZIK hatsuon (撥音拡張) categories
;;;;

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

(nskk-deftest-table azik-hatsuon-x-row
  :description "AZIK x-row 撥音拡張: complete (non-demoted) entries in hash lookup"
  :columns (input expected)
  :rows (("xz" "しゃん")
         ("xj" "しゅん")
         ("xd" "しぇん")
         ("xl" "しょん"))
  :body (nskk-with-azik-style
          (should (equal expected (nskk-converter-lookup input)))))

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

(nskk-deftest-table azik-diphthong-full-rows
  :columns (prefix q-expected h-expected w-expected p-expected)
  :rows    (("k" "かい" "くう" "けい" "こう")
            ("m" "まい" "むう" "めい" "もう")
            ("y" "やい" "ゆう" "えい" "よう")
            ("r" "らい" "るう" "れい" "ろう")
            ("g" "がい" "ぐう" "げい" "ごう")
            ("z" "ざい" "ずう" "ぜい" "ぞう")
            ("b" "ばい" "ぶう" "べい" "ぼう")
            ("f" "ふぁい" "ふう" "ふぇい" "ふぉー")
            ("j" "じゃい" "じゅう" "じぇい" "じょう")
            ("v" "ゔぁい" "ゔう" "ゔぇい" "ゔぉー"))
  :description "AZIK diphthong rules: q/h/w/p keys produce correct double-vowel forms"
  :body (nskk-with-azik-style
          (should (equal q-expected (nskk-converter-lookup (concat prefix "q"))))
          (should (equal h-expected (nskk-converter-lookup (concat prefix "h"))))
          (should (equal w-expected (nskk-converter-lookup (concat prefix "w"))))
          (should (equal p-expected (nskk-converter-lookup (concat prefix "p"))))))


;;;;
;;;; Contract-Based PBT: nskk-test-convert-romaji
;;;;

(nskk-describe "nskk-test-convert-romaji AZIK contract"
  (nskk-it "should return a non-empty string for any AZIK pattern"
    (let ((failures nil))
      (dotimes (_ 50)
        (nskk-with-azik-style
          (let* ((input  (nskk--pbt-random-choice (nskk--pbt-get-all-azik-patterns)))
                 (result (nskk-test-convert-romaji input)))
            (unless (stringp result)
              (push (list :postcondition-failed :input input :result result) failures))
            (when (string-empty-p result)
              (push (list :invariant-failed "result is empty string" input) failures)))))
      (when failures
        (ert-fail (format "Contract test `nskk-test-convert-romaji' AZIK: %d failures:\n%S"
                          (length failures) failures))))))


;;;;
;;;; Property-Based Tests: AZIK-wide invariants
;;;;

(nskk-property-test azik-any-rule-lookup-returns-valid-type
  ((pattern azik-rule))
  (nskk-with-azik-style
    (let ((result (nskk-converter-lookup pattern)))
      (or (stringp result)
          (eq result :incomplete)
          (null result))))
  50)

(nskk-property-test azik-hatsuon-z-suffix-produces-string-ending-in-ん
  ((row-pattern azik-rule))  ; used to drive random iteration count only
  (nskk-with-azik-style
    (let* ((hatsuon-categories (cl-remove-if-not
                                (lambda (cat) (string-prefix-p "hatsuon-" (symbol-name (car cat))))
                                nskk--pbt-azik-categories))
           (category (nskk--pbt-random-choice hatsuon-categories))
           (patterns (cdr category))
           (z-pattern (car patterns))
           (result (nskk-converter-lookup z-pattern)))
      (and (stringp result)
           (string-suffix-p "ん" result))))
  50)


;;;;
;;;; Exhaustive Property Test: entire AZIK rule set never crashes on lookup
;;;;

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
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (let ((call-count 0))
              (nskk--init-azik-rules/k
               (lambda ()
                 (cl-incf call-count)))
              (should (= call-count 1))))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                 saved-romaji-table))))

  (nskk-it "on-done is called after hash table is populated"
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (let ((hash-populated nil))
              (nskk--init-azik-rules/k
               (lambda ()
                 (setq hash-populated
                       (stringp (nskk-converter-lookup "kz")))))
              (should hash-populated)))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
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
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?a)))
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?i)))
      (should (nskk-prolog-holds-p `(azik-key-extends "sh" ,?u)))))

  (nskk-it "does not assert pairs for keys with no longer hash entry"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p `(azik-key-extends "kz" \?ch)))))

  (nskk-it "deduplicates: same (prefix, ch) pair asserted at most once"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table (nskk-romaji-table))))
        (unwind-protect
            (progn
              (puthash "ka"  "か"   (nskk-romaji-table))
              (puthash "kab" "かあ" (nskk-romaji-table))
              (nskk--azik-init-key-extend-facts)
              (should (= 1 (length (nskk-prolog-query
                                    `(azik-key-extends "k" ,?a))))))
          (clrhash (nskk-romaji-table))
          (maphash (lambda (k v) (puthash k v (nskk-romaji-table))) saved))))))

(nskk-describe "azik-nonvowel-ext/1 Prolog rule"
  (nskk-it "succeeds for a prefix that has at least one non-vowel extension"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table (nskk-romaji-table))))
        (unwind-protect
            (progn
              (nskk--azik-init-char-facts)
              (puthash "tx"  "てすと"  (nskk-romaji-table))
              (puthash "txk" "てすとん" (nskk-romaji-table))
              (nskk--azik-init-key-extend-facts)
              (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
              (nskk-prolog-<- (azik-nonvowel-ext \?k)
                (azik-key-extends \?k \?ch)
                (not (azik-vowel-char \?ch)))
              (should (nskk-prolog-holds-p '(azik-nonvowel-ext "tx"))))
          (clrhash (nskk-romaji-table))
          (maphash (lambda (k v) (puthash k v (nskk-romaji-table))) saved)))))

  (nskk-it "fails for a prefix whose extensions are all vowels"
    (nskk-prolog-test-with-isolated-db
      (let ((saved (copy-hash-table (nskk-romaji-table))))
        (unwind-protect
            (progn
              (nskk--azik-init-char-facts)
              (puthash "sh"  "すう" (nskk-romaji-table))
              (puthash "sha" "しゃ" (nskk-romaji-table))
              (puthash "shi" "し"   (nskk-romaji-table))
              (nskk--azik-init-key-extend-facts)
              (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
              (nskk-prolog-<- (azik-nonvowel-ext \?k)
                (azik-key-extends \?k \?ch)
                (not (azik-vowel-char \?ch)))
              (should-not (nskk-prolog-holds-p '(azik-nonvowel-ext "sh"))))
          (clrhash (nskk-romaji-table))
          (maphash (lambda (k v) (puthash k v (nskk-romaji-table))) saved))))))

(nskk-describe "azik-vowel-shadow/1 Prolog rule"
  (nskk-it "holds for \"sh\" after AZIK init — vowel-only extensions"
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-vowel-shadow "sh")))))

  (nskk-it "holds for \"ch\" after AZIK init — vowel-only extensions"
    (nskk-with-azik-style
      (should (nskk-prolog-holds-p '(azik-vowel-shadow "ch")))))

  (nskk-it "does not hold for keys with no extensions (e.g. \"kz\")"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-vowel-shadow "kz")))))

  (nskk-it "does not hold for non-AZIK complete rules"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-vowel-shadow "ka"))))))

(nskk-describe "nskk--azik-classify-key/k"
  (nskk-it "calls succeed with :vowel-shadow for \"sh\""
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
      (let ((saved (copy-hash-table (nskk-romaji-table))))
        (unwind-protect
            (progn
              (nskk-prolog-retract-all 'azik-rule 2)
              (nskk-prolog-set-index 'azik-rule 2 :hash)
              (nskk-prolog-assert '((azik-rule "tx" "てすと")))
              (puthash "tx"  "てすと"  (nskk-romaji-table))
              (puthash "txk" "てすとん" (nskk-romaji-table))
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
          (clrhash (nskk-romaji-table))
          (maphash (lambda (k v) (puthash k v (nskk-romaji-table))) saved))))))

(nskk-describe "nskk--azik-finalize-hash-table"
  (nskk-it "calls on-done continuation exactly once"
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (let ((call-count 0))
              (nskk--azik-finalize-hash-table/k
               (lambda () (cl-incf call-count)))
              (should (= call-count 1))))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                 saved-romaji-table))))

  (nskk-it "\"sh\" remains complete in hash after finalize (vowel-shadow)"
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (equal (nskk-converter-lookup "sh") "すう")))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                 saved-romaji-table))))

  (nskk-it "\"sh\" is recorded in nskk--azik-vowel-shadow-set after finalize"
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (gethash "sh" nskk--azik-vowel-shadow-set)))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                 saved-romaji-table))))

  (nskk-it "registers :incomplete for proper prefixes of AZIK rules"
    (let ((saved-romaji-table (copy-hash-table (nskk-romaji-table))))
      (unwind-protect
          (nskk-prolog-test-with-isolated-db
            (nskk--init-azik-rules)
            (should (eq (gethash "kg" (nskk-romaji-table)) :incomplete)))
        (clrhash (nskk-romaji-table))
        (maphash (lambda (k v) (puthash k v (nskk-romaji-table)))
                 saved-romaji-table)))))

(nskk-property-test converter-convert/k-azik-exactly-one-branch-called
  ((pattern azik-rule))
  (nskk-with-azik-style
    (let ((branch-called nil))
      (nskk-converter-convert/k
       pattern
       (lambda (_kana _rest)  (setq branch-called 'match))
       (lambda (_romaji)      (setq branch-called 'incomplete))
       (lambda ()             (setq branch-called 'fail)))
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
      (nskk-azik-hatsuon "k" "か" "き" "く" "け" "こ")
      (should (equal (nskk-prolog-query-value '(azik-rule "kz" \?v) '\?v) "かん"))
      (should (equal (nskk-prolog-query-value '(azik-rule "kk" \?v) '\?v) "きん"))
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
      (nskk-azik-double-vowel "k" "か" "く" "け" "こ")
      (should (equal (nskk-prolog-query-value '(azik-rule "kq" \?v) '\?v) "かい"))
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
      (should (equal (nskk-prolog-query-value '(azik-rule "kga" \?v) '\?v) "きゃ"))
      (should (equal (nskk-prolog-query-value '(azik-rule "kgu" \?v) '\?v) "きゅ"))
      (should (equal (nskk-prolog-query-value '(azik-rule "kgo" \?v) '\?v) "きょ"))))

  (nskk-it "also generates hatsuon and double vowel extension rules"
    (nskk-prolog-test-with-isolated-db
      (nskk-azik-youon "kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
      (should (equal (nskk-prolog-query-value '(azik-rule "kgz" \?v) '\?v) "きゃん"))
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
      (nskk-with-azik-style
        (should (equal "っ" (gethash "+" (nskk-romaji-table)))))))

  (nskk-it "US101 rules do not include + → っ"
    (let ((nskk-azik-keyboard-type 'us101))
      (nskk-with-azik-style
        (should-not (gethash "+" (nskk-romaji-table)))))))

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
  (nskk-it "should NOT register plus (handled via plus-jp106 path, not colon-arm)"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-colon-trigger-char ?+)))))
  (nskk-it "should NOT register other chars like a"
    (nskk-with-azik-style
      (should-not (nskk-prolog-holds-p '(azik-colon-trigger-char ?a))))))

(defmacro nskk-test-with-azik-transaction-state (&rest body)
  "Run BODY with isolated AZIK transaction state."
  (declare (indent 0)
           (debug t))
  `(let ((nskk--romaji-table (make-hash-table :test 'equal))
         (nskk--azik-vowel-shadow-set (make-hash-table :test 'equal))
         (nskk--style-registry (copy-tree nskk--style-registry))
         (nskk--converter-style-transaction-hash-tables
          (copy-sequence nskk--converter-style-transaction-hash-tables))
         (nskk--converter-style-transaction-variables
          (copy-sequence nskk--converter-style-transaction-variables))
         (nskk--azik-toggle-key-state nil)
         (nskk-mode-map (make-sparse-keymap))
         (nskk-azik-keyboard-type 'us101))
     (nskk-prolog-with-database-fields
         ((database (make-hash-table :test 'equal))
          (database-tails (make-hash-table :test 'equal))
          (index-config (make-hash-table :test 'equal))
          (hash-indices (make-hash-table :test 'equal))
          (trie-indices (make-hash-table :test 'equal))
          (index-bucket-tail-cache (make-hash-table :test 'equal)))
       (nskk-converter-register-style-transaction-hash-table
        'nskk--azik-vowel-shadow-set)
       (nskk-converter-register-style-transaction-variable
        'nskk--azik-toggle-key-state)
       (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
       (nskk-converter-add-rule "old" "旧")
       (nskk-prolog-set-index 'transaction-sentinel 1 :hash)
       (progn
         (nskk-prolog-assert '((transaction-sentinel intact)))
         (nskk-prolog-assert '((azik-toggle-key us101 "[")))
         (nskk-prolog-assert '((azik-toggle-key jp106 "@"))))
       (puthash "old-shadow" t nskk--azik-vowel-shadow-set)
       (define-key nskk-mode-map (kbd "C-c o") 'ignore)
       (cl-progv '(nskk-mode-map)
           (list nskk-mode-map)
         ,@body))))
(defun nskk-test--azik-transaction-references ()
  "Return the live AZIK transaction state references."
  (list
   (nskk-romaji-table)
   (nskk-prolog-database)
   (nskk-prolog-database-tails)
   (nskk-prolog-index-config)
   (nskk-prolog-hash-indices)
   (nskk-prolog-trie-indices)
   (nskk-prolog-index-bucket-tail-cache)
   nskk--azik-vowel-shadow-set))
(defun nskk-test--azik-load-style-condition (style)
  "Load STYLE and return the signaled condition type, if any."
  (condition-case
    condition
    (progn
      (nskk-converter-load-style style)
      nil)
    (quit (car condition))
    (error (car condition))))
(defun nskk-test--should-retain-azik-transaction-state (references mode-map mode-map-copy)
  "Assert exact rollback to REFERENCES and MODE-MAP-COPY."
  (cl-mapc
    (lambda (before after)
      (should (eq before after)))
    references
    (nskk-test--azik-transaction-references))
  (should (eq mode-map nskk-mode-map))
  (should (equal mode-map-copy nskk-mode-map))
  (should (equal (nskk-converter-lookup "old") "旧"))
  (should-not (nskk-converter-lookup "new"))
  (should (gethash "old-shadow" nskk--azik-vowel-shadow-set))
  (should-not (gethash "new-shadow" nskk--azik-vowel-shadow-set))
  (should (eq (lookup-key nskk-mode-map (kbd "C-c o")) 'ignore))
  (should (nskk-prolog-holds-p '(transaction-sentinel intact))))
(defun nskk-test--signal-during-azik-style (condition)
  "Mutate staged AZIK state, then signal CONDITION when non-nil."
  (nskk-converter-add-rule "new" "新")
  (puthash "new-shadow" t nskk--azik-vowel-shadow-set)
  (define-key nskk-mode-map (kbd "C-c n") 'next-line)
  (when condition
    (signal condition nil)))
(nskk-describe
  "AZIK style transactions"
  (nskk-it
    "rolls back exact state after initializer error and quit"
    (dolist (condition '(error quit))
      (nskk-test-with-azik-transaction-state
        (nskk-converter-register-style
          'transaction-signal
          (apply-partially #'nskk-test--signal-during-azik-style condition))
        (let ((references (nskk-test--azik-transaction-references))
              (mode-map nskk-mode-map)
              (mode-map-copy (copy-keymap nskk-mode-map)))
          (should
            (eq (nskk-test--azik-load-style-condition 'transaction-signal) condition))
          (nskk-test--should-retain-azik-transaction-state
            references
            mode-map
            mode-map-copy)))))
  (nskk-it
    "rolls back exact state after publish error and quit"
    (dolist (condition '(error quit))
      (nskk-test-with-azik-transaction-state
        (nskk-converter-register-style
          'transaction-publish-failure
          (apply-partially #'nskk-test--signal-during-azik-style nil))
        (let ((references (nskk-test--azik-transaction-references))
              (mode-map nskk-mode-map)
              (mode-map-copy (copy-keymap nskk-mode-map)))
          (cl-letf
            (((symbol-function 'nskk--converter-replace-keymap-contents)
                (apply-partially
                  (lambda (signaled-condition target source)
                    (setcdr target (cdr source))
                    (signal signaled-condition nil))
                  condition)))
            (should
              (eq
                (nskk-test--azik-load-style-condition 'transaction-publish-failure)
                condition)))
          (nskk-test--should-retain-azik-transaction-state
            references
            mode-map
            mode-map-copy)))))
  (nskk-it
    "commits AZIK stores while preserving keymap identity and facts"
    (nskk-test-with-azik-transaction-state
      (let ((references (nskk-test--azik-transaction-references))
            (mode-map nskk-mode-map))
        (should (eq (nskk-converter-load-style 'azik) 'azik))
        (cl-mapc
          (lambda (before after)
            (should-not (eq before after)))
          references
          (nskk-test--azik-transaction-references))
        (should (eq mode-map nskk-mode-map))
        (should-not (nskk-converter-lookup "old"))
        (should (equal (nskk-converter-lookup "kz") "かん"))
        (should-not (gethash "old-shadow" nskk--azik-vowel-shadow-set))
        (should (gethash "sh" nskk--azik-vowel-shadow-set))
        (should (eq (lookup-key nskk-mode-map (kbd "C-c o")) 'ignore))
        (should (eq (lookup-key nskk-mode-map (kbd "[")) 'nskk-toggle-japanese-mode))
        (should (nskk-prolog-holds-p '(transaction-sentinel intact)))))))

(provide 'nskk-azik-test)

;;; nskk-azik-test.el ends here
