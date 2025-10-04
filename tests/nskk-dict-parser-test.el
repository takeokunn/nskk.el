;;; nskk-dict-parser-test.el --- Tests for nskk-dict-parser -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-dict-parser.elのテストを実装します。

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-dict-parser)

;;; 候補解析テスト

(nskk-deftest nskk-dict-parser-test-parse-candidates-basic
  "基本的な候補リストのパース"
  :tags '(:unit :dict-parser)
  (let ((result (nskk-parse-candidates "/漢字/幹事/")))
    (should (= (length result) 2))
    (should (equal (car result) '("漢字" . nil)))
    (should (equal (cadr result) '("幹事" . nil)))))

(nskk-deftest nskk-dict-parser-test-parse-candidates-with-annotation
  "注釈付き候補のパース"
  :tags '(:unit :dict-parser)
  (let ((result (nskk-parse-candidates "/愛;love/哀;sorrow/藍;indigo/")))
    (should (= (length result) 3))
    (should (equal (car result) '("愛" . "love")))
    (should (equal (cadr result) '("哀" . "sorrow")))
    (should (equal (caddr result) '("藍" . "indigo")))))

(nskk-deftest nskk-dict-parser-test-parse-candidates-mixed
  "注釈ありなし混在の候補"
  :tags '(:unit :dict-parser)
  (let ((result (nskk-parse-candidates "/足;(下肢)/脚;(家具)/葦/")))
    (should (= (length result) 3))
    (should (equal (car result) '("足" . "(下肢)")))
    (should (equal (cadr result) '("脚" . "(家具)")))
    (should (equal (caddr result) '("葦" . nil)))))

(nskk-deftest nskk-dict-parser-test-parse-candidates-empty
  "空の候補リスト"
  :tags '(:unit :dict-parser)
  (let ((result (nskk-parse-candidates "//")))
    (should (null result))))

(nskk-deftest nskk-dict-parser-test-parse-candidates-error
  "不正な候補リスト形式"
  :tags '(:unit :dict-parser)
  (should-error (nskk-parse-candidates "漢字/幹事/"))
  (should-error (nskk-parse-candidates "/漢字/幹事")))

;;; エントリ解析テスト

(nskk-deftest nskk-dict-parser-test-parse-entry-basic
  "基本的なエントリのパース"
  :tags '(:unit :dict-parser)
  (let ((entry (nskk-parse-entry "かんじ /漢字/幹事/")))
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-midashi entry) "かんじ"))
    (should (= (length (nskk-dict-entry-candidates entry)) 2))))

(nskk-deftest nskk-dict-parser-test-parse-entry-okuri-ari
  "送り仮名ありエントリのパース"
  :tags '(:unit :dict-parser)
  (let ((entry (nskk-parse-entry "わたr /渡/航/")))
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-midashi entry) "わたr"))
    (should (nskk-dict-parser--has-romaji-suffix-p
             (nskk-dict-entry-midashi entry)))))

(nskk-deftest nskk-dict-parser-test-parse-entry-with-annotation
  "注釈付きエントリのパース"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "あし /足;(下肢)/脚;(家具)/葦/"))
         (candidates (nskk-dict-entry-candidates entry)))
    (should (equal (nth 0 candidates) '("足" . "(下肢)")))
    (should (equal (nth 1 candidates) '("脚" . "(家具)")))
    (should (equal (nth 2 candidates) '("葦" . nil)))))

(nskk-deftest nskk-dict-parser-test-parse-entry-symbol
  "記号エントリのパース"
  :tags '(:unit :dict-parser)
  (let ((entry (nskk-parse-entry "! /！/感嘆符/")))
    (should (equal (nskk-dict-entry-midashi entry) "!"))
    (should (= (length (nskk-dict-entry-candidates entry)) 2))))

(nskk-deftest nskk-dict-parser-test-parse-entry-error
  "不正なエントリ形式"
  :tags '(:unit :dict-parser)
  (should-error (nskk-parse-entry "かんじ"))
  (should-error (nskk-parse-entry "/漢字/幹事/"))
  (should-error (nskk-parse-entry "かんじ/漢字/幹事/")))

;;; エントリ検証テスト

(nskk-deftest nskk-dict-parser-test-validate-entry-valid
  "正常なエントリの検証"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "かんじ /漢字/幹事/"))
         (errors (nskk-dict-parser-validate-entry entry)))
    (should (null errors))))

(nskk-deftest nskk-dict-parser-test-validate-okuri-ari-valid
  "正常な送り仮名ありエントリの検証"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "わたr /渡/航/"))
         (errors (nskk-dict-parser-validate-okuri-ari-entry entry)))
    (should (null errors))))

(nskk-deftest nskk-dict-parser-test-validate-okuri-ari-invalid
  "不正な送り仮名ありエントリの検証"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "わたし /私/"))
         (errors (nskk-dict-parser-validate-okuri-ari-entry entry)))
    (should (member "Okuri-ari entry must have romaji suffix" errors))))

(nskk-deftest nskk-dict-parser-test-has-romaji-suffix
  "ローマ字接尾辞の判定"
  :tags '(:unit :dict-parser)
  (should (nskk-dict-parser--has-romaji-suffix-p "わたr"))
  (should (nskk-dict-parser--has-romaji-suffix-p "あるk"))
  (should-not (nskk-dict-parser--has-romaji-suffix-p "かんじ"))
  (should-not (nskk-dict-parser--has-romaji-suffix-p "わたし")))

;;; エンコーディング検出テスト

(nskk-deftest nskk-dict-parser-test-detect-encoding-from-header
  "ヘッダーからのエンコーディング検出"
  :tags '(:unit :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert "かんじ /漢字/\n"))
          (should (eq (nskk-dict-parser-detect-encoding temp-file) 'utf-8)))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-parser-test-detect-encoding-eucjp
  "EUC-JP辞書のエンコーディング検出"
  :tags '(:unit :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: euc-jp -*-\n")
            (insert "かんじ /漢字/\n"))
          (should (eq (nskk-dict-parser-detect-encoding temp-file) 'euc-jp)))
      (delete-file temp-file))))

;;; 辞書ファイル全体のパーステスト

(nskk-deftest nskk-dict-parser-test-parse-dictionary-basic
  "基本的な辞書ファイルのパース"
  :tags '(:integration :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert ";; Test dictionary\n")
            (insert ";; okuri-ari entries.\n")
            (insert "わたr /渡/航/\n")
            (insert "わすr /忘/\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "かんじ /漢字/幹事/\n")
            (insert "あい /愛/哀/藍/\n"))
          (let ((dict (nskk-parse-dictionary temp-file)))
            (should (nskk-dict-p dict))
            (should (eq (nskk-dict-encoding dict) 'utf-8))
            (should (= (length (nskk-dict-okuri-ari dict)) 2))
            (should (= (length (nskk-dict-okuri-nasi dict)) 2))
            (should (>= (length (nskk-dict-header dict)) 1))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-parser-test-parse-dictionary-with-annotation
  "注釈付き辞書のパース"
  :tags '(:integration :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "あし /足;(下肢)/脚;(家具)/葦;(植物)/\n"))
          (let* ((dict (nskk-parse-dictionary temp-file))
                 (entry (car (nskk-dict-okuri-nasi dict)))
                 (candidates (nskk-dict-entry-candidates entry)))
            (should (= (length candidates) 3))
            (should (equal (cdr (nth 0 candidates)) "(下肢)"))
            (should (equal (cdr (nth 1 candidates)) "(家具)"))
            (should (equal (cdr (nth 2 candidates)) "(植物)"))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-parser-test-parse-dictionary-empty-lines
  "空行を含む辞書のパース"
  :tags '(:integration :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert "\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "\n")
            (insert "かんじ /漢字/幹事/\n")
            (insert "\n")
            (insert "あい /愛/哀/\n"))
          (let ((dict (nskk-parse-dictionary temp-file)))
            (should (= (length (nskk-dict-okuri-nasi dict)) 2))))
      (delete-file temp-file))))

;;; エントリ文字列化テスト

(nskk-deftest nskk-dict-parser-test-entry-to-string-basic
  "エントリの文字列化（基本）"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "かんじ /漢字/幹事/"))
         (str (nskk-dict-parser-entry-to-string entry)))
    (should (equal str "かんじ /漢字/幹事/"))))

(nskk-deftest nskk-dict-parser-test-entry-to-string-annotation
  "エントリの文字列化（注釈付き）"
  :tags '(:unit :dict-parser)
  (let* ((entry (nskk-parse-entry "あし /足;(下肢)/脚;(家具)/"))
         (str (nskk-dict-parser-entry-to-string entry)))
    (should (equal str "あし /足;(下肢)/脚;(家具)/"))))

;;; 統計情報テスト

(nskk-deftest nskk-dict-parser-test-statistics
  "辞書統計情報の生成"
  :tags '(:unit :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert ";; Test dictionary\n")
            (insert ";; okuri-ari entries.\n")
            (insert "わたr /渡/航/\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "かんじ /漢字/幹事/\n")
            (insert "あい /愛/哀/\n"))
          (let* ((dict (nskk-parse-dictionary temp-file))
                 (stats (nskk-dict-parser-statistics dict)))
            (should (eq (plist-get stats :encoding) 'utf-8))
            (should (= (plist-get stats :okuri-ari-entries) 1))
            (should (= (plist-get stats :okuri-nasi-entries) 2))
            (should (= (plist-get stats :total-entries) 3))))
      (delete-file temp-file))))

;;; エラーハンドリングテスト

(nskk-deftest nskk-dict-parser-test-error-lenient-mode
  "寛容モードでのエラーハンドリング"
  :tags '(:unit :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-"))
        (nskk-dict-parser-error-strategy 'lenient))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "かんじ /漢字/幹事/\n")
            (insert "不正な行\n")  ; 不正なエントリ
            (insert "あい /愛/哀/\n"))
          (let ((dict (nskk-parse-dictionary temp-file)))
            ;; 寛容モードでは不正な行をスキップして継続
            (should (= (length (nskk-dict-okuri-nasi dict)) 2))
            ;; エラーが記録されている
            (should (> (length (nskk-dict-errors dict)) 0))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-parser-test-error-strict-mode
  "厳格モードでのエラーハンドリング"
  :tags '(:unit :dict-parser)
  (let ((temp-file (make-temp-file "nskk-test-dict-"))
        (nskk-dict-parser-error-strategy 'strict))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- coding: utf-8 -*-\n")
            (insert ";; okuri-nasi entries.\n")
            (insert "かんじ /漢字/幹事/\n")
            (insert "不正な行\n"))  ; 不正なエントリ
          ;; 厳格モードではエラーで中断
          (should-error (nskk-parse-dictionary temp-file)))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-parser-test-file-not-found
  "存在しないファイルのエラー"
  :tags '(:unit :dict-parser)
  (should-error (nskk-parse-dictionary "/nonexistent/file.txt")))

(provide 'nskk-dict-parser-test)

;;; nskk-dict-parser-test.el ends here
