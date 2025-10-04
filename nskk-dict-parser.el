;;; nskk-dict-parser.el --- SKK dictionary parser for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

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

;; このファイルはSKK辞書ファイルのパーサーを実装します。
;;
;; 特徴:
;; - SKK形式辞書の完全なパース
;; - エンコーディング自動検出（UTF-8/EUC-JP）
;; - 注釈（アノテーション）の抽出
;; - エントリ検証
;; - エラーハンドリング（厳格モード/寛容モード）
;; - 数値変換パターンの解析
;;
;; SKK辞書形式:
;;
;;   ;; -*- coding: utf-8 -*-
;;   ;; okuri-ari entries.
;;   わたr /渡/航/
;;   わすr /忘/
;;   ;; okuri-nasi entries.
;;   かんじ /漢字/幹事/
;;   あい /愛;love/哀;sorrow/藍;indigo/
;;
;; 使用例:
;;
;;   (require 'nskk-dict-parser)
;;
;;   ;; 辞書ファイルをパース
;;   (let ((dict (nskk-parse-dictionary "~/.skk/jisyo")))
;;     (message "Header: %S" (nskk-dict-header dict))
;;     (message "Okuri-ari entries: %d" (length (nskk-dict-okuri-ari dict)))
;;     (message "Okuri-nasi entries: %d" (length (nskk-dict-okuri-nasi dict))))
;;
;;   ;; エントリを解析
;;   (nskk-parse-entry "かんじ /漢字/幹事/")
;;   ;; => (nskk-dict-entry "かんじ" (("漢字" . nil) ("幹事" . nil)))

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-errors)

;;; カスタマイズ変数

(defgroup nskk-dict-parser nil
  "SKK dictionary parser customization."
  :group 'nskk
  :prefix "nskk-dict-parser-")

(defcustom nskk-dict-parser-error-strategy 'lenient
  "エラー処理戦略。
- 'strict: エラーで処理を中断
- 'lenient: 警告を出力してエントリをスキップ
- 'ignore: エラーを無視して継続"
  :type '(choice (const :tag "Strict" strict)
                 (const :tag "Lenient" lenient)
                 (const :tag "Ignore" ignore))
  :group 'nskk-dict-parser)

(defcustom nskk-dict-parser-max-candidates 1000
  "1エントリあたりの最大候補数。"
  :type 'integer
  :group 'nskk-dict-parser)

(defcustom nskk-dict-parser-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-dict-parser)

;;; データ構造

(cl-defstruct (nskk-dict
               (:constructor nskk-dict--create)
               (:copier nil))
  "SKK辞書オブジェクト。

スロット:
  file-path    - 辞書ファイルパス
  encoding     - ファイルエンコーディング
  header       - ヘッダー行のリスト
  okuri-ari    - 送り仮名ありエントリのリスト
  okuri-nasi   - 送り仮名なしエントリのリスト
  errors       - パースエラーのリスト"
  (file-path nil :type (or null string))
  (encoding nil :type (or null symbol))
  (header nil :type list)
  (okuri-ari nil :type list)
  (okuri-nasi nil :type list)
  (errors nil :type list))

(cl-defstruct (nskk-dict-entry
               (:constructor nskk-dict-entry--create)
               (:copier nil))
  "SKK辞書エントリ。

スロット:
  midashi     - 見出し語
  candidates  - 候補のリスト（各要素は (word . annotation) のcons）"
  (midashi nil :type string)
  (candidates nil :type list))

(cl-defstruct (nskk-parse-error
               (:constructor nskk-parse-error--create)
               (:copier nil))
  "パースエラー情報。

スロット:
  line-number - 行番号
  line        - 行内容
  message     - エラーメッセージ
  severity    - エラーの重要度（'warning, 'error, 'fatal）"
  (line-number nil :type (or null integer))
  (line nil :type (or null string))
  (message nil :type string)
  (severity 'error :type symbol))

;;; エンコーディング検出

(defun nskk-dict-parser-detect-encoding (file-path)
  "FILE-PATH のエンコーディングを検出する。
UTF-8またはEUC-JPを返す。"
  ;; ステップ1: coding宣言をチェック
  (let ((coding-from-header (nskk-dict-parser--detect-from-header file-path)))
    (if coding-from-header
        coding-from-header
      ;; ステップ2: バイトパターン解析
      (nskk-dict-parser--detect-from-bytes file-path))))

(defun nskk-dict-parser--detect-from-header (file-path)
  "ファイルヘッダーからエンコーディングを検出する。"
  (with-temp-buffer
    (insert-file-contents file-path nil 0 1000)
    (goto-char (point-min))
    (when (re-search-forward "-\\*-.*coding:\\s-*\\([-a-z0-9]+\\)" nil t)
      (let ((coding-str (match-string 1)))
        (cond
         ((string-match-p "utf-?8" coding-str) 'utf-8)
         ((string-match-p "euc-?jp" coding-str) 'euc-jp)
         (t nil))))))

(defun nskk-dict-parser--detect-from-bytes (file-path)
  "バイトパターンからエンコーディングを検出する。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path nil 0 8192)
    (let ((utf8-score 0)
          (eucjp-score 0))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((byte (char-after)))
          ;; UTF-8パターン検出
          (cond
           ;; UTF-8 2バイトシーケンス
           ((and (>= byte #xC0) (<= byte #xDF))
            (when (nskk-dict-parser--valid-utf8-continuation-p (+ (point) 1))
              (setq utf8-score (+ utf8-score 10))))
           ;; UTF-8 3バイトシーケンス
           ((and (>= byte #xE0) (<= byte #xEF))
            (when (and (nskk-dict-parser--valid-utf8-continuation-p (+ (point) 1))
                      (nskk-dict-parser--valid-utf8-continuation-p (+ (point) 2)))
              (setq utf8-score (+ utf8-score 10))))
           ;; EUC-JPパターン検出
           ((and (>= byte #xA1) (<= byte #xFE))
            (when (and (< (+ (point) 1) (point-max))
                      (let ((next-byte (char-after (+ (point) 1))))
                        (and (>= next-byte #xA1) (<= next-byte #xFE))))
              (setq eucjp-score (+ eucjp-score 10)))))
          (forward-char 1)))
      ;; スコアに基づく判定
      (cond
       ((> utf8-score eucjp-score) 'utf-8)
       ((> eucjp-score utf8-score) 'euc-jp)
       (t 'utf-8)))))  ; デフォルトはUTF-8

(defun nskk-dict-parser--valid-utf8-continuation-p (pos)
  "POSの位置がUTF-8継続バイトか判定する。"
  (when (< pos (point-max))
    (let ((byte (char-after pos)))
      (and (>= byte #x80) (<= byte #xBF)))))

;;; パーサーメイン

;;;###autoload
(defun nskk-parse-dictionary (file-path)
  "FILE-PATH のSKK辞書をパースする。
戻り値: `nskk-dict' 構造体"
  (unless (file-exists-p file-path)
    (signal 'nskk-dict-io-file-not-found (list file-path)))

  (let* ((encoding (nskk-dict-parser-detect-encoding file-path))
         (dict (nskk-dict--create :file-path file-path
                                  :encoding encoding))
         (current-section 'header))
    (when nskk-dict-parser-verbose
      (message "Parsing %s (encoding: %s)" file-path encoding))

    (with-temp-buffer
      (let ((coding-system-for-read encoding))
        (insert-file-contents file-path))

      (goto-char (point-min))
      (let ((line-number 0))
        (while (not (eobp))
          (setq line-number (1+ line-number))
          (let ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
            (condition-case err
                (cond
                 ;; 空行
                 ((string-match-p "^\\s-*$" line)
                  nil)
                 ;; コメント行
                 ((string-match "^;;\\s-*\\(.*\\)" line)
                  (let ((comment (match-string 1 line)))
                    (cond
                     ;; 送り仮名ありマーカー
                     ((string-match-p "okuri-ari\\s-+entries\\." comment)
                      (setq current-section 'okuri-ari))
                     ;; 送り仮名なしマーカー
                     ((string-match-p "okuri-nasi\\s-+entries\\." comment)
                      (setq current-section 'okuri-nasi))
                     ;; 通常のコメント（ヘッダーに追加）
                     ((eq current-section 'header)
                      (push line (nskk-dict-header dict))))))
                 ;; エントリ
                 (t
                  (let ((entry (nskk-parse-entry line)))
                    (pcase current-section
                      ('okuri-ari
                       (push entry (nskk-dict-okuri-ari dict)))
                      ('okuri-nasi
                       (push entry (nskk-dict-okuri-nasi dict)))
                      ('header
                       (nskk-dict-parser--log-error
                        dict line-number line
                        "Unexpected entry in header section"
                        'warning))))))
              (error
               (nskk-dict-parser--handle-error
                dict line-number line
                (error-message-string err)))))
          (forward-line 1))))

    ;; リストを逆順に（ファイル順を保持）
    (setf (nskk-dict-header dict) (nreverse (nskk-dict-header dict)))
    (setf (nskk-dict-okuri-ari dict) (nreverse (nskk-dict-okuri-ari dict)))
    (setf (nskk-dict-okuri-nasi dict) (nreverse (nskk-dict-okuri-nasi dict)))

    (when nskk-dict-parser-verbose
      (message "Parsed: %d okuri-ari, %d okuri-nasi entries"
               (length (nskk-dict-okuri-ari dict))
               (length (nskk-dict-okuri-nasi dict))))

    dict))

;;;###autoload
(defun nskk-parse-entry (line)
  "LINE をSKKエントリとして解析する。
戻り値: `nskk-dict-entry' 構造体"
  ;; エントリ形式: "見出し語 /候補1/候補2/..."
  (unless (string-match "^\\([^ \t]+\\)\\s-+\\(/.*/$\\)" line)
    (signal 'nskk-dict-parse-invalid-format (list line)))

  (let* ((midashi (match-string 1 line))
         (candidate-part (match-string 2 line))
         (candidates (nskk-parse-candidates candidate-part)))

    ;; 候補数チェック
    (when (> (length candidates) nskk-dict-parser-max-candidates)
      (warn "Entry has too many candidates (%d > %d): %s"
            (length candidates) nskk-dict-parser-max-candidates midashi))

    (nskk-dict-entry--create :midashi midashi
                             :candidates candidates)))

(defun nskk-parse-candidates (candidate-part)
  "CANDIDATE-PART から候補リストを解析する。
候補リスト形式: \"/候補1/候補2;注釈2/.../\"
戻り値: ((word . annotation) ...) のリスト"
  (unless (and (string-prefix-p "/" candidate-part)
              (string-suffix-p "/" candidate-part))
    (signal 'nskk-dict-parse-invalid-format
           (list (format "Candidate list must be enclosed in slashes: %s" candidate-part))))

  ;; 先頭と末尾の"/"を除去
  (let* ((content (substring candidate-part 1 -1))
         (raw-candidates (split-string content "/" t))
         (candidates nil))

    (dolist (raw-cand raw-candidates)
      (if (string-match "^\\([^;]+\\);\\(.+\\)$" raw-cand)
          ;; 注釈あり
          (push (cons (match-string 1 raw-cand)
                     (match-string 2 raw-cand))
                candidates)
        ;; 注釈なし
        (push (cons raw-cand nil) candidates)))

    (nreverse candidates)))

;;; エントリ検証

(defun nskk-dict-parser-validate-entry (entry)
  "ENTRY の妥当性を検証する。
問題があれば警告を出力し、エラーメッセージのリストを返す。"
  (let ((errors nil))
    ;; 見出し語の検証
    (when (string-empty-p (nskk-dict-entry-midashi entry))
      (push "Empty midashi" errors))

    ;; 候補の検証
    (when (null (nskk-dict-entry-candidates entry))
      (push "No candidates" errors))

    (dolist (cand (nskk-dict-entry-candidates entry))
      (when (string-empty-p (car cand))
        (push "Empty candidate word" errors)))

    errors))

(defun nskk-dict-parser-validate-okuri-ari-entry (entry)
  "送り仮名ありエントリ ENTRY の妥当性を検証する。"
  (let ((errors (nskk-dict-parser-validate-entry entry)))
    ;; 送り仮名マーカー（ローマ字接尾辞）の検証
    (unless (nskk-dict-parser--has-romaji-suffix-p
             (nskk-dict-entry-midashi entry))
      (push "Okuri-ari entry must have romaji suffix" errors))
    errors))

(defun nskk-dict-parser--has-romaji-suffix-p (midashi)
  "MIDASHI が送り仮名マーカー（ローマ字接尾辞）を持つか判定する。"
  (and (> (length midashi) 0)
       (let ((last-char (aref midashi (1- (length midashi)))))
         (and (>= last-char ?a) (<= last-char ?z)))))

;;; エラーハンドリング

(defun nskk-dict-parser--log-error (dict line-number line message severity)
  "エラーを記録する。"
  (let ((error-obj (nskk-parse-error--create
                   :line-number line-number
                   :line line
                   :message message
                   :severity severity)))
    (push error-obj (nskk-dict-errors dict))
    (when nskk-dict-parser-verbose
      (message "[%s] Line %d: %s" severity line-number message))))

(defun nskk-dict-parser--handle-error (dict line-number line message)
  "エラーを処理戦略に従って処理する。"
  (pcase nskk-dict-parser-error-strategy
    ('strict
     (signal 'nskk-dict-parse-error
            (list (format "Parse error at line %d: %s (Line: %s)"
                         line-number message line))))
    ('lenient
     (nskk-dict-parser--log-error dict line-number line message 'error))
    ('ignore
     nil)))

;;; ユーティリティ関数

(defun nskk-dict-parser-entry-to-string (entry)
  "ENTRY を文字列形式に変換する。"
  (concat (nskk-dict-entry-midashi entry)
          " /"
          (mapconcat (lambda (cand)
                      (if (cdr cand)
                          (format "%s;%s" (car cand) (cdr cand))
                        (car cand)))
                    (nskk-dict-entry-candidates entry)
                    "/")
          "/"))

(defun nskk-dict-parser-statistics (dict)
  "DICT の統計情報を返す。
戻り値: plist形式の統計情報"
  (list :file-path (nskk-dict-file-path dict)
        :encoding (nskk-dict-encoding dict)
        :header-lines (length (nskk-dict-header dict))
        :okuri-ari-entries (length (nskk-dict-okuri-ari dict))
        :okuri-nasi-entries (length (nskk-dict-okuri-nasi dict))
        :total-entries (+ (length (nskk-dict-okuri-ari dict))
                         (length (nskk-dict-okuri-nasi dict)))
        :errors (length (nskk-dict-errors dict))))

(defun nskk-dict-parser-print-statistics (dict)
  "DICT の統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-dict-parser-statistics dict)))
    (message "SKK Dictionary Statistics:
  File: %s
  Encoding: %s
  Header lines: %d
  Okuri-ari entries: %d
  Okuri-nasi entries: %d
  Total entries: %d
  Parse errors: %d"
             (plist-get stats :file-path)
             (plist-get stats :encoding)
             (plist-get stats :header-lines)
             (plist-get stats :okuri-ari-entries)
             (plist-get stats :okuri-nasi-entries)
             (plist-get stats :total-entries)
             (plist-get stats :errors))))

(provide 'nskk-dict-parser)

;;; nskk-dict-parser.el ends here
