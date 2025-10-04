;;; nskk-converter.el --- Romaji to Kana conversion engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
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

;; このファイルはNSKKのローマ字→かな変換エンジンを実装します。
;;
;; 特徴:
;; - 最長一致アルゴリズムによる効率的な変換
;; - 促音（っ）の自動処理（子音重複検出）
;; - 撥音（ん）の文脈依存処理
;; - 未確定入力のプレフィックスマッチング
;; - 状態機械ベースの実装
;;
;; 使用例:
;; (nskk-convert-romaji "kya")        ;; => ("きゃ" "")
;; (nskk-convert-romaji "kk")         ;; => ("" "kk")  ; 未確定
;; (nskk-convert-romaji "kka")        ;; => ("っか" "")
;; (nskk-convert-romaji "n")          ;; => ("" "n")   ; 未確定
;; (nskk-convert-romaji "nn")         ;; => ("ん" "")
;; (nskk-convert-romaji "ka")         ;; => ("か" "")

;;; Code:

(require 'cl-lib)
(require 'nskk-romaji-tables)

;;; カスタマイズ可能変数

(defgroup nskk-converter nil
  "NSKK romaji to kana conversion settings."
  :group 'nskk
  :prefix "nskk-converter-")

(defcustom nskk-converter-use-sokuon t
  "非nilの場合、促音（っ）の自動処理を有効にする。
例: \"kka\" → \"っか\""
  :type 'boolean
  :group 'nskk-converter)

(defcustom nskk-converter-sokuon-chars "kgtdbpszfchjmnyrw"
  "促音として扱う子音文字。
これらの文字が連続した場合、最初の文字を「っ」として処理する。"
  :type 'string
  :group 'nskk-converter)

(defcustom nskk-converter-n-processing-mode 'smart
  "撥音「ん」の処理モード。
- `smart': 文脈依存（n+子音で自動確定、n+母音で継続）
- `explicit': nn, n', xn のみで確定
- `aggressive': n単独でも即座に確定"
  :type '(choice (const :tag "スマート処理" smart)
                 (const :tag "明示的のみ" explicit)
                 (const :tag "積極的確定" aggressive))
  :group 'nskk-converter)

;;; 内部変数

(defvar nskk-converter--max-romaji-length nil
  "ローマ字テーブル内の最大文字列長（内部キャッシュ）。")

;;; 補助関数

(defun nskk-converter--init-max-length ()
  "ローマ字テーブルの最大長を初期化する。"
  (unless nskk-converter--max-romaji-length
    (setq nskk-converter--max-romaji-length
          (nskk-romaji-get-max-length))))

(defsubst nskk-converter--is-vowel (char)
  "CHAR が母音（a, i, u, e, o）かどうかを判定する。"
  (and char (memq char '(?a ?i ?u ?e ?o))))

(defsubst nskk-converter--is-sokuon-char (char)
  "CHAR が促音化可能な子音かどうかを判定する。"
  (and char
       nskk-converter-use-sokuon
       (string-match-p (regexp-quote (char-to-string char))
                       nskk-converter-sokuon-chars)))

(defsubst nskk-converter--can-be-sokuon (str)
  "STR が促音パターン（同じ子音の連続）かどうかを判定する。
例: \"kk\", \"tt\", \"pp\" など"
  (and (>= (length str) 2)
       (= (aref str 0) (aref str 1))
       (nskk-converter--is-sokuon-char (aref str 0))))

(defun nskk-converter--process-sokuon (input)
  "INPUT から促音パターンを処理する。
促音が検出された場合は (\"っ\" . 残り) を返す。
検出されない場合は nil を返す。

例:
  (nskk-converter--process-sokuon \"kka\")  ;; => (\"っ\" . \"ka\")
  (nskk-converter--process-sokuon \"ka\")   ;; => nil
  (nskk-converter--process-sokuon \"kk\")   ;; => nil (未確定)"
  (when (and nskk-converter-use-sokuon
             ;; 3文字以上必要（kk + 続く文字）
             (>= (length input) 3))
    (let ((first-char (aref input 0))
          (second-char (aref input 1)))
      (when (and (= first-char second-char)
                 (nskk-converter--is-sokuon-char first-char)
                 ;; "nn" は促音ではなく撥音
                 (not (= first-char ?n)))
        (cons "っ" (substring input 1))))))

(defun nskk-converter--process-n (input)
  "INPUT の先頭が 'n' の場合の特殊処理を行う。
撥音「ん」として確定できる場合は (\"ん\" . 残り) を返す。
確定できない場合は nil を返す。

処理ルール:
- nn, n', xn → 即座に「ん」として確定
- n + 子音 → スマートモードでは「ん」として確定
- n + 母音 → 確定せず（「な」等の可能性）
- n単独 → モード依存"
  (when (and (>= (length input) 1)
             (= (aref input 0) ?n))
    (cond
     ;; 2文字以上の場合
     ((>= (length input) 2)
      (let ((second-char (aref input 1)))
        (cond
         ;; nn, n', xn → 確定
         ((or (= second-char ?n)
              (= second-char ?')
              (and (= (aref input 0) ?x) (= second-char ?n)))
          (cons "ん" (substring input 2)))

         ;; n + 母音 → 確定しない（na, ni, nu, ne, no の可能性）
         ((nskk-converter--is-vowel second-char)
          nil)

         ;; n + 子音 → smartモードでは確定
         ((eq nskk-converter-n-processing-mode 'smart)
          (cons "ん" (substring input 1)))

         ;; それ以外 → 確定しない
         (t nil))))

     ;; n単独の場合
     ((= (length input) 1)
      (when (eq nskk-converter-n-processing-mode 'aggressive)
        (cons "ん" "")))

     (t nil))))

(defun nskk-converter--longest-match (input)
  "INPUT に対して最長一致するローマ字変換を試みる。
成功した場合は (変換後文字列 . 残り) を返す。
失敗した場合は nil を返す。

アルゴリズム:
1. INPUTの長さから降順に検索
2. 最初にマッチした（最長の）パターンを採用
3. マッチしない場合は nil

例:
  (nskk-converter--longest-match \"kya\")  ;; => (\"きゃ\" . \"\")
  (nskk-converter--longest-match \"ka\")   ;; => (\"か\" . \"\")
  (nskk-converter--longest-match \"xyz\")  ;; => nil"
  (nskk-converter--init-max-length)
  (let* ((max-len (min (length input) nskk-converter--max-romaji-length))
         (result nil))
    (cl-loop for len from max-len downto 1
             until result
             do (let* ((prefix (substring input 0 len))
                       (kana (nskk-romaji-lookup prefix)))
                  (when kana
                    (setq result (cons kana (substring input len))))))
    result))

(defun nskk-converter--has-prefix-match (input)
  "INPUT が何らかのローマ字パターンのプレフィックスかどうかを判定する。
プレフィックスマッチがある場合は t、ない場合は nil を返す。

これは未確定入力の判定に使用される。

例:
  (nskk-converter--has-prefix-match \"k\")   ;; => t (\"ka\", \"ki\" 等の候補)
  (nskk-converter--has-prefix-match \"ky\")  ;; => t (\"kya\", \"kyu\" 等の候補)
  (nskk-converter--has-prefix-match \"xyz\") ;; => nil (候補なし)"
  (let ((candidates (nskk-romaji-get-candidates input)))
    (and candidates t)))

;;; 公開API

(cl-defstruct (nskk-converter-result
               (:constructor nskk-converter-result-create)
               (:copier nil))
  "ローマ字変換の結果を表す構造体。

スロット:
  converted  - 変換確定した文字列
  pending    - 未確定の入力文字列
  consumed   - 処理した文字数"
  (converted "" :type string :read-only t)
  (pending "" :type string :read-only t)
  (consumed 0 :type integer :read-only t))

(defun nskk-convert-romaji (input)
  "INPUT をローマ字→かなに変換する。

返り値は `nskk-converter-result' 構造体:
  - converted: 確定した変換結果
  - pending: 未確定の入力
  - consumed: 消費した文字数

変換ルール:
1. 促音処理（kka → っか）
2. 撥音処理（nn → ん、n+子音 → ん+子音）
3. 最長一致変換
4. プレフィックスマッチング（未確定判定）

例:
  (nskk-convert-romaji \"kya\")
  ;; => #s(nskk-converter-result \"きゃ\" \"\" 3)

  (nskk-convert-romaji \"k\")
  ;; => #s(nskk-converter-result \"\" \"k\" 0)

  (nskk-convert-romaji \"kka\")
  ;; => #s(nskk-converter-result \"っか\" \"\" 3)

  (nskk-convert-romaji \"nn\")
  ;; => #s(nskk-converter-result \"ん\" \"\" 2)"
  (if (string-empty-p input)
      (nskk-converter-result-create :converted ""
                                    :pending ""
                                    :consumed 0)

    ;; 促音処理を試行
    (if-let ((sokuon-result (nskk-converter--process-sokuon input)))
        (let* ((sokuon-kana (car sokuon-result))
               (rest-input (cdr sokuon-result))
               ;; 残りを再帰的に変換
               (rest-result (nskk-convert-romaji rest-input)))
          (nskk-converter-result-create
           :converted (concat sokuon-kana
                             (nskk-converter-result-converted rest-result))
           :pending (nskk-converter-result-pending rest-result)
           :consumed (+ 1 (nskk-converter-result-consumed rest-result))))

      ;; 撥音処理を試行
      (if-let ((n-result (nskk-converter--process-n input)))
          (let* ((n-kana (car n-result))
                 (rest-input (cdr n-result))
                 (rest-result (nskk-convert-romaji rest-input)))
            (nskk-converter-result-create
             :converted (concat n-kana
                               (nskk-converter-result-converted rest-result))
             :pending (nskk-converter-result-pending rest-result)
             :consumed (+ (- (length input) (length rest-input))
                         (nskk-converter-result-consumed rest-result))))

        ;; 最長一致変換を試行
        (if-let ((match-result (nskk-converter--longest-match input)))
            (let* ((kana (car match-result))
                   (rest-input (cdr match-result))
                   (consumed-len (- (length input) (length rest-input)))
                   (rest-result (nskk-convert-romaji rest-input)))
              (nskk-converter-result-create
               :converted (concat kana
                                 (nskk-converter-result-converted rest-result))
               :pending (nskk-converter-result-pending rest-result)
               :consumed (+ consumed-len
                           (nskk-converter-result-consumed rest-result))))

          ;; プレフィックスマッチがあれば未確定として保持
          (if (nskk-converter--has-prefix-match input)
              (nskk-converter-result-create
               :converted ""
               :pending input
               :consumed 0)

            ;; どのパターンにもマッチしない → 最初の1文字をそのまま出力
            (let ((rest-result (nskk-convert-romaji (substring input 1))))
              (nskk-converter-result-create
               :converted (concat (substring input 0 1)
                                 (nskk-converter-result-converted rest-result))
               :pending (nskk-converter-result-pending rest-result)
               :consumed (1+ (nskk-converter-result-consumed rest-result))))))))))

(defun nskk-convert-romaji-simple (input)
  "INPUT をローマ字→かなに変換し、確定部分のみを返す。
`nskk-convert-romaji' の簡易版。

例:
  (nskk-convert-romaji-simple \"kya\")   ;; => \"きゃ\"
  (nskk-convert-romaji-simple \"k\")     ;; => \"\"
  (nskk-convert-romaji-simple \"kka\")   ;; => \"っか\""
  (nskk-converter-result-converted (nskk-convert-romaji input)))

(defun nskk-convert-romaji-pending (input)
  "INPUT をローマ字→かなに変換し、未確定部分のみを返す。

例:
  (nskk-convert-romaji-pending \"kya\")  ;; => \"\"
  (nskk-convert-romaji-pending \"k\")    ;; => \"k\"
  (nskk-convert-romaji-pending \"kka\")  ;; => \"\""
  (nskk-converter-result-pending (nskk-convert-romaji input)))

;;; デバッグ・統計機能

(defun nskk-converter-stats ()
  "変換エンジンの統計情報を返す。

返り値は以下の要素を含むplist:
  :romaji-table-size  - ローマ字テーブルのエントリ数
  :max-romaji-length  - 最大ローマ字長
  :sokuon-enabled     - 促音処理の有効/無効
  :n-processing-mode  - 撥音処理モード"
  (nskk-converter--init-max-length)
  (list :romaji-table-size (length nskk-romaji-table)
        :max-romaji-length nskk-converter--max-romaji-length
        :sokuon-enabled nskk-converter-use-sokuon
        :n-processing-mode nskk-converter-n-processing-mode))

(provide 'nskk-converter)

;;; nskk-converter.el ends here
