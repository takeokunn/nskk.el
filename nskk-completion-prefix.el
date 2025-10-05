;;; nskk-completion-prefix.el --- Prefix-based completion for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, prefix
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

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

;; このファイルはトライ木を活用した高速前方一致補完を実装します。
;;
;; 特徴:
;; - トライ木による O(k + n) の高速検索 (k=prefix長, n=結果数)
;; - インクリメンタル補完サポート
;; - 送り仮名タイプの柔軟な指定
;; - キャッシュ統合準備
;;
;; パフォーマンス目標:
;; - 検索応答: < 1ms
;; - 100件候補取得: < 5ms
;; - メモリ効率: 既存トライ木を活用
;;
;; 使用例:
;;
;;   (require 'nskk-completion-prefix)
;;
;;   ;; 基本的な前方一致補完
;;   (nskk-completion-prefix-search index "かん")
;;   ;; => (("かん" . entry1) ("かんじ" . entry2) ("かんたん" . entry3))
;;
;;   ;; 制限付き検索
;;   (nskk-completion-prefix-search index "か" :limit 10)
;;   ;; => 最大10件
;;
;;   ;; 送り仮名タイプ指定
;;   (nskk-completion-prefix-search index "かんが" :okuri-type 'okuri-ari)

;;; Code:

(require 'cl-lib)
(require 'nskk-trie)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-completion-prefix nil
  "Prefix-based completion for NSKK."
  :group 'nskk
  :prefix "nskk-completion-prefix-")

(defcustom nskk-completion-prefix-default-limit 50
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-prefix)

(defcustom nskk-completion-prefix-min-length 1
  "補完を開始する最小プレフィックス長。"
  :type 'integer
  :group 'nskk-completion-prefix)

(defcustom nskk-completion-prefix-sort-method 'frequency
  "補完結果のソート方法。
  - 'frequency: 使用頻度順
  - 'length: 文字列長の短い順
  - 'kana: 五十音順
  - 'none: ソートしない"
  :type '(choice (const :tag "Frequency order" frequency)
                 (const :tag "Length order" length)
                 (const :tag "Kana order" kana)
                 (const :tag "No sorting" none))
  :group 'nskk-completion-prefix)

;;; データ構造

(cl-defstruct (nskk-completion-prefix-result
               (:constructor nskk-completion-prefix-result--create)
               (:copier nil))
  "前方一致補完結果。

スロット:
  midashi    - 見出し語
  entry      - 辞書エントリ
  score      - スコア（ソート用）
  match-type - マッチタイプ（'exact/'prefix）"
  (midashi nil :type string)
  (entry nil :type nskk-dict-entry)
  (score 0 :type number)
  (match-type 'prefix :type symbol))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-prefix-search (index prefix &rest args)
  "前方一致補完検索を実行する。

引数:
  INDEX  - nskk-dict-index構造体
  PREFIX - 検索プレフィックス

キーワード引数:
  :limit      - 最大結果数（デフォルト: nskk-completion-prefix-default-limit）
  :okuri-type - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  :sort       - ソート方法（デフォルト: nskk-completion-prefix-sort-method）
  :exact      - 完全一致を含めるか（デフォルト: t）

戻り値:
  nskk-completion-prefix-result構造体のリスト

パフォーマンス: O(k + n) (k=prefix長, n=結果数)"
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))

  ;; プレフィックス長チェック
  (when (< (length prefix) nskk-completion-prefix-min-length)
    (return nil))

  (let* ((limit (or (plist-get args :limit) nskk-completion-prefix-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (sort-method (or (plist-get args :sort) nskk-completion-prefix-sort-method))
         (include-exact (if (plist-member args :exact)
                           (plist-get args :exact)
                         t))
         (results nil))

    ;; トライ木から前方一致検索
    (setq results (nskk-completion-prefix--search-tries
                   index prefix limit okuri-type include-exact))

    ;; ソート
    (setq results (nskk-completion-prefix--sort-results results sort-method))

    ;; limit適用（ソート後）
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

(defun nskk-completion-prefix--search-tries (index prefix limit okuri-type include-exact)
  "トライ木から前方一致検索を実行する（内部関数）。

引数:
  INDEX         - nskk-dict-index構造体
  PREFIX        - 検索プレフィックス
  LIMIT         - 最大結果数
  OKURI-TYPE    - 送り仮名タイプ
  INCLUDE-EXACT - 完全一致を含めるか

戻り値:
  nskk-completion-prefix-result構造体のリスト"
  (let ((results nil)
        (exact-match nil)
        (tries (cond
                ((eq okuri-type 'okuri-ari)
                 (list (cons (nskk-dict-index-trie-ari index) 'okuri-ari)))
                ((eq okuri-type 'okuri-nasi)
                 (list (cons (nskk-dict-index-trie-nasi index) 'okuri-nasi)))
                (t
                 ;; 両方から検索（送り仮名なしを優先）
                 (list (cons (nskk-dict-index-trie-nasi index) 'okuri-nasi)
                       (cons (nskk-dict-index-trie-ari index) 'okuri-ari))))))

    ;; 完全一致を先にチェック
    (when include-exact
      (cl-block nil
        (dolist (trie-pair tries)
          (let* ((trie (car trie-pair))
                 (lookup-result (nskk-trie-lookup trie prefix)))
            (when (cdr lookup-result)
              (setq exact-match
                    (nskk-completion-prefix-result--create
                     :midashi prefix
                     :entry (car lookup-result)
                     :score (nskk-dict-entry-frequency (car lookup-result))
                     :match-type 'exact))
              ;; 最初の完全一致のみ
              (cl-return))))))

    ;; 前方一致検索
    (dolist (trie-pair tries)
      (let ((trie (car trie-pair)))
        (when trie
          (let ((trie-results (nskk-trie-prefix-search trie prefix limit)))
            (dolist (result trie-results)
              (let ((midashi (car result))
                    (entry (cdr result)))
                ;; 完全一致は除外（既に追加済み）
                (unless (and include-exact (equal midashi prefix))
                  (push (nskk-completion-prefix-result--create
                         :midashi midashi
                         :entry entry
                         :score (nskk-dict-entry-frequency entry)
                         :match-type 'prefix)
                        results))))))))

    ;; 完全一致を先頭に追加
    (when exact-match
      (push exact-match results))

    ;; 重複除去（同じ見出し語が複数のトライ木に存在する場合）
    (nskk-completion-prefix--remove-duplicates results)))

(defun nskk-completion-prefix--remove-duplicates (results)
  "補完結果から重複を除去する（内部関数）。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

戻り値:
  重複除去後のリスト（最初に出現したものを保持）"
  (let ((seen (make-hash-table :test 'equal))
        (unique nil))
    (dolist (result results)
      (let ((midashi (nskk-completion-prefix-result-midashi result)))
        (unless (gethash midashi seen)
          (puthash midashi t seen)
          (push result unique))))
    (nreverse unique)))

;;; ソート機能

(defun nskk-completion-prefix--sort-results (results sort-method)
  "補完結果をソートする（内部関数）。

引数:
  RESULTS     - nskk-completion-prefix-result構造体のリスト
  SORT-METHOD - ソート方法

戻り値:
  ソート済みリスト"
  (pcase sort-method
    ('frequency
     (nskk-completion-prefix--sort-by-frequency results))
    ('length
     (nskk-completion-prefix--sort-by-length results))
    ('kana
     (nskk-completion-prefix--sort-by-kana results))
    ('none
     results)
    (_
     results)))

(defun nskk-completion-prefix--sort-by-frequency (results)
  "補完結果を使用頻度でソートする。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

処理:
  1. 完全一致を最優先
  2. 頻度の高い順
  3. 同頻度の場合は文字列長の短い順

戻り値:
  ソート済みリスト"
  (sort results
        (lambda (a b)
          (let ((a-exact (eq (nskk-completion-prefix-result-match-type a) 'exact))
                (b-exact (eq (nskk-completion-prefix-result-match-type b) 'exact))
                (a-score (nskk-completion-prefix-result-score a))
                (b-score (nskk-completion-prefix-result-score b)))
            (cond
             ;; 完全一致を優先
             ((and a-exact (not b-exact)) t)
             ((and (not a-exact) b-exact) nil)
             ;; 頻度で比較
             ((> a-score b-score) t)
             ((< a-score b-score) nil)
             ;; 同頻度の場合は文字列長の短い順
             (t (< (length (nskk-completion-prefix-result-midashi a))
                   (length (nskk-completion-prefix-result-midashi b)))))))))

(defun nskk-completion-prefix--sort-by-length (results)
  "補完結果を文字列長でソートする。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

戻り値:
  ソート済みリスト（短い順）"
  (sort results
        (lambda (a b)
          (let ((a-len (length (nskk-completion-prefix-result-midashi a)))
                (b-len (length (nskk-completion-prefix-result-midashi b))))
            (< a-len b-len)))))

(defun nskk-completion-prefix--sort-by-kana (results)
  "補完結果を五十音順でソートする。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

戻り値:
  ソート済みリスト"
  (sort results
        (lambda (a b)
          (string< (nskk-completion-prefix-result-midashi a)
                   (nskk-completion-prefix-result-midashi b)))))

;;; インクリメンタル補完

;;;###autoload
(defun nskk-completion-prefix-incremental (index prefix-list &rest args)
  "インクリメンタル前方一致補完を実行する。

引数:
  INDEX       - nskk-dict-index構造体
  PREFIX-LIST - プレフィックスのリスト（例: '(\"か\" \"かん\" \"かんじ\")）

キーワード引数:
  :limit      - 最大結果数
  :okuri-type - 送り仮名タイプ

戻り値:
  各プレフィックスに対する補完結果のalist
  ((prefix1 . results1) (prefix2 . results2) ...)

用途:
  ユーザーがタイプするたびに補完候補を更新する場合に使用"
  (unless (listp prefix-list)
    (error "Prefix-list must be a list: %s" prefix-list))

  (let ((results nil))
    (dolist (prefix prefix-list)
      (let ((completions (apply #'nskk-completion-prefix-search
                               index prefix args)))
        (push (cons prefix completions) results)))
    (nreverse results)))

;;; ユーティリティ関数

(defun nskk-completion-prefix-count (index prefix &rest args)
  "前方一致補完の候補数を取得する（検索を実行せずカウントのみ）。

引数:
  INDEX  - nskk-dict-index構造体
  PREFIX - 検索プレフィックス

キーワード引数:
  :okuri-type - 送り仮名タイプ

戻り値:
  候補数（整数）"
  (let* ((okuri-type (plist-get args :okuri-type))
         (results (apply #'nskk-completion-prefix-search
                        index prefix :limit nil args)))
    (length results)))

(defun nskk-completion-prefix-to-simple-list (results)
  "補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-prefix-result-midashi result)
                 (nskk-completion-prefix-result-entry result)))
          results))

(defun nskk-completion-prefix-to-string-list (results)
  "補完結果を文字列リストに変換する。

引数:
  RESULTS - nskk-completion-prefix-result構造体のリスト

戻り値:
  見出し語の文字列リスト"
  (mapcar #'nskk-completion-prefix-result-midashi results))

;;; デバッグ用関数

(defun nskk-completion-prefix-debug-info (index prefix)
  "補完検索のデバッグ情報を表示する。

引数:
  INDEX  - nskk-dict-index構造体
  PREFIX - 検索プレフィックス

戻り値:
  plist形式のデバッグ情報"
  (let* ((start-time (float-time))
         (results (nskk-completion-prefix-search index prefix))
         (elapsed-time (- (float-time) start-time)))
    (list :prefix prefix
          :result-count (length results)
          :elapsed-time elapsed-time
          :results (nskk-completion-prefix-to-string-list results))))

(provide 'nskk-completion-prefix)

;;; nskk-completion-prefix.el ends here
