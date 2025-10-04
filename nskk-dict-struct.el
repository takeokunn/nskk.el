;;; nskk-dict-struct.el --- Dictionary data structures for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはSKK辞書の効率的な検索・管理のためのデータ構造を実装します。
;;
;; 特徴:
;; - cl-defstructによる型安全なデータ構造
;; - ハッシュテーブルによる O(1) 平均検索
;; - 前方一致検索のための最適化インデックス
;; - メモリ効率を考慮した設計
;; - Task 1.17 (トライ木) への移行を考慮した拡張性
;;
;; パフォーマンス目標:
;; - エントリ検索: O(1) 平均（ハッシュテーブル）
;; - 前方一致検索: O(k + m) (k=prefix長, m=結果数)
;; - メモリ使用: 10万エントリで < 50MB
;; - 構造変換: 10万エントリで < 500ms
;;
;; 使用例:
;;
;;   (require 'nskk-dict-struct)
;;   (require 'nskk-dict-parser)
;;
;;   ;; パーサー出力を最適化された構造に変換
;;   (let* ((parsed (nskk-parse-dictionary "~/.skk/jisyo"))
;;          (index (nskk-dict-struct-from-parser parsed)))
;;     ;; 基本検索
;;     (nskk-dict-struct-lookup index "かんじ" 'okuri-nasi)
;;     ;; => nskk-dict-entry構造体
;;
;;     ;; 前方一致検索
;;     (nskk-dict-struct-prefix-search index "かん" 10)
;;     ;; => エントリのリスト
;;     )

;;; Code:

(require 'cl-lib)
(require 'nskk-trie)

;;; カスタマイズ変数

(defgroup nskk-dict-struct nil
  "Dictionary data structure customization."
  :group 'nskk
  :prefix "nskk-dict-struct-")

(defcustom nskk-dict-struct-prefix-index-depth 2
  "前方一致インデックスの文字深度。
値が大きいほど検索は高速だがメモリ使用量が増加する。"
  :type 'integer
  :group 'nskk-dict-struct)

(defcustom nskk-dict-struct-enable-statistics t
  "非nilの場合、辞書統計情報を収集する。"
  :type 'boolean
  :group 'nskk-dict-struct)

;;; データ構造定義

;; 候補構造（メモリ効率重視）
(cl-defstruct (nskk-dict-candidate
               (:constructor nskk-dict-candidate--create)
               (:copier nil))
  "辞書候補の最適化された構造。

スロット:
  word       - 候補語（string）
  annotation - 注釈（string or nil）
  score      - スコア（検索ランキング用、初期値0）"
  (word nil :type string)
  (annotation nil :type (or null string))
  (score 0 :type number))

;; 辞書エントリ構造
(cl-defstruct (nskk-dict-entry
               (:constructor (nskk-dict-entry--create
                               (&key midashi candidates frequency last-used okuri-type metadata)))
               (:copier nil))
  "SKK辞書エントリの最適化された構造。

スロット:
  midashi         - 見出し語（string）
  candidates      - 候補リスト（nskk-dict-candidate のリスト）
  frequency       - 使用頻度（学習用、初期値0）
  last-used       - 最終使用時刻（float-time形式、nilの場合未使用）
  okuri-type      - 送り仮名タイプ（'okuri-ari/'okuri-nasi）
  metadata        - その他メタデータ（plist）"
  (midashi nil :type string)
  (candidates nil :type list)
  (frequency 0 :type integer)
  (last-used nil :type (or null number))
  (okuri-type nil :type (or null symbol))
  (metadata nil :type list))

;; 前方一致インデックス
(cl-defstruct (nskk-dict-prefix-index
               (:constructor nskk-dict-prefix-index--create)
               (:copier nil))
  "前方一致検索用インデックス。

スロット:
  char-map - 文字プレフィックス→エントリリストのハッシュテーブル
  depth    - インデックス深度（文字数）"
  (char-map nil :type hash-table)
  (depth 2 :type integer))

;; 辞書統計情報
(cl-defstruct (nskk-dict-statistics
               (:constructor nskk-dict-statistics--create)
               (:copier nil))
  "辞書統計情報。

スロット:
  okuri-ari-count  - 送り仮名ありエントリ数
  okuri-nasi-count - 送り仮名なしエントリ数
  total-candidates - 総候補数
  avg-candidates   - エントリあたり平均候補数
  build-time       - インデックス構築時間（秒）
  memory-usage     - 推定メモリ使用量（バイト）"
  (okuri-ari-count 0 :type integer)
  (okuri-nasi-count 0 :type integer)
  (total-candidates 0 :type integer)
  (avg-candidates 0.0 :type float)
  (build-time 0.0 :type float)
  (memory-usage 0 :type integer))

;; 辞書メタデータ
(cl-defstruct (nskk-dict-metadata
               (:constructor nskk-dict-metadata--create)
               (:copier nil))
  "辞書メタデータ。

スロット:
  file-path      - 辞書ファイルパス
  encoding       - エンコーディング
  version        - 辞書バージョン（文字列）
  entry-count    - エントリ数
  last-modified  - 最終更新時刻（float-time形式）
  checksum       - チェックサム（整合性確認用、未実装時はnil）
  options        - 辞書オプション（plist）"
  (file-path nil :type (or null string))
  (encoding nil :type (or null symbol))
  (version nil :type (or null string))
  (entry-count 0 :type integer)
  (last-modified nil :type (or null number))
  (checksum nil :type (or null string))
  (options nil :type list))

;; 辞書インデックス構造
(cl-defstruct (nskk-dict-index
               (:constructor nskk-dict-index--create)
               (:copier nil))
  "辞書インデックス構造。

スロット:
  okuri-ari-table  - 送り仮名ありエントリのハッシュテーブル（midashi→entry）
  okuri-nasi-table - 送り仮名なしエントリのハッシュテーブル（midashi→entry）
  trie-ari         - 送り仮名あり前方一致検索用トライ木（Task 1.17統合）
  trie-nasi        - 送り仮名なし前方一致検索用トライ木（Task 1.17統合）
  prefix-index-ari  - 送り仮名あり前方一致インデックス（後方互換性のため保持）
  prefix-index-nasi - 送り仮名なし前方一致インデックス（後方互換性のため保持）
  metadata         - 辞書メタデータ
  statistics       - 統計情報"
  (okuri-ari-table nil :type hash-table)
  (okuri-nasi-table nil :type hash-table)
  (trie-ari nil :type (or null nskk-trie))
  (trie-nasi nil :type (or null nskk-trie))
  (prefix-index-ari nil :type (or null nskk-dict-prefix-index))
  (prefix-index-nasi nil :type (or null nskk-dict-prefix-index))
  (metadata nil :type (or null nskk-dict-metadata))
  (statistics nil :type (or null nskk-dict-statistics)))

;;; 候補生成関数

(defun nskk-dict-candidate-create (word &optional annotation score)
  "候補を生成する。

引数:
  WORD       - 候補語
  ANNOTATION - 注釈（オプション）
  SCORE      - スコア（オプション、デフォルト0）

戻り値:
  nskk-dict-candidate構造体"
  (nskk-dict-candidate--create
   :word word
   :annotation annotation
   :score (or score 0)))

;;; エントリ生成関数

(defun nskk-dict-entry-create (midashi candidates &optional okuri-type)
  "辞書エントリを生成する。

引数:
  MIDASHI    - 見出し語
  CANDIDATES - 候補リスト（nskk-dict-candidateのリストまたは文字列リスト）
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari または 'okuri-nasi、省略時は自動判定）

戻り値:
  nskk-dict-entry構造体"
  (let* ((okuri (or okuri-type
                   (nskk-dict-struct--infer-okuri-type midashi)))
         (cand-list (if (and candidates
                            (nskk-dict-candidate-p (car candidates)))
                       candidates
                     (mapcar (lambda (c)
                              (if (consp c)
                                  (nskk-dict-candidate-create (car c) (cdr c))
                                (nskk-dict-candidate-create c)))
                            candidates))))
    (nskk-dict-entry--create
     :midashi midashi
     :candidates cand-list
     :okuri-type okuri)))

(defun nskk-dict-struct--infer-okuri-type (midashi)
  "見出し語から送り仮名タイプを推測する。

引数:
  MIDASHI - 見出し語

戻り値:
  'okuri-ari または 'okuri-nasi"
  (if (and (> (length midashi) 0)
           (let ((last-char (aref midashi (1- (length midashi)))))
             (and (>= last-char ?a) (<= last-char ?z))))
      'okuri-ari
    'okuri-nasi))

;;; パーサー出力変換関数

;;;###autoload
(defun nskk-dict-struct-from-parser (parsed-dict)
  "パーサー出力（nskk-dict）を最適化されたインデックス構造に変換する。

引数:
  PARSED-DICT - nskk-parse-dictionary の戻り値（nskk-dict構造体）

戻り値:
  nskk-dict-index構造体

パフォーマンス目標:
  10万エントリで < 500ms

変更点（Task 1.18）:
  - トライ木を構築して高速前方一致検索をサポート
  - 既存のprefix-indexも後方互換性のため保持"
  (let* ((start-time (float-time))
         (okuri-ari-table (make-hash-table :test 'equal :size 10000))
         (okuri-nasi-table (make-hash-table :test 'equal :size 100000))
         ;; トライ木の作成（Task 1.18）
         (trie-ari (nskk-trie-create))
         (trie-nasi (nskk-trie-create))
         ;; 既存のprefix-indexも保持（後方互換性）
         (prefix-ari (nskk-dict-prefix-index--create
                     :char-map (make-hash-table :test 'equal :size 1000)
                     :depth nskk-dict-struct-prefix-index-depth))
         (prefix-nasi (nskk-dict-prefix-index--create
                      :char-map (make-hash-table :test 'equal :size 10000)
                      :depth nskk-dict-struct-prefix-index-depth))
         (stats (when nskk-dict-struct-enable-statistics
                 (nskk-dict-statistics--create)))
         (total-candidates 0))

    ;; 送り仮名ありエントリを変換
    ;; トライ木とプレフィックスインデックスを同時に構築
    (let ((ari-entries nil))
      (dolist (parsed-entry (nskk-dict-okuri-ari parsed-dict))
        (let* ((midashi (nskk-dict-entry-midashi parsed-entry))
               (candidates (nskk-dict-struct--convert-candidates
                           (nskk-dict-entry-candidates parsed-entry)))
               (entry (nskk-dict-entry--create
                      :midashi midashi
                      :candidates candidates
                      :okuri-type 'okuri-ari)))
          (puthash midashi entry okuri-ari-table)
          (push (cons midashi entry) ari-entries)
          ;; トライ木に挿入（Task 1.18）
          (nskk-trie-insert trie-ari midashi entry)
          (when stats
            (setq total-candidates (+ total-candidates (length candidates))))))
      ;; プレフィックスインデックス一括構築（後方互換性）
      (dolist (entry-pair ari-entries)
        (nskk-dict-struct--add-to-prefix-index prefix-ari (car entry-pair) (cdr entry-pair))))

    ;; 送り仮名なしエントリを変換
    (let ((nasi-entries nil))
      (dolist (parsed-entry (nskk-dict-okuri-nasi parsed-dict))
        (let* ((midashi (nskk-dict-entry-midashi parsed-entry))
               (candidates (nskk-dict-struct--convert-candidates
                           (nskk-dict-entry-candidates parsed-entry)))
               (entry (nskk-dict-entry--create
                      :midashi midashi
                      :candidates candidates
                      :okuri-type 'okuri-nasi)))
          (puthash midashi entry okuri-nasi-table)
          (push (cons midashi entry) nasi-entries)
          ;; トライ木に挿入（Task 1.18）
          (nskk-trie-insert trie-nasi midashi entry)
          (when stats
            (setq total-candidates (+ total-candidates (length candidates))))))
      ;; プレフィックスインデックス一括構築（後方互換性）
      (dolist (entry-pair nasi-entries)
        (nskk-dict-struct--add-to-prefix-index prefix-nasi (car entry-pair) (cdr entry-pair))))

    ;; メタデータ構築
    (let* ((metadata (nskk-dict-metadata--create
                     :file-path (nskk-dict-file-path parsed-dict)
                     :encoding (nskk-dict-encoding parsed-dict)
                     :entry-count (+ (hash-table-count okuri-ari-table)
                                    (hash-table-count okuri-nasi-table))
                     :last-modified (float-time)))
           (build-time (- (float-time) start-time)))

      ;; 統計情報を更新
      (when stats
        (setf (nskk-dict-statistics-okuri-ari-count stats)
              (hash-table-count okuri-ari-table))
        (setf (nskk-dict-statistics-okuri-nasi-count stats)
              (hash-table-count okuri-nasi-table))
        (setf (nskk-dict-statistics-total-candidates stats)
              total-candidates)
        (setf (nskk-dict-statistics-avg-candidates stats)
              (if (> (nskk-dict-metadata-entry-count metadata) 0)
                  (/ (float total-candidates)
                     (nskk-dict-metadata-entry-count metadata))
                0.0))
        (setf (nskk-dict-statistics-build-time stats) build-time)
        (setf (nskk-dict-statistics-memory-usage stats)
              (nskk-dict-struct--estimate-memory-usage
               okuri-ari-table okuri-nasi-table)))

      ;; インデックス構造を返す（トライ木を追加）
      (nskk-dict-index--create
       :okuri-ari-table okuri-ari-table
       :okuri-nasi-table okuri-nasi-table
       :trie-ari trie-ari
       :trie-nasi trie-nasi
       :prefix-index-ari prefix-ari
       :prefix-index-nasi prefix-nasi
       :metadata metadata
       :statistics stats))))

(defun nskk-dict-struct--convert-candidates (parsed-candidates)
  "パーサー出力の候補リストを新しい候補構造に変換する。

引数:
  PARSED-CANDIDATES - ((word . annotation) ...) 形式のリスト

戻り値:
  nskk-dict-candidate構造体のリスト"
  (mapcar (lambda (cand)
           (nskk-dict-candidate--create
            :word (car cand)
            :annotation (cdr cand)
            :score 0))
          parsed-candidates))

(defun nskk-dict-struct--add-to-prefix-index (prefix-index midashi entry)
  "前方一致インデックスにエントリを追加する。

引数:
  PREFIX-INDEX - nskk-dict-prefix-index構造体
  MIDASHI      - 見出し語
  ENTRY        - nskk-dict-entry構造体

深度分のすべてのプレフィックスに対してエントリを登録する。
例: 深度2で「かんじ」なら、「か」と「かん」の両方に登録。"
  (let* ((depth (nskk-dict-prefix-index-depth prefix-index))
         (char-map (nskk-dict-prefix-index-char-map prefix-index))
         (midashi-len (length midashi)))
    ;; 1文字から深度までのすべてのプレフィックスに登録
    (dotimes (i (min depth midashi-len))
      (let* ((prefix-len (1+ i))
             (prefix (substring midashi 0 prefix-len))
             (entries (gethash prefix char-map)))
        (puthash prefix (cons entry entries) char-map)))))

(defun nskk-dict-struct--estimate-memory-usage (okuri-ari-table okuri-nasi-table)
  "辞書インデックスの推定メモリ使用量を計算する（バイト）。

引数:
  OKURI-ARI-TABLE  - 送り仮名ありハッシュテーブル
  OKURI-NASI-TABLE - 送り仮名なしハッシュテーブル

戻り値:
  推定メモリ使用量（バイト）

注: これは概算値です。実際のメモリ使用量はEmacsの内部実装に依存します。"
  (let ((total 0))
    ;; ハッシュテーブルオーバーヘッド（概算）
    (setq total (+ total
                  (* (hash-table-count okuri-ari-table) 32)
                  (* (hash-table-count okuri-nasi-table) 32)))

    ;; エントリとキーのサイズ
    (maphash (lambda (key value)
              (setq total (+ total
                            (* (length key) 4)  ; 文字列キー（UTF-8想定）
                            200)))              ; エントリ構造体のオーバーヘッド
             okuri-ari-table)
    (maphash (lambda (key value)
              (setq total (+ total
                            (* (length key) 4)
                            200)))
             okuri-nasi-table)

    total))

;;; 検索関数

;;;###autoload
(defun nskk-dict-struct-lookup (index midashi &optional okuri-type)
  "インデックスから見出し語を検索する。

引数:
  INDEX      - nskk-dict-index構造体
  MIDASHI    - 見出し語
  OKURI-TYPE - 'okuri-ari または 'okuri-nasi（省略時は両方検索）

戻り値:
  nskk-dict-entry構造体、見つからない場合はnil

パフォーマンス:
  O(1) 平均（ハッシュテーブル検索）"
  (cond
   ((eq okuri-type 'okuri-ari)
    (gethash midashi (nskk-dict-index-okuri-ari-table index)))
   ((eq okuri-type 'okuri-nasi)
    (gethash midashi (nskk-dict-index-okuri-nasi-table index)))
   (t
    ;; okuri-typeが指定されていない場合、送り仮名なしを優先して検索
    (or (gethash midashi (nskk-dict-index-okuri-nasi-table index))
        (gethash midashi (nskk-dict-index-okuri-ari-table index))))))

;;;###autoload
(defun nskk-dict-struct-prefix-search (index prefix &optional limit okuri-type)
  "前方一致検索を実行する。

引数:
  INDEX      - nskk-dict-index構造体
  PREFIX     - 検索プレフィックス（見出し語の先頭部分）
  LIMIT      - 最大結果数（省略時は無制限）
  OKURI-TYPE - 'okuri-ari または 'okuri-nasi（省略時は両方検索）

戻り値:
  nskk-dict-entry構造体のリスト

パフォーマンス:
  O(k + m) (k=prefix長, m=結果数)"
  (let ((results nil)
        (count 0)
        (tables (cond
                ((eq okuri-type 'okuri-ari)
                 (list (cons (nskk-dict-index-okuri-ari-table index)
                           (nskk-dict-index-prefix-index-ari index))))
                ((eq okuri-type 'okuri-nasi)
                 (list (cons (nskk-dict-index-okuri-nasi-table index)
                           (nskk-dict-index-prefix-index-nasi index))))
                (t
                 (list (cons (nskk-dict-index-okuri-nasi-table index)
                           (nskk-dict-index-prefix-index-nasi index))
                      (cons (nskk-dict-index-okuri-ari-table index)
                           (nskk-dict-index-prefix-index-ari index)))))))

    (catch 'done
      (dolist (table-pair tables)
        (let* ((table (car table-pair))
               (prefix-idx (cdr table-pair))
               (depth (nskk-dict-prefix-index-depth prefix-idx))
               (prefix-key (substring prefix 0 (min depth (length prefix)))))

          ;; 前方一致インデックスを使用して候補を絞り込む
          (let ((candidate-entries (gethash prefix-key
                                           (nskk-dict-prefix-index-char-map prefix-idx))))
            (dolist (entry candidate-entries)
              (when (and entry
                        (string-prefix-p prefix (nskk-dict-entry-midashi entry)))
                (push entry results)
                (setq count (1+ count))
                (when (and limit (>= count limit))
                  (throw 'done nil))))))))

    (nreverse results)))

;;; ユーティリティ関数

(defun nskk-dict-struct-get-statistics (index)
  "辞書インデックスの統計情報を取得する。

引数:
  INDEX - nskk-dict-index構造体

戻り値:
  nskk-dict-statistics構造体、統計が無効な場合はnil"
  (nskk-dict-index-statistics index))

(defun nskk-dict-struct-get-metadata (index)
  "辞書インデックスのメタデータを取得する。

引数:
  INDEX - nskk-dict-index構造体

戻り値:
  nskk-dict-metadata構造体"
  (nskk-dict-index-metadata index))

(defun nskk-dict-struct-entry-count (index &optional okuri-type)
  "辞書インデックスのエントリ数を取得する。

引数:
  INDEX      - nskk-dict-index構造体
  OKURI-TYPE - 'okuri-ari または 'okuri-nasi（省略時は合計）

戻り値:
  エントリ数（整数）"
  (cond
   ((eq okuri-type 'okuri-ari)
    (hash-table-count (nskk-dict-index-okuri-ari-table index)))
   ((eq okuri-type 'okuri-nasi)
    (hash-table-count (nskk-dict-index-okuri-nasi-table index)))
   (t
    (+ (hash-table-count (nskk-dict-index-okuri-ari-table index))
       (hash-table-count (nskk-dict-index-okuri-nasi-table index))))))

(defun nskk-dict-struct-print-statistics (index)
  "辞書インデックスの統計情報を表示する。

引数:
  INDEX - nskk-dict-index構造体"
  (interactive)
  (let ((stats (nskk-dict-struct-get-statistics index))
        (metadata (nskk-dict-struct-get-metadata index)))
    (if (and stats metadata)
        (message "Dictionary Index Statistics:
  File: %s
  Encoding: %s
  Okuri-ari entries: %d
  Okuri-nasi entries: %d
  Total candidates: %d
  Avg candidates/entry: %.2f
  Build time: %.3f sec
  Estimated memory: %.2f MB"
                 (nskk-dict-metadata-file-path metadata)
                 (nskk-dict-metadata-encoding metadata)
                 (nskk-dict-statistics-okuri-ari-count stats)
                 (nskk-dict-statistics-okuri-nasi-count stats)
                 (nskk-dict-statistics-total-candidates stats)
                 (nskk-dict-statistics-avg-candidates stats)
                 (nskk-dict-statistics-build-time stats)
                 (/ (nskk-dict-statistics-memory-usage stats) 1048576.0))
      (message "Statistics not available for this index"))))

;;; デバッグ用関数

(defun nskk-dict-struct-validate-index (index)
  "辞書インデックスの整合性を検証する。

引数:
  INDEX - nskk-dict-index構造体

戻り値:
  問題がなければt、問題があればエラーメッセージのリスト"
  (let ((errors nil))
    ;; 基本構造チェック
    (unless (nskk-dict-index-p index)
      (push "Invalid index structure" errors))

    (unless (hash-table-p (nskk-dict-index-okuri-ari-table index))
      (push "Invalid okuri-ari table" errors))

    (unless (hash-table-p (nskk-dict-index-okuri-nasi-table index))
      (push "Invalid okuri-nasi table" errors))

    ;; エントリ整合性チェック
    (maphash (lambda (key entry)
              (unless (equal key (nskk-dict-entry-midashi entry))
                (push (format "Mismatched key-midashi: %s != %s"
                            key (nskk-dict-entry-midashi entry))
                     errors)))
            (nskk-dict-index-okuri-ari-table index))

    (maphash (lambda (key entry)
              (unless (equal key (nskk-dict-entry-midashi entry))
                (push (format "Mismatched key-midashi: %s != %s"
                            key (nskk-dict-entry-midashi entry))
                     errors)))
            (nskk-dict-index-okuri-nasi-table index))

    (if errors
        (nreverse errors)
      t)))

(provide 'nskk-dict-struct)

;;; nskk-dict-struct.el ends here
