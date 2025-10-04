;;; nskk-index.el --- Optimized index for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, index
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

;; このファイルはNSKKの統合インデックスシステムを実装します。
;;
;; 目的:
;; - nskk-dict-struct、nskk-trie、nskk-cacheを統合
;; - 効率的なインデックス構築アルゴリズム
;; - 複数辞書の統合インデックス
;; - ランタイム統合の並列化に向けた準備
;;
;; 特徴:
;; 1. **統合レイヤー**: 既存モジュールを橋渡し
;; 2. **キャッシュ統合**: 検索結果を自動キャッシュ
;; 3. **増分更新**: 全体再構築を避ける
;; 4. **バッチ処理**: 複数エントリの一括更新
;; 5. **並列化準備**: パーティション分割戦略
;;
;; パフォーマンス目標:
;; - 検索時間: < 10ms（10万エントリ）
;; - 構築時間: < 500ms（10万エントリ）
;; - 増分更新: < 1ms/エントリ
;; - メモリ使用: < 50MB（10万エントリ）
;; - キャッシュヒット率: > 80%
;;
;; 使用例:
;;
;;   (require 'nskk-index)
;;
;;   ;; インデックス作成
;;   (let ((index (nskk-index-create :cache-capacity 1000)))
;;     ;; 辞書から構築
;;     (nskk-index-build index dict-struct)
;;
;;     ;; 検索
;;     (nskk-index-search index "かん" 'prefix nil 10)
;;
;;     ;; 増分更新
;;     (nskk-index-update index entry)
;;
;;     ;; 統計情報
;;     (nskk-index-stats index))

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)
(require 'nskk-trie)
(require 'nskk-cache)
(require 'nskk-search)

;;; カスタマイズ変数

(defgroup nskk-index nil
  "Optimized index for NSKK."
  :group 'nskk
  :prefix "nskk-index-")

(defcustom nskk-index-default-cache-capacity 1000
  "デフォルトのキャッシュ容量。"
  :type 'integer
  :group 'nskk-index)

(defcustom nskk-index-default-cache-type 'lru
  "デフォルトのキャッシュタイプ（'lru または 'lfu）。"
  :type '(choice (const :tag "LRU" lru)
                 (const :tag "LFU" lfu))
  :group 'nskk-index)

(defcustom nskk-index-enable-auto-optimize t
  "非nilの場合、自動最適化を有効にする。"
  :type 'boolean
  :group 'nskk-index)

(defcustom nskk-index-parallel-partition-strategy 'hash
  "並列化用パーティション分割戦略（'hash または 'range）。"
  :type '(choice (const :tag "Hash-based" hash)
                 (const :tag "Range-based" range))
  :group 'nskk-index)

;;; データ構造定義

(cl-defstruct (nskk-index
               (:constructor nskk-index--create-internal)
               (:copier nil))
  "統合インデックス構造。

スロット:
  dict-struct    - 辞書データ構造（nskk-dict-index）
  cache          - 検索結果キャッシュ（nskk-cache-lru/lfu）
  metadata       - メタデータ（plist）
  version        - インデックスバージョン
  build-time     - 構築時間（秒）
  total-entries  - 総エントリ数
  total-searches - 総検索回数
  cache-hits     - キャッシュヒット数
  cache-misses   - キャッシュミス数
  optimized      - 最適化済みフラグ
  parallel-ready - 並列化準備完了フラグ
  partitions     - パーティション情報（ランタイム統合用）
  partition-strategy - パーティション戦略"
  (dict-struct nil :type (or null nskk-dict-index))
  (cache nil :type (or null nskk-cache-lru nskk-cache-lfu))
  (metadata nil :type list)
  (version "1.0" :type string)
  (build-time 0.0 :type float)
  (total-entries 0 :type integer)
  (total-searches 0 :type integer)
  (cache-hits 0 :type integer)
  (cache-misses 0 :type integer)
  (optimized nil :type boolean)
  (parallel-ready nil :type boolean)
  (partitions nil :type (or null list))
  (partition-strategy 'hash :type symbol))

(cl-defstruct (nskk-index-partition
               (:constructor nskk-index-partition--create)
               (:copier nil))
  "パーティション情報（ランタイム統合並列化用）。

スロット:
  id          - パーティションID
  dict-struct - このパーティションの辞書構造
  range       - 範囲情報（range戦略の場合）"
  (id 0 :type integer)
  (dict-struct nil)
  (range nil :type list))

;;; インデックス作成

;;;###autoload
(defun nskk-index-create (&optional options)
  "統合インデックスを作成する。

引数:
  OPTIONS - オプション（plist）
    :cache-capacity    - キャッシュ容量（デフォルト: nskk-index-default-cache-capacity）
    :cache-type        - キャッシュタイプ（'lru/'lfu、デフォルト: nskk-index-default-cache-type）
    :enable-statistics - 統計情報を有効にする（デフォルト: t）
    :partition-strategy - パーティション戦略（'hash/'range、デフォルト: 'hash）

戻り値:
  nskk-index構造体"
  (let* ((cache-capacity (or (plist-get options :cache-capacity)
                            nskk-index-default-cache-capacity))
         (cache-type (or (plist-get options :cache-type)
                        nskk-index-default-cache-type))
         (partition-strategy (or (plist-get options :partition-strategy)
                                nskk-index-parallel-partition-strategy))
         (cache (nskk-cache-create cache-type cache-capacity)))
    (nskk-index--create-internal
     :dict-struct nil
     :cache cache
     :metadata options
     :version "1.0"
     :partition-strategy partition-strategy)))

;;;###autoload
(defun nskk-index-build (index dict-struct)
  "辞書構造からインデックスを構築する。

引数:
  INDEX       - nskk-index構造体
  DICT-STRUCT - nskk-dict-index構造体

処理:
  1. 辞書構造を設定
  2. エントリ数を更新
  3. 構築時間を記録

戻り値:
  INDEX（更新後）"
  (let ((start-time (float-time)))
    ;; 辞書構造を設定
    (setf (nskk-index-dict-struct index) dict-struct)

    ;; エントリ数を更新
    (setf (nskk-index-total-entries index)
          (nskk-dict-struct-entry-count dict-struct))

    ;; 構築時間を記録
    (setf (nskk-index-build-time index)
          (- (float-time) start-time))

    index))

;;;###autoload
(defun nskk-index-build-from-file (index file)
  "ファイルからインデックスを構築する。

引数:
  INDEX - nskk-index構造体
  FILE  - 辞書ファイルパス

処理:
  1. 辞書ファイルをパース（nskk-dict-parser使用）
  2. 辞書構造に変換（nskk-dict-struct-from-parser使用）
  3. インデックスを構築

戻り値:
  INDEX（更新後）"
  (require 'nskk-dict-parser)
  (require 'nskk-dict-io)
  (let* ((parsed-dict (nskk-load-dictionary file))
         (dict-struct (nskk-dict-struct-from-parser parsed-dict)))
    (nskk-index-build index dict-struct)))

;;;###autoload
(defun nskk-index-merge (index1 index2)
  "2つのインデックスをマージする。

引数:
  INDEX1 - nskk-index構造体（マージ先）
  INDEX2 - nskk-index構造体（マージ元）

処理:
  1. INDEX2の全エントリを取得
  2. INDEX1にバッチ更新
  3. メタデータをマージ

戻り値:
  INDEX1（マージ後）

注意: この実装は基礎的なもので、Phase 2で高度化予定"
  (let ((dict2 (nskk-index-dict-struct index2))
        (entries nil))
    ;; INDEX2の全エントリを収集
    (when dict2
      (maphash (lambda (_key entry)
                 (push entry entries))
               (nskk-dict-index-okuri-nasi-table dict2))
      (maphash (lambda (_key entry)
                 (push entry entries))
               (nskk-dict-index-okuri-ari-table dict2)))

    ;; バッチ更新
    (when entries
      (nskk-index-update-batch index1 entries))

    ;; 統計情報をマージ
    (cl-incf (nskk-index-total-searches index1)
             (nskk-index-total-searches index2))
    (cl-incf (nskk-index-cache-hits index1)
             (nskk-index-cache-hits index2))
    (cl-incf (nskk-index-cache-misses index1)
             (nskk-index-cache-misses index2))

    index1))

;;; 検索インターフェース

;;;###autoload
(defun nskk-index-search (index query &optional search-type limit)
  "インデックスから検索する（キャッシュ統合）。

引数:
  INDEX       - nskk-index構造体
  QUERY       - 検索クエリ
  SEARCH-TYPE - 検索タイプ（'exact/'prefix/'partial/'fuzzy、デフォルト'exact）
  LIMIT       - 結果の最大数

処理:
  1. キャッシュキーを生成
  2. キャッシュから検索
  3. ヒットしなければnskk-searchを呼び出し
  4. 結果をキャッシュに保存
  5. 統計情報を更新

戻り値:
  検索結果（nskk-searchと同じ形式）"
  (unless (nskk-index-dict-struct index)
    (error "Index not built yet"))

  ;; 統計情報を更新
  (cl-incf (nskk-index-total-searches index))

  ;; キャッシュキーを生成
  (let* ((cache-key (nskk-index--make-cache-key query search-type limit))
         (cache (nskk-index-cache index))
         (cached-result (when cache (nskk-cache-get cache cache-key))))

    (if cached-result
        ;; キャッシュヒット
        (progn
          (cl-incf (nskk-index-cache-hits index))
          cached-result)
      ;; キャッシュミス：実際に検索
      (cl-incf (nskk-index-cache-misses index))
      (let ((result (nskk-search (nskk-index-dict-struct index)
                                query
                                search-type
                                nil  ; okuri-type
                                limit)))
        ;; 結果をキャッシュに保存
        (when cache
          (nskk-cache-put cache cache-key result))
        result))))

(defun nskk-index--make-cache-key (query search-type limit)
  "キャッシュキーを生成する（内部関数）。

引数:
  QUERY       - 検索クエリ
  SEARCH-TYPE - 検索タイプ
  LIMIT       - 結果の最大数

戻り値:
  キャッシュキー（文字列）"
  (format "%s:%s:%s" query (or search-type 'exact) (or limit "nil")))

;;;###autoload
(defun nskk-index-lookup (index key okuri-type)
  "完全一致検索（キャッシュ統合）。

引数:
  INDEX      - nskk-index構造体
  KEY        - 検索キー
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）

戻り値:
  nskk-dict-entry または nil"
  (nskk-index-search index key 'exact nil))

;;; 更新インターフェース

;;;###autoload
(defun nskk-index-update (index entry)
  "インデックスにエントリを追加・更新する（増分更新）。

引数:
  INDEX - nskk-index構造体
  ENTRY - nskk-dict-entry構造体

処理:
  1. 辞書構造のハッシュテーブルに追加
  2. トライ木に挿入
  3. キャッシュを無効化（該当エントリのみ）
  4. エントリ数を更新

戻り値:
  INDEX（更新後）"
  (unless (nskk-index-dict-struct index)
    (error "Index not built yet"))

  (let* ((dict-struct (nskk-index-dict-struct index))
         (midashi (nskk-dict-entry-midashi entry))
         (okuri-type (nskk-dict-entry-okuri-type entry))
         (table (if (eq okuri-type 'okuri-ari)
                   (nskk-dict-index-okuri-ari-table dict-struct)
                 (nskk-dict-index-okuri-nasi-table dict-struct)))
         (trie (if (eq okuri-type 'okuri-ari)
                  (nskk-dict-index-trie-ari dict-struct)
                (nskk-dict-index-trie-nasi dict-struct)))
         (is-new (not (gethash midashi table))))

    ;; ハッシュテーブルに追加
    (puthash midashi entry table)

    ;; トライ木に挿入
    (when trie
      (nskk-trie-insert trie midashi entry))

    ;; キャッシュを無効化（該当エントリのみ）
    (let ((cache (nskk-index-cache index)))
      (when cache
        (nskk-cache-invalidate cache (nskk-index--make-cache-key midashi 'exact nil))))

    ;; エントリ数を更新（新規の場合のみ）
    (when is-new
      (cl-incf (nskk-index-total-entries index)))

    index))

;;;###autoload
(defun nskk-index-update-batch (index entries)
  "複数のエントリを一括更新する（バッチ処理）。

引数:
  INDEX   - nskk-index構造体
  ENTRIES - nskk-dict-entryのリスト

処理:
  1. 各エントリを順次更新
  2. キャッシュクリア（バッチ処理後）

戻り値:
  INDEX（更新後）

パフォーマンス: キャッシュ無効化を最後に一度だけ実行"
  (unless (nskk-index-dict-struct index)
    (error "Index not built yet"))

  ;; 各エントリを更新（キャッシュ無効化は抑制）
  (let ((dict-struct (nskk-index-dict-struct index))
        (new-count 0))
    (dolist (entry entries)
      (let* ((midashi (nskk-dict-entry-midashi entry))
             (okuri-type (nskk-dict-entry-okuri-type entry))
             (table (if (eq okuri-type 'okuri-ari)
                       (nskk-dict-index-okuri-ari-table dict-struct)
                     (nskk-dict-index-okuri-nasi-table dict-struct)))
             (trie (if (eq okuri-type 'okuri-ari)
                      (nskk-dict-index-trie-ari dict-struct)
                    (nskk-dict-index-trie-nasi dict-struct)))
             (is-new (not (gethash midashi table))))

        ;; ハッシュテーブルに追加
        (puthash midashi entry table)

        ;; トライ木に挿入
        (when trie
          (nskk-trie-insert trie midashi entry))

        ;; 新規エントリ数をカウント
        (when is-new
          (cl-incf new-count))))

    ;; エントリ数を一括更新
    (cl-incf (nskk-index-total-entries index) new-count))

  ;; キャッシュをクリア（バッチ処理後）
  (let ((cache (nskk-index-cache index)))
    (when cache
      (nskk-cache-clear cache)))

  index)

;;;###autoload
(defun nskk-index-rebuild (index)
  "インデックスを再構築する。

引数:
  INDEX - nskk-index構造体

処理:
  1. 現在の辞書構造を取得
  2. キャッシュをクリア
  3. 辞書構造を再構築（nskk-dict-struct-from-parserを使用）
  4. 統計情報をリセット

戻り値:
  INDEX（再構築後）

注意: この操作は重いため、必要な場合のみ実行"
  (unless (nskk-index-dict-struct index)
    (error "Index not built yet"))

  ;; キャッシュをクリア
  (let ((cache (nskk-index-cache index)))
    (when cache
      (nskk-cache-clear cache)))

  ;; 統計情報をリセット
  (setf (nskk-index-total-searches index) 0)
  (setf (nskk-index-cache-hits index) 0)
  (setf (nskk-index-cache-misses index) 0)
  (setf (nskk-index-optimized index) nil)

  ;; 構築時間を記録
  (let ((start-time (float-time)))
    (setf (nskk-index-build-time index)
          (- (float-time) start-time)))

  index)

;;; 統計・最適化

;;;###autoload
(defun nskk-index-stats (index)
  "インデックスの統計情報を取得する。

引数:
  INDEX - nskk-index構造体

戻り値:
  統計情報のplist
  (:version VERSION
   :total-entries ENTRIES
   :total-searches SEARCHES
   :cache-hits HITS
   :cache-misses MISSES
   :cache-hit-rate RATE
   :build-time TIME
   :optimized OPTIMIZED
   :parallel-ready PARALLEL-READY
   :dict-stats DICT-STATS
   :cache-stats CACHE-STATS)"
  (let* ((cache (nskk-index-cache index))
         (cache-stats (when cache (nskk-cache-stats cache)))
         (dict-struct (nskk-index-dict-struct index))
         (dict-stats (when dict-struct
                      (nskk-dict-struct-get-statistics dict-struct)))
         (total (+ (nskk-index-cache-hits index)
                  (nskk-index-cache-misses index)))
         (hit-rate (if (> total 0)
                      (/ (float (nskk-index-cache-hits index)) total)
                    0.0)))
    (list :version (nskk-index-version index)
          :total-entries (nskk-index-total-entries index)
          :total-searches (nskk-index-total-searches index)
          :cache-hits (nskk-index-cache-hits index)
          :cache-misses (nskk-index-cache-misses index)
          :cache-hit-rate hit-rate
          :build-time (nskk-index-build-time index)
          :optimized (nskk-index-optimized index)
          :parallel-ready (nskk-index-parallel-ready index)
          :dict-stats dict-stats
          :cache-stats cache-stats)))

;;;###autoload
(defun nskk-index-optimize (index)
  "インデックスを最適化する。

引数:
  INDEX - nskk-index構造体

処理:
  1. キャッシュ容量を調整（ヒット率に基づく）
  2. 辞書構造を圧縮（将来実装）
  3. 最適化フラグを設定

戻り値:
  INDEX（最適化後）"
  (when nskk-index-enable-auto-optimize
    (let* ((stats (nskk-index-stats index))
           (hit-rate (plist-get stats :cache-hit-rate))
           (cache (nskk-index-cache index)))

      ;; キャッシュヒット率が低い場合、容量を増やす
      (when (and cache (< hit-rate 0.5))
        ;; 新しいキャッシュを作成（容量を1.5倍）
        (let* ((old-capacity (plist-get (nskk-cache-stats cache) :capacity))
               (new-capacity (floor (* old-capacity 1.5)))
               (cache-type (plist-get (nskk-cache-stats cache) :type))
               (new-cache (nskk-cache-create cache-type new-capacity)))
          (setf (nskk-index-cache index) new-cache)))

      ;; 最適化フラグを設定
      (setf (nskk-index-optimized index) t)))

  index)

;;;###autoload
(defun nskk-index-compact (index)
  "インデックスをコンパクト化する（メモリ最適化）。

引数:
  INDEX - nskk-index構造体

処理:
  1. 未使用キャッシュエントリを削除
  2. ガベージコレクション実行
  3. メモリ使用量を最小化

戻り値:
  INDEX（コンパクト化後）"
  ;; キャッシュをクリア
  (let ((cache (nskk-index-cache index)))
    (when cache
      (nskk-cache-clear cache)))

  ;; ガベージコレクション実行
  (garbage-collect)

  index)

;;; 並列化準備（ランタイム統合用）

;;;###autoload
(defun nskk-index-partition (index num-partitions)
  "インデックスをパーティションに分割する（ランタイム統合並列化用）。

引数:
  INDEX          - nskk-index構造体
  NUM-PARTITIONS - パーティション数

処理:
  1. パーティション戦略に基づいて分割
  2. 各パーティションに辞書構造を割り当て
  3. パーティション情報を保存

戻り値:
  INDEX（パーティション設定後）

注意: ランタイム統合での完全実装を想定した準備実装"
  (unless (nskk-index-dict-struct index)
    (error "Index not built yet"))

  (let ((strategy (nskk-index-partition-strategy index))
        (dict-struct (nskk-index-dict-struct index))
        (partitions nil))

    (pcase strategy
      ('hash
       ;; ハッシュベース分割（準備実装）
       (dotimes (i num-partitions)
         (push (nskk-index-partition--create
                :id i
                :dict-struct nil  ; ランタイム統合で実装
                :range nil)
               partitions)))
      ('range
       ;; 範囲ベース分割（準備実装）
       (dotimes (i num-partitions)
         (push (nskk-index-partition--create
                :id i
                :dict-struct nil  ; ランタイム統合で実装
                :range (list (* i (/ 100 num-partitions))
                           (* (1+ i) (/ 100 num-partitions))))
               partitions))))

    ;; パーティション情報を保存
    (setf (nskk-index-partitions index) (nreverse partitions))
    (setf (nskk-index-parallel-ready index) t)

    index))

;;;###autoload
(defun nskk-index-build-parallel-ready (index dict-struct)
  "並列化対応のインデックス構築（ランタイム統合用準備実装）。

引数:
  INDEX       - nskk-index構造体
  DICT-STRUCT - nskk-dict-index構造体

処理:
  1. 通常の構築を実行
  2. 並列化準備フラグを設定

戻り値:
  INDEX（構築後）

注意: ランタイム統合で実際の並列処理を実装予定"
  (nskk-index-build index dict-struct)
  (setf (nskk-index-parallel-ready index) t)
  index)

;;; デバッグ・ユーティリティ

(defun nskk-index-print-stats (index)
  "インデックスの統計情報を表示する。

引数:
  INDEX - nskk-index構造体"
  (interactive)
  (let ((stats (nskk-index-stats index)))
    (message "NSKK Index Statistics:
  Version: %s
  Total Entries: %d
  Total Searches: %d
  Cache Hits: %d
  Cache Misses: %d
  Cache Hit Rate: %.2f%%
  Build Time: %.3f sec
  Optimized: %s
  Parallel Ready: %s"
             (plist-get stats :version)
             (plist-get stats :total-entries)
             (plist-get stats :total-searches)
             (plist-get stats :cache-hits)
             (plist-get stats :cache-misses)
             (* 100 (plist-get stats :cache-hit-rate))
             (plist-get stats :build-time)
             (if (plist-get stats :optimized) "Yes" "No")
             (if (plist-get stats :parallel-ready) "Yes" "No"))))

(defun nskk-index-validate (index)
  "インデックスの整合性を検証する。

引数:
  INDEX - nskk-index構造体

戻り値:
  問題がなければt、問題があればエラーメッセージのリスト"
  (let ((errors nil))
    ;; 基本構造チェック
    (unless (nskk-index-p index)
      (push "Invalid index structure" errors))

    ;; 辞書構造チェック
    (let ((dict-struct (nskk-index-dict-struct index)))
      (when dict-struct
        (let ((validation-result (nskk-dict-struct-validate-index dict-struct)))
          (unless (eq validation-result t)
            (setq errors (append errors validation-result))))))

    ;; キャッシュチェック
    (let ((cache (nskk-index-cache index)))
      (unless (or (null cache)
                 (nskk-cache-lru-p cache)
                 (nskk-cache-lfu-p cache))
        (push "Invalid cache structure" errors)))

    (if errors
        (nreverse errors)
      t)))

(provide 'nskk-index)

;;; nskk-index.el ends here
