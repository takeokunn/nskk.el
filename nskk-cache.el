;;; nskk-cache.el --- Cache mechanism for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; このファイルはNSKKのキャッシュ機構を実装します。
;;
;; サポートするキャッシュアルゴリズム:
;; - LRU (Least Recently Used): 最近使用されていないものを削除
;; - LFU (Least Frequently Used): 使用頻度が低いものを削除
;;
;; 特徴:
;; - O(1) get/put操作（ハッシュテーブル + 双方向リンクリスト）
;; - 容量管理（エントリ数ベース）
;; - 自動エビクション
;; - キャッシュヒット率測定
;; - パターンマッチ無効化
;;
;; パフォーマンス目標:
;; - get操作: O(1), < 0.1ms
;; - put操作: O(1), < 0.1ms
;; - キャッシュヒット時の検索: < 10ms
;;
;; 使用例:
;;
;;   (require 'nskk-cache)
;;
;;   ;; LRUキャッシュ作成
;;   (setq cache (nskk-cache-create 'lru 1000))
;;
;;   ;; データ追加
;;   (nskk-cache-put cache "key1" "value1")
;;
;;   ;; データ取得
;;   (nskk-cache-get cache "key1")
;;   ;; => "value1"
;;
;;   ;; 統計情報
;;   (nskk-cache-stats cache)
;;   ;; => (:hits 10 :misses 5 :hit-rate 0.666 ...)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-cache nil
  "Cache mechanism customization."
  :group 'nskk
  :prefix "nskk-cache-")

(defcustom nskk-cache-default-capacity 1000
  "Default cache capacity in number of entries."
  :type 'integer
  :group 'nskk-cache)

(defcustom nskk-cache-default-type 'lru
  "Default cache type, either `lru' or `lfu'."
  :type '(choice (const :tag "LRU (Least Recently Used)" lru)
                 (const :tag "LFU (Least Frequently Used)" lfu))
  :group 'nskk-cache)

;;; LRUキャッシュ用データ構造

;; LRUキャッシュ用ノード（双方向リンクリスト）
(cl-defstruct (nskk-cache-lru-node
               (:constructor nskk-cache-lru-node--create)
               (:copier nil))
  "LRUキャッシュ用ノード。

スロット:
  key   - キー
  value - 値
  prev  - 前のノード（nskk-cache-lru-node）
  next  - 次のノード（nskk-cache-lru-node）"
  key
  value
  prev
  next)

;; LRUキャッシュ構造
(cl-defstruct (nskk-cache-lru
               (:constructor nskk-cache-lru--create)
               (:copier nil))
  "LRUキャッシュ構造。

スロット:
  capacity - 最大容量（エントリ数）
  size     - 現在のエントリ数
  hash     - キー→ノードのハッシュテーブル
  head     - ダミーヘッドノード（最新側）
  tail     - ダミーテールノード（最古側）
  hits     - キャッシュヒット数
  misses   - キャッシュミス数"
  (capacity 1000 :type integer)
  (size 0 :type integer)
  (hash nil :type hash-table)
  (head nil)
  (tail nil)
  (hits 0 :type integer)
  (misses 0 :type integer))

;;; LFUキャッシュ用データ構造

;; LFUキャッシュ用エントリ
(cl-defstruct (nskk-cache-lfu-entry
               (:constructor nskk-cache-lfu-entry--create)
               (:copier nil))
  "LFUキャッシュ用エントリ。

スロット:
  key       - キー
  value     - 値
  frequency - 使用頻度"
  key
  value
  (frequency 1 :type integer))

;; LFUキャッシュ構造
(cl-defstruct (nskk-cache-lfu
               (:constructor nskk-cache-lfu--create)
               (:copier nil))
  "LFUキャッシュ構造。

スロット:
  capacity  - 最大容量（エントリ数）
  size      - 現在のエントリ数
  hash      - キー→エントリのハッシュテーブル
  freq      - 頻度→キーのリストのハッシュテーブル
  min-freq  - 現在の最小頻度
  hits      - キャッシュヒット数
  misses    - キャッシュミス数"
  (capacity 1000 :type integer)
  (size 0 :type integer)
  (hash nil :type hash-table)
  (freq nil :type hash-table)
  (min-freq 0 :type integer)
  (hits 0 :type integer)
  (misses 0 :type integer))

;;; LRUキャッシュ実装

;;;###autoload
(defun nskk-cache-lru-create (capacity)
  "Create an LRU cache with CAPACITY entries."
  (let* ((head (nskk-cache-lru-node--create))
         (tail (nskk-cache-lru-node--create)))
    ;; ダミーヘッドとテールを接続
    (setf (nskk-cache-lru-node-next head) tail)
    (setf (nskk-cache-lru-node-prev tail) head)
    (nskk-cache-lru--create
     :capacity capacity
     :size 0
     :hash (make-hash-table :test 'equal :size capacity)
     :head head
     :tail tail
     :hits 0
     :misses 0)))

(defsubst nskk-cache-lru--remove-node (node)
  "Remove NODE from the doubly-linked list."
  (let ((prev-node (nskk-cache-lru-node-prev node))
        (next-node (nskk-cache-lru-node-next node)))
    (setf (nskk-cache-lru-node-next prev-node) next-node)
    (setf (nskk-cache-lru-node-prev next-node) prev-node)))

(defsubst nskk-cache-lru--add-to-head (cache node)
  "Add NODE right after the head of CACHE."
  (let ((head (nskk-cache-lru-head cache))
        (next-node (nskk-cache-lru-node-next (nskk-cache-lru-head cache))))
    (setf (nskk-cache-lru-node-next node) next-node)
    (setf (nskk-cache-lru-node-prev node) head)
    (setf (nskk-cache-lru-node-next head) node)
    (setf (nskk-cache-lru-node-prev next-node) node)))

(defsubst nskk-cache-lru--move-to-head (cache node)
  "Move NODE to the head position in CACHE."
  (nskk-cache-lru--remove-node node)
  (nskk-cache-lru--add-to-head cache node))

(defsubst nskk-cache-lru--remove-tail (cache)
  "Remove and return the tail node from CACHE."
  (let* ((tail (nskk-cache-lru-tail cache))
         (node (nskk-cache-lru-node-prev tail)))
    (nskk-cache-lru--remove-node node)
    node))

;;;###autoload
(defun nskk-cache-lru-get (cache key)
  "Get value for KEY from LRU CACHE."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (if node
        (progn
          ;; キャッシュヒット：ノードをヘッドに移動
          (nskk-cache-lru--move-to-head cache node)
          (cl-incf (nskk-cache-lru-hits cache))
          (nskk-cache-lru-node-value node))
      ;; キャッシュミス
      (cl-incf (nskk-cache-lru-misses cache))
      nil)))

;;;###autoload
(defun nskk-cache-lru-put (cache key value)
  "Put KEY and VALUE pair into LRU CACHE."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (if node
        ;; 既存エントリ：値を更新してヘッドに移動
        (progn
          (setf (nskk-cache-lru-node-value node) value)
          (nskk-cache-lru--move-to-head cache node))
      ;; 新規エントリ
      (let ((new-node (nskk-cache-lru-node--create :key key :value value)))
        (puthash key new-node (nskk-cache-lru-hash cache))
        (nskk-cache-lru--add-to-head cache new-node)
        (cl-incf (nskk-cache-lru-size cache))
        ;; 容量超過チェック
        (when (> (nskk-cache-lru-size cache) (nskk-cache-lru-capacity cache))
          (let ((tail-node (nskk-cache-lru--remove-tail cache)))
            (remhash (nskk-cache-lru-node-key tail-node) (nskk-cache-lru-hash cache))
            (cl-decf (nskk-cache-lru-size cache))))))))

;;;###autoload
(defun nskk-cache-lru-invalidate (cache key)
  "Invalidate KEY in LRU CACHE.
Return t if KEY was found and removed, nil otherwise."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (when node
      (nskk-cache-lru--remove-node node)
      (remhash key (nskk-cache-lru-hash cache))
      (cl-decf (nskk-cache-lru-size cache))
      t)))

;;;###autoload
(defun nskk-cache-lru-clear (cache)
  "Clear all entries from LRU CACHE."
  (clrhash (nskk-cache-lru-hash cache))
  (setf (nskk-cache-lru-size cache) 0)
  ;; ヘッドとテールを再接続
  (let ((head (nskk-cache-lru-head cache))
        (tail (nskk-cache-lru-tail cache)))
    (setf (nskk-cache-lru-node-next head) tail)
    (setf (nskk-cache-lru-node-prev tail) head))
  ;; 統計をリセット
  (setf (nskk-cache-lru-hits cache) 0)
  (setf (nskk-cache-lru-misses cache) 0))

;;; LFUキャッシュ実装

;;;###autoload
(defun nskk-cache-lfu-create (capacity)
  "Create an LFU cache with CAPACITY entries."
  (nskk-cache-lfu--create
   :capacity capacity
   :size 0
   :hash (make-hash-table :test 'equal :size capacity)
   :freq (make-hash-table :test 'equal :size capacity)
   :min-freq 0
   :hits 0
   :misses 0))

(defsubst nskk-cache-lfu--update-freq (cache entry old-freq)
  "Update frequency of ENTRY in LFU CACHE from OLD-FREQ."
  (let ((freq-table (nskk-cache-lfu-freq cache))
        (key (nskk-cache-lfu-entry-key entry))
        (new-freq (nskk-cache-lfu-entry-frequency entry)))
    ;; 古い頻度のリストから削除
    (when old-freq
      (let ((keys (gethash old-freq freq-table)))
        (setq keys (delq key keys))
        (if keys
            (puthash old-freq keys freq-table)
          (remhash old-freq freq-table)
          ;; 最小頻度が削除された場合、更新
          (when (= old-freq (nskk-cache-lfu-min-freq cache))
            (setf (nskk-cache-lfu-min-freq cache) new-freq)))))
    ;; 新しい頻度のリストに追加（FIFO順序: 末尾に追加）
    (let ((keys (gethash new-freq freq-table)))
      (puthash new-freq (append keys (list key)) freq-table))))

;;;###autoload
(defun nskk-cache-lfu-get (cache key)
  "Get value for KEY from LFU CACHE."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (if entry
        (progn
          ;; キャッシュヒット：頻度を増やす
          (let ((old-freq (nskk-cache-lfu-entry-frequency entry)))
            (cl-incf (nskk-cache-lfu-entry-frequency entry))
            (nskk-cache-lfu--update-freq cache entry old-freq))
          (cl-incf (nskk-cache-lfu-hits cache))
          (nskk-cache-lfu-entry-value entry))
      ;; キャッシュミス
      (cl-incf (nskk-cache-lfu-misses cache))
      nil)))

;;;###autoload
(defun nskk-cache-lfu-put (cache key value)
  "Put KEY and VALUE pair into LFU CACHE."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (if entry
        ;; 既存エントリ：値を更新して頻度を増やす
        (let ((old-freq (nskk-cache-lfu-entry-frequency entry)))
          (setf (nskk-cache-lfu-entry-value entry) value)
          (cl-incf (nskk-cache-lfu-entry-frequency entry))
          (nskk-cache-lfu--update-freq cache entry old-freq))
      ;; 新規エントリ
      (when (>= (nskk-cache-lfu-size cache) (nskk-cache-lfu-capacity cache))
        ;; 容量超過：最小頻度のエントリを削除
        (let* ((min-freq (nskk-cache-lfu-min-freq cache))
               (keys (gethash min-freq (nskk-cache-lfu-freq cache)))
               (evict-key (car keys)))
          (when evict-key
            (remhash evict-key (nskk-cache-lfu-hash cache))
            (setq keys (cdr keys))
            (if keys
                (puthash min-freq keys (nskk-cache-lfu-freq cache))
              (remhash min-freq (nskk-cache-lfu-freq cache)))
            (cl-decf (nskk-cache-lfu-size cache)))))
      ;; 新規エントリを追加
      (let ((new-entry (nskk-cache-lfu-entry--create :key key :value value :frequency 1)))
        (puthash key new-entry (nskk-cache-lfu-hash cache))
        (nskk-cache-lfu--update-freq cache new-entry nil)
        (setf (nskk-cache-lfu-min-freq cache) 1)
        (cl-incf (nskk-cache-lfu-size cache))))))

;;;###autoload
(defun nskk-cache-lfu-invalidate (cache key)
  "Invalidate KEY in LFU CACHE.
Return t if KEY was found and removed, nil otherwise."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (when entry
      (let* ((freq (nskk-cache-lfu-entry-frequency entry))
             (keys (gethash freq (nskk-cache-lfu-freq cache))))
        (setq keys (delq key keys))
        (if keys
            (puthash freq keys (nskk-cache-lfu-freq cache))
          (remhash freq (nskk-cache-lfu-freq cache))))
      (remhash key (nskk-cache-lfu-hash cache))
      (cl-decf (nskk-cache-lfu-size cache))
      t)))

;;;###autoload
(defun nskk-cache-lfu-clear (cache)
  "Clear all entries from LFU CACHE."
  (clrhash (nskk-cache-lfu-hash cache))
  (clrhash (nskk-cache-lfu-freq cache))
  (setf (nskk-cache-lfu-size cache) 0)
  (setf (nskk-cache-lfu-min-freq cache) 0)
  (setf (nskk-cache-lfu-hits cache) 0)
  (setf (nskk-cache-lfu-misses cache) 0))

;;; 統合インターフェース

;;;###autoload
(defun nskk-cache-create (&rest args)
  "Create a cache.
ARGS can be TYPE and CAPACITY positional, or keyword :type and :size/:capacity."
  (let ((cache-type nskk-cache-default-type)
        (cache-capacity nskk-cache-default-capacity))
    (cond
     ((null args))
     ((keywordp (car args))
      (let ((plist args))
        (when (plist-member plist :type)
          (setq cache-type (plist-get plist :type)))
        (when (plist-member plist :capacity)
          (setq cache-capacity (plist-get plist :capacity)))
        (when (plist-member plist :size)
          (setq cache-capacity (plist-get plist :size)))))
     (t
      (setq cache-type (car args))
      (when (> (length args) 1)
        (setq cache-capacity (nth 1 args)))))
    (pcase cache-type
      ('lru (nskk-cache-lru-create cache-capacity))
      ('lfu (nskk-cache-lfu-create cache-capacity))
      (_ (error "Unknown cache type: %s" cache-type)))))

;;;###autoload
(defun nskk-cache-get (cache key)
  "Get value for KEY from CACHE."
  (cond
   ((nskk-cache-lru-p cache)
    (nskk-cache-lru-get cache key))
   ((nskk-cache-lfu-p cache)
    (nskk-cache-lfu-get cache key))
   (t (error "Invalid cache type"))))

;;;###autoload
(defun nskk-cache-put (cache key value)
  "Put KEY and VALUE pair into CACHE."
  (cond
   ((nskk-cache-lru-p cache)
    (nskk-cache-lru-put cache key value))
   ((nskk-cache-lfu-p cache)
    (nskk-cache-lfu-put cache key value))
   (t (error "Invalid cache type"))))

;;;###autoload
(defun nskk-cache-invalidate (cache key)
  "Invalidate KEY in CACHE.
Return t if KEY was found and removed, nil otherwise."
  (cond
   ((nskk-cache-lru-p cache)
    (nskk-cache-lru-invalidate cache key))
   ((nskk-cache-lfu-p cache)
    (nskk-cache-lfu-invalidate cache key))
   (t (error "Invalid cache type"))))

;;;###autoload
(defun nskk-cache-clear (cache)
  "Clear all entries from CACHE."
  (cond
   ((nskk-cache-lru-p cache)
    (nskk-cache-lru-clear cache))
   ((nskk-cache-lfu-p cache)
    (nskk-cache-lfu-clear cache))
   (t (error "Invalid cache type"))))

;;;###autoload
(defun nskk-cache-invalidate-pattern (cache pattern)
  "Invalidate all keys matching PATTERN in CACHE.
Return a list of invalidated keys."
  (let ((deleted-keys nil)
        (hash-table (cond
                     ((nskk-cache-lru-p cache)
                      (nskk-cache-lru-hash cache))
                     ((nskk-cache-lfu-p cache)
                      (nskk-cache-lfu-hash cache))
                     (t (error "Invalid cache type")))))
    (maphash (lambda (key _value)
               (when (string-match-p pattern key)
                 (nskk-cache-invalidate cache key)
                 (push key deleted-keys)))
             hash-table)
    (nreverse deleted-keys)))

;;;###autoload
(defun nskk-cache-stats (cache)
  "Return statistics plist for CACHE."
  (let* ((type (cond ((nskk-cache-lru-p cache) 'lru)
                     ((nskk-cache-lfu-p cache) 'lfu)
                     (t 'unknown)))
         (capacity (cond ((nskk-cache-lru-p cache) (nskk-cache-lru-capacity cache))
                        ((nskk-cache-lfu-p cache) (nskk-cache-lfu-capacity cache))
                        (t 0)))
         (size (cond ((nskk-cache-lru-p cache) (nskk-cache-lru-size cache))
                    ((nskk-cache-lfu-p cache) (nskk-cache-lfu-size cache))
                    (t 0)))
         (hits (cond ((nskk-cache-lru-p cache) (nskk-cache-lru-hits cache))
                    ((nskk-cache-lfu-p cache) (nskk-cache-lfu-hits cache))
                    (t 0)))
         (misses (cond ((nskk-cache-lru-p cache) (nskk-cache-lru-misses cache))
                      ((nskk-cache-lfu-p cache) (nskk-cache-lfu-misses cache))
                      (t 0)))
         (total (+ hits misses))
         (hit-rate (if (> total 0) (/ (float hits) total) 0.0)))
    (list :type type
          :capacity capacity
          :size size
          :hits hits
          :misses misses
          :hit-rate hit-rate)))

;;;###autoload
(defun nskk-cache-hit-rate (cache)
  "Return hit rate for CACHE as a float between 0.0 and 1.0."
  (plist-get (nskk-cache-stats cache) :hit-rate))

;;;###autoload
(defun nskk-cache-p (cache)
  "Return non-nil if CACHE is a valid cache structure."
  (or (nskk-cache-lru-p cache)
      (nskk-cache-lfu-p cache)))

;;;###autoload
(defun nskk-cache-size (cache)
  "Return the current number of entries in CACHE."
  (cond
   ((nskk-cache-lru-p cache)
    (nskk-cache-lru-size cache))
   ((nskk-cache-lfu-p cache)
    (nskk-cache-lfu-size cache))
   (t (error "Invalid cache type"))))

(provide 'nskk-cache)

;;; nskk-cache.el ends here
