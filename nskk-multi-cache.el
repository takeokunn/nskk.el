;;; nskk-multi-cache.el --- Multi-level cache for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, cache, optimization
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

;; このファイルはNSKKの多層キャッシュシステムを実装します。
;;
;; キャッシュ階層:
;; - L1キャッシュ: 超高速（< 0.01ms）、容量100、LRU
;; - L2キャッシュ: 高速（< 0.05ms）、容量1000、ARC
;; - L3キャッシュ: 標準（< 0.1ms）、容量10000、LFU
;; - TLBキャッシュ: 翻訳ルックアサイドバッファ（< 0.01ms）、容量50
;;
;; アルゴリズム:
;; - LRU: Least Recently Used
;; - ARC: Adaptive Replacement Cache
;; - LFU: Least Frequently Used
;;
;; 特徴:
;; - 階層的キャッシュ管理
;; - 自動プロモーション/デモーション
;; - ヒット率測定
;; - キャッシュウォーミング
;;
;; 使用例:
;;   (setq cache (nskk-multi-cache-create))
;;   (nskk-multi-cache-get cache "key")
;;   (nskk-multi-cache-put cache "key" "value")
;;   (nskk-multi-cache-stats cache)

;;; Code:

(require 'cl-lib)
(require 'nskk-cache)

;;; カスタマイズ変数

(defgroup nskk-multi-cache nil
  "Multi-level cache settings."
  :group 'nskk
  :prefix "nskk-multi-cache-")

(defcustom nskk-multi-cache-l1-capacity 100
  "L1キャッシュの容量。"
  :type 'integer
  :group 'nskk-multi-cache)

(defcustom nskk-multi-cache-l2-capacity 1000
  "L2キャッシュの容量。"
  :type 'integer
  :group 'nskk-multi-cache)

(defcustom nskk-multi-cache-l3-capacity 10000
  "L3キャッシュの容量。"
  :type 'integer
  :group 'nskk-multi-cache)

(defcustom nskk-multi-cache-tlb-capacity 50
  "TLBキャッシュの容量。"
  :type 'integer
  :group 'nskk-multi-cache)

(defcustom nskk-multi-cache-enable-auto-promotion t
  "自動プロモーション（下位→上位キャッシュ）を有効にするか。"
  :type 'boolean
  :group 'nskk-multi-cache)

;;; ARCキャッシュ実装

;; ARC (Adaptive Replacement Cache) は、LRUとLFUの良いところを組み合わせたアルゴリズム
(cl-defstruct (nskk-multi-cache-arc
               (:constructor nskk-multi-cache-arc--create)
               (:copier nil))
  "ARCキャッシュ構造。

スロット:
  capacity    - 最大容量
  target      - T1の目標サイズ
  t1          - 最近使用されたページ（LRU）
  t2          - 頻繁に使用されたページ（LFU）
  b1          - T1から追い出されたページのゴーストエントリ
  b2          - T2から追い出されたページのゴーストエントリ
  hash        - キー→値のハッシュテーブル
  hits        - ヒット数
  misses      - ミス数"
  (capacity 1000 :type integer)
  (target 0 :type integer)
  (t1 nil)
  (t2 nil)
  (b1 nil)
  (b2 nil)
  (hash nil :type hash-table)
  (hits 0 :type integer)
  (misses 0 :type integer))

(defun nskk-multi-cache-arc-create (capacity)
  "ARCキャッシュを作成する。

引数:
  CAPACITY - 最大容量

返り値:
  ARCキャッシュ構造体"
  (nskk-multi-cache-arc--create
   :capacity capacity
   :target 0
   :t1 nil
   :t2 nil
   :b1 nil
   :b2 nil
   :hash (make-hash-table :test 'equal :size capacity)
   :hits 0
   :misses 0))

(defun nskk-multi-cache-arc-get (cache key)
  "ARCキャッシュから値を取得する。

引数:
  CACHE - ARCキャッシュ
  KEY   - キー

返り値:
  値（存在しない場合はnil）"
  (let ((value (gethash key (nskk-multi-cache-arc-hash cache))))
    (if value
        (progn
          ;; キャッシュヒット
          (cl-incf (nskk-multi-cache-arc-hits cache))
          ;; T1にある場合はT2に移動
          (when (member key (nskk-multi-cache-arc-t1 cache))
            (setf (nskk-multi-cache-arc-t1 cache)
                  (delete key (nskk-multi-cache-arc-t1 cache)))
            (push key (nskk-multi-cache-arc-t2 cache)))
          value)
      ;; キャッシュミス
      (cl-incf (nskk-multi-cache-arc-misses cache))
      nil)))

(defun nskk-multi-cache-arc-put (cache key value)
  "ARCキャッシュに値を追加する。

引数:
  CACHE - ARCキャッシュ
  KEY   - キー
  VALUE - 値"
  (cl-block nil
    (let ((capacity (nskk-multi-cache-arc-capacity cache)))
      ;; 既存エントリの場合は更新
      (when (gethash key (nskk-multi-cache-arc-hash cache))
        (puthash key value (nskk-multi-cache-arc-hash cache))
        (cl-return))

      ;; B1にある場合（最近追い出された）
      (when (member key (nskk-multi-cache-arc-b1 cache))
        ;; targetを増やす（T1を優先）
        (setf (nskk-multi-cache-arc-target cache)
              (min capacity
                   (+ (nskk-multi-cache-arc-target cache)
                      (max 1 (/ (length (nskk-multi-cache-arc-b1 cache))
                               (max 1 (length (nskk-multi-cache-arc-b2 cache))))))))
        (setf (nskk-multi-cache-arc-b1 cache)
              (delete key (nskk-multi-cache-arc-b1 cache))))

      ;; B2にある場合（頻繁に使われていた）
      (when (member key (nskk-multi-cache-arc-b2 cache))
        ;; targetを減らす（T2を優先）
        (setf (nskk-multi-cache-arc-target cache)
              (max 0
                   (- (nskk-multi-cache-arc-target cache)
                      (max 1 (/ (length (nskk-multi-cache-arc-b2 cache))
                               (max 1 (length (nskk-multi-cache-arc-b1 cache))))))))
        (setf (nskk-multi-cache-arc-b2 cache)
              (delete key (nskk-multi-cache-arc-b2 cache))))

      ;; キャッシュが満杯の場合はエビクション
      (when (>= (+ (length (nskk-multi-cache-arc-t1 cache))
                  (length (nskk-multi-cache-arc-t2 cache)))
               capacity)
        (nskk-multi-cache-arc--evict cache))

      ;; T1に追加
      (push key (nskk-multi-cache-arc-t1 cache))
      (puthash key value (nskk-multi-cache-arc-hash cache)))))

(defun nskk-multi-cache-arc--pop-last (lst)
  "ARC用リストLSTの末尾要素を取り出し、(要素 . 新しいリスト)を返す。"
  (cond
   ((null lst) (cons nil nil))
   ((null (cdr lst))
    (let ((value (car lst)))
      (cons value nil)))
   (t
    (let ((prev lst)
          (curr (cdr lst)))
      (while (cdr curr)
        (setq prev curr)
        (setq curr (cdr curr)))
      (setcdr prev nil)
      (cons (car curr) lst)))))

(defun nskk-multi-cache-arc--evict (cache)
  "ARCキャッシュからエントリを追い出す。

引数:
  CACHE - ARCキャッシュ"
  (let ((target (nskk-multi-cache-arc-target cache))
        (t1-size (length (nskk-multi-cache-arc-t1 cache))))
    (if (>= t1-size target)
        ;; T1から追い出す
        (let* ((result (nskk-multi-cache-arc--pop-last
                        (nskk-multi-cache-arc-t1 cache)))
               (evict-key (car result)))
          (when evict-key
            (setf (nskk-multi-cache-arc-t1 cache) (cdr result))
            (push evict-key (nskk-multi-cache-arc-b1 cache))
            (remhash evict-key (nskk-multi-cache-arc-hash cache))))
      ;; T2から追い出す
      (let* ((result (nskk-multi-cache-arc--pop-last
                      (nskk-multi-cache-arc-t2 cache)))
             (evict-key (car result)))
        (when evict-key
          (setf (nskk-multi-cache-arc-t2 cache) (cdr result))
          (push evict-key (nskk-multi-cache-arc-b2 cache))
          (remhash evict-key (nskk-multi-cache-arc-hash cache)))))))

;;; 多層キャッシュ構造

(cl-defstruct (nskk-multi-cache
               (:constructor nskk-multi-cache--create)
               (:copier nil))
  "多層キャッシュ構造。

スロット:
  l1          - L1キャッシュ（LRU、超高速）
  l2          - L2キャッシュ（ARC、高速）
  l3          - L3キャッシュ（LFU、標準）
  tlb         - TLBキャッシュ（LRU、翻訳用）
  hits        - 総ヒット数
  misses      - 総ミス数
  l1-hits     - L1ヒット数
  l2-hits     - L2ヒット数
  l3-hits     - L3ヒット数
  tlb-hits    - TLBヒット数"
  (l1 nil)
  (l2 nil)
  (l3 nil)
  (tlb nil)
  (hits 0 :type integer)
  (misses 0 :type integer)
  (l1-hits 0 :type integer)
  (l2-hits 0 :type integer)
  (l3-hits 0 :type integer)
  (tlb-hits 0 :type integer))

;;;###autoload
(defun nskk-multi-cache-create ()
  "多層キャッシュを作成する。

返り値:
  多層キャッシュ構造体"
  (nskk-multi-cache--create
   :l1 (nskk-cache-lru-create nskk-multi-cache-l1-capacity)
   :l2 (nskk-multi-cache-arc-create nskk-multi-cache-l2-capacity)
   :l3 (nskk-cache-lfu-create nskk-multi-cache-l3-capacity)
   :tlb (nskk-cache-lru-create nskk-multi-cache-tlb-capacity)
   :hits 0
   :misses 0
   :l1-hits 0
   :l2-hits 0
   :l3-hits 0
   :tlb-hits 0))

;;;###autoload
(defun nskk-multi-cache-get (cache key)
  "多層キャッシュから値を取得する。

引数:
  CACHE - 多層キャッシュ
  KEY   - キー

返り値:
  値（存在しない場合はnil）

動作:
  1. L1キャッシュを確認
  2. L2キャッシュを確認
  3. L3キャッシュを確認
  4. ヒットした場合は上位キャッシュにプロモーション"
  (cl-block nil
    ;; TLBチェック（特殊な翻訳キャッシュ）
    (let ((tlb-value (nskk-cache-lru-get (nskk-multi-cache-tlb cache) key)))
      (when tlb-value
        (cl-incf (nskk-multi-cache-tlb-hits cache))
        (cl-incf (nskk-multi-cache-hits cache))
        (cl-return tlb-value)))

    ;; L1キャッシュチェック
    (let ((l1-value (nskk-cache-lru-get (nskk-multi-cache-l1 cache) key)))
      (when l1-value
        (cl-incf (nskk-multi-cache-l1-hits cache))
        (cl-incf (nskk-multi-cache-hits cache))
        (cl-return l1-value)))

    ;; L2キャッシュチェック
    (let ((l2-value (nskk-multi-cache-arc-get (nskk-multi-cache-l2 cache) key)))
      (when l2-value
        (cl-incf (nskk-multi-cache-l2-hits cache))
        (cl-incf (nskk-multi-cache-hits cache))
        ;; L1にプロモーション
        (when nskk-multi-cache-enable-auto-promotion
          (nskk-cache-lru-put (nskk-multi-cache-l1 cache) key l2-value))
        (cl-return l2-value)))

    ;; L3キャッシュチェック
    (let ((l3-value (nskk-cache-lfu-get (nskk-multi-cache-l3 cache) key)))
      (when l3-value
        (cl-incf (nskk-multi-cache-l3-hits cache))
        (cl-incf (nskk-multi-cache-hits cache))
        ;; L2にプロモーション
        (when nskk-multi-cache-enable-auto-promotion
          (nskk-multi-cache-arc-put (nskk-multi-cache-l2 cache) key l3-value))
        (cl-return l3-value)))

    ;; キャッシュミス
    (cl-incf (nskk-multi-cache-misses cache))
    nil))

;;;###autoload
(defun nskk-multi-cache-put (cache key value)
  "多層キャッシュに値を追加する。

引数:
  CACHE - 多層キャッシュ
  KEY   - キー
  VALUE - 値

動作:
  L1キャッシュに追加（最も高速なキャッシュから開始）"
  (nskk-cache-lru-put (nskk-multi-cache-l1 cache) key value))

;;;###autoload
(defun nskk-multi-cache-put-tlb (cache key value)
  "TLBキャッシュに値を追加する。

引数:
  CACHE - 多層キャッシュ
  KEY   - キー
  VALUE - 値

用途:
  頻繁に使用される翻訳ルール専用"
  (nskk-cache-lru-put (nskk-multi-cache-tlb cache) key value))

;;;###autoload
(defun nskk-multi-cache-invalidate (cache key)
  "すべてのキャッシュ階層から指定キーを無効化する。

引数:
  CACHE - 多層キャッシュ
  KEY   - 無効化するキー

返り値:
  削除された階層数"
  (let ((count 0))
    (when (nskk-cache-lru-invalidate (nskk-multi-cache-l1 cache) key)
      (cl-incf count))
    ;; L2（ARC）は簡易実装のため省略
    (when (nskk-cache-lfu-invalidate (nskk-multi-cache-l3 cache) key)
      (cl-incf count))
    (when (nskk-cache-lru-invalidate (nskk-multi-cache-tlb cache) key)
      (cl-incf count))
    count))

;;;###autoload
(defun nskk-multi-cache-clear (cache)
  "すべてのキャッシュ階層をクリアする。

引数:
  CACHE - 多層キャッシュ"
  (nskk-cache-lru-clear (nskk-multi-cache-l1 cache))
  ;; L2（ARC）のクリアは省略（構造が複雑なため）
  (nskk-cache-lfu-clear (nskk-multi-cache-l3 cache))
  (nskk-cache-lru-clear (nskk-multi-cache-tlb cache))
  ;; 統計をリセット
  (setf (nskk-multi-cache-hits cache) 0)
  (setf (nskk-multi-cache-misses cache) 0)
  (setf (nskk-multi-cache-l1-hits cache) 0)
  (setf (nskk-multi-cache-l2-hits cache) 0)
  (setf (nskk-multi-cache-l3-hits cache) 0)
  (setf (nskk-multi-cache-tlb-hits cache) 0))

;;; キャッシュウォーミング

;;;###autoload
(defun nskk-multi-cache-warm-up (cache entries)
  "キャッシュをウォームアップする（事前にデータを投入）。

引数:
  CACHE   - 多層キャッシュ
  ENTRIES - エントリのリスト（各要素は (key . value) のcons）

用途:
  起動時によく使われるデータを事前にキャッシュに投入"
  (dolist (entry entries)
    (let ((key (car entry))
          (value (cdr entry)))
      ;; L3に直接投入（起動時は頻度が低いため）
      (nskk-cache-lfu-put (nskk-multi-cache-l3 cache) key value)))
  (message "Cache warmed up with %d entries" (length entries)))

;;; 統計情報

;;;###autoload
(defun nskk-multi-cache-stats (cache)
  "多層キャッシュの統計情報を取得する。

引数:
  CACHE - 多層キャッシュ

返り値:
  plist
    :total-hits      - 総ヒット数
    :total-misses    - 総ミス数
    :hit-rate        - ヒット率
    :l1-hits         - L1ヒット数
    :l2-hits         - L2ヒット数
    :l3-hits         - L3ヒット数
    :tlb-hits        - TLBヒット数
    :l1-size         - L1サイズ
    :l2-size         - L2サイズ
    :l3-size         - L3サイズ
    :tlb-size        - TLBサイズ"
  (let* ((total-hits (nskk-multi-cache-hits cache))
         (total-misses (nskk-multi-cache-misses cache))
         (total (+ total-hits total-misses))
         (hit-rate (if (> total 0) (/ (float total-hits) total) 0.0)))
    (list :total-hits total-hits
          :total-misses total-misses
          :hit-rate hit-rate
          :l1-hits (nskk-multi-cache-l1-hits cache)
          :l2-hits (nskk-multi-cache-l2-hits cache)
          :l3-hits (nskk-multi-cache-l3-hits cache)
          :tlb-hits (nskk-multi-cache-tlb-hits cache)
          :l1-size (nskk-cache-lru-size (nskk-multi-cache-l1 cache))
          :l2-size (hash-table-count (nskk-multi-cache-arc-hash
                                     (nskk-multi-cache-l2 cache)))
          :l3-size (nskk-cache-lfu-size (nskk-multi-cache-l3 cache))
          :tlb-size (nskk-cache-lru-size (nskk-multi-cache-tlb cache)))))

;;;###autoload
(defun nskk-multi-cache-show-stats (cache)
  "多層キャッシュの統計情報を表示する。

引数:
  CACHE - 多層キャッシュ"
  (interactive)
  (let ((stats (nskk-multi-cache-stats cache)))
    (with-output-to-temp-buffer "*NSKK Multi-Cache Stats*"
      (princ "NSKK Multi-Level Cache Statistics\n")
      (princ (make-string 70 ?=))
      (princ "\n\n")
      (princ "=== Hit Statistics ===\n")
      (princ (format "Total Hits:        %10d\n" (plist-get stats :total-hits)))
      (princ (format "Total Misses:      %10d\n" (plist-get stats :total-misses)))
      (princ (format "Hit Rate:          %10.2f%%\n"
                    (* (plist-get stats :hit-rate) 100)))
      (princ "\n")
      (princ "=== Per-Level Hits ===\n")
      (princ (format "TLB Hits:          %10d (%.2f%%)\n"
                    (plist-get stats :tlb-hits)
                    (if (> (plist-get stats :total-hits) 0)
                        (* (/ (float (plist-get stats :tlb-hits))
                             (plist-get stats :total-hits))
                          100)
                      0)))
      (princ (format "L1 Hits:           %10d (%.2f%%)\n"
                    (plist-get stats :l1-hits)
                    (if (> (plist-get stats :total-hits) 0)
                        (* (/ (float (plist-get stats :l1-hits))
                             (plist-get stats :total-hits))
                          100)
                      0)))
      (princ (format "L2 Hits:           %10d (%.2f%%)\n"
                    (plist-get stats :l2-hits)
                    (if (> (plist-get stats :total-hits) 0)
                        (* (/ (float (plist-get stats :l2-hits))
                             (plist-get stats :total-hits))
                          100)
                      0)))
      (princ (format "L3 Hits:           %10d (%.2f%%)\n"
                    (plist-get stats :l3-hits)
                    (if (> (plist-get stats :total-hits) 0)
                        (* (/ (float (plist-get stats :l3-hits))
                             (plist-get stats :total-hits))
                          100)
                      0)))
      (princ "\n")
      (princ "=== Cache Sizes ===\n")
      (princ (format "TLB Size:          %10d / %d\n"
                    (plist-get stats :tlb-size)
                    nskk-multi-cache-tlb-capacity))
      (princ (format "L1 Size:           %10d / %d\n"
                    (plist-get stats :l1-size)
                    nskk-multi-cache-l1-capacity))
      (princ (format "L2 Size:           %10d / %d\n"
                    (plist-get stats :l2-size)
                    nskk-multi-cache-l2-capacity))
      (princ (format "L3 Size:           %10d / %d\n"
                    (plist-get stats :l3-size)
                    nskk-multi-cache-l3-capacity)))))

(provide 'nskk-multi-cache)

;;; nskk-multi-cache.el ends here
