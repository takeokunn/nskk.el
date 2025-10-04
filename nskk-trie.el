;;; nskk-trie.el --- Trie data structure for fast prefix search -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, trie
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

;; このファイルは高速な前方一致検索を実現するトライ木（Trie Tree）を実装します。
;;
;; トライ木は文字列検索に最適化されたデータ構造で、以下の特徴があります:
;; - 挿入/検索/削除の計算量: O(m) (m=キー長)
;; - 前方一致検索: O(k + n) (k=prefix長, n=結果数)
;; - メモリ効率的な実装（ハッシュテーブルベース）
;;
;; 使用例:
;;
;;   (require 'nskk-trie)
;;
;;   ;; トライ木の作成
;;   (let ((trie (nskk-trie-create)))
;;     ;; エントリの挿入
;;     (nskk-trie-insert trie "かんじ" entry1)
;;     (nskk-trie-insert trie "かんたん" entry2)
;;     (nskk-trie-insert trie "かん" entry3)
;;
;;     ;; 完全一致検索
;;     (nskk-trie-lookup trie "かんじ")
;;     ;; => (entry1 . t)
;;
;;     ;; 前方一致検索
;;     (nskk-trie-prefix-search trie "かん")
;;     ;; => (("かん" . entry3) ("かんじ" . entry1) ("かんたん" . entry2))
;;
;;     ;; 削除
;;     (nskk-trie-delete trie "かんじ")
;;
;;     ;; シリアライズ
;;     (nskk-trie-save-to-file trie "/tmp/trie.dat"))
;;
;; パフォーマンス目標:
;; - 10万キー挿入: < 3秒
;; - 検索: < 1ms
;; - 前方一致検索（100件）: < 10ms
;; - メモリ使用量: 10万キーで < 30MB

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-trie nil
  "Trie data structure for NSKK."
  :group 'nskk
  :prefix "nskk-trie-")

(defcustom nskk-trie-default-hash-size 50
  "トライ木ノードの子ノード用ハッシュテーブルのデフォルトサイズ。"
  :type 'integer
  :group 'nskk-trie)

;;; データ構造定義

(cl-defstruct (nskk-trie-node
               (:constructor nskk-trie-node--create)
               (:copier nil))
  "トライ木のノード構造。

スロット:
  char     - このノードが表す文字（ルートノードはnil）
  children - 子ノードのハッシュテーブル（文字→ノード）
  value    - このノードに関連付けられた値（終端ノードのみ）
  is-end   - 終端ノードか（キーの終わり）
  count    - このノードを通過するキーの数（統計用）"
  (char nil :type (or null character))
  (children nil :type (or null hash-table))
  (value nil)
  (is-end nil :type boolean)
  (count 0 :type integer))

(cl-defstruct (nskk-trie
               (:constructor nskk-trie--create-internal)
               (:copier nil))
  "トライ木の構造。

スロット:
  root      - ルートノード
  size      - 格納されているキーの総数
  metadata  - メタデータ（plist）"
  (root nil :type nskk-trie-node)
  (size 0 :type integer)
  (metadata nil :type list))

;;; トライ木の基本操作

;;;###autoload
(defun nskk-trie-create ()
  "空のトライ木を作成する。

戻り値:
  nskk-trie構造体"
  (nskk-trie--create-internal
   :root (nskk-trie-node--create)
   :size 0
   :metadata nil))

;;;###autoload
(defun nskk-trie-insert (trie key value)
  "トライ木TRIEにKEYとVALUEを挿入する。

引数:
  TRIE  - nskk-trie構造体
  KEY   - キー文字列
  VALUE - 関連付ける値

処理:
  1. ルートから開始
  2. KEYの各文字について:
     - 対応する子ノードがあればそれをたどる
     - なければ新しいノードを作成
  3. 最後のノードにVALUEを設定
  4. サイズをインクリメント

戻り値:
  TRIE（更新後）"
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (when (zerop (length key))
    (error "Key cannot be empty"))

  (let ((node (nskk-trie-root trie))
        (key-len (length key))
        (was-new nil))

    ;; KEYの各文字をたどる
    (dotimes (i key-len)
      (let ((char (aref key i)))
        ;; 子ノードテーブルがなければ作成
        (unless (nskk-trie-node-children node)
          (setf (nskk-trie-node-children node)
                (make-hash-table :test 'eq :size nskk-trie-default-hash-size)))

        ;; 子ノードを取得または作成
        (let ((next-node (gethash char (nskk-trie-node-children node))))
          (unless next-node
            (setq next-node (nskk-trie-node--create :char char))
            (puthash char next-node (nskk-trie-node-children node)))

          ;; カウントをインクリメント
          (cl-incf (nskk-trie-node-count next-node))

          ;; 次のノードへ移動
          (setq node next-node))))

    ;; 最後のノードに値を設定
    ;; 既に終端ノードであれば上書き、新規であればサイズをインクリメント
    (setq was-new (not (nskk-trie-node-is-end node)))
    (setf (nskk-trie-node-is-end node) t)
    (setf (nskk-trie-node-value node) value)

    ;; サイズ更新
    (when was-new
      (cl-incf (nskk-trie-size trie)))

    trie))

;;;###autoload
(defun nskk-trie-lookup (trie key)
  "トライ木TRIEからKEYに対応する値を検索する。

引数:
  TRIE - nskk-trie構造体
  KEY  - 検索するキー文字列

戻り値:
  見つかった場合: (値 . t)
  見つからない場合: (nil . nil)"
  (unless (stringp key)
    (error "Key must be a string: %s" key))

  (let ((node (nskk-trie--find-node trie key)))
    (if (and node (nskk-trie-node-is-end node))
        (cons (nskk-trie-node-value node) t)
      (cons nil nil))))

;;;###autoload
(defun nskk-trie-has-key-p (trie key)
  "トライ木TRIEがKEYを持つか判定する。

引数:
  TRIE - nskk-trie構造体
  KEY  - 検索するキー文字列

戻り値:
  存在する場合t、しない場合nil"
  (let ((node (nskk-trie--find-node trie key)))
    (and node (nskk-trie-node-is-end node))))

(defun nskk-trie--find-node (trie key)
  "トライ木TRIEからKEYに対応するノードを検索する（内部関数）。

引数:
  TRIE - nskk-trie構造体
  KEY  - 検索するキー文字列

戻り値:
  見つかったノード、見つからない場合はnil"
  (let ((node (nskk-trie-root trie))
        (key-len (length key)))

    ;; KEYの各文字をたどる
    (catch 'not-found
      (dotimes (i key-len)
        (let ((char (aref key i)))
          (unless (and (nskk-trie-node-children node)
                      (setq node (gethash char (nskk-trie-node-children node))))
            (throw 'not-found nil))))
      node)))

;;;###autoload
(defun nskk-trie-delete (trie key)
  "トライ木TRIEからKEYを削除する。

引数:
  TRIE - nskk-trie構造体
  KEY  - 削除するキー文字列

処理:
  1. KEYを検索
  2. 終端ノードのis-endをnilに設定
  3. 値をクリア
  4. 子ノードがない場合、親方向にノードを削除
  5. サイズをデクリメント

戻り値:
  削除成功の場合t、キーが存在しない場合nil"
  (unless (stringp key)
    (error "Key must be a string: %s" key))

  (let ((node (nskk-trie--find-node trie key)))
    (when (and node (nskk-trie-node-is-end node))
      ;; 終端フラグをクリア
      (setf (nskk-trie-node-is-end node) nil)
      (setf (nskk-trie-node-value node) nil)

      ;; サイズをデクリメント
      (cl-decf (nskk-trie-size trie))

      ;; 不要なノードを削除（最適化）
      ;; 子ノードがなく、終端でもない場合は親から削除
      (nskk-trie--cleanup-path trie key)

      t)))

(defun nskk-trie--cleanup-path (trie key)
  "削除後の不要ノードをクリーンアップする（内部関数）。

引数:
  TRIE - nskk-trie構造体
  KEY  - 削除したキー文字列

処理:
  KEYのパスを逆順にたどり、子がなく終端でもないノードを削除"
  (let ((nodes-path nil)
        (node (nskk-trie-root trie))
        (key-len (length key)))

    ;; パスを記録
    (dotimes (i key-len)
      (let ((char (aref key i)))
        (when (and (nskk-trie-node-children node)
                  (setq node (gethash char (nskk-trie-node-children node))))
          (push (cons char node) nodes-path))))

    ;; 逆順にクリーンアップ
    (let ((parent (nskk-trie-root trie))
          (parent-stack nil))
      (setq node (nskk-trie-root trie))

      (dotimes (i key-len)
        (let ((char (aref key i)))
          (push parent parent-stack)
          (setq node (gethash char (nskk-trie-node-children node)))))

      ;; 末尾から親方向にたどる
      (dotimes (i key-len)
        (when (and node
                  (not (nskk-trie-node-is-end node))
                  (or (null (nskk-trie-node-children node))
                      (zerop (hash-table-count (nskk-trie-node-children node)))))
          ;; このノードは削除可能
          (setq parent (pop parent-stack))
          (when (nskk-trie-node-children parent)
            (remhash (nskk-trie-node-char node) (nskk-trie-node-children parent)))
          (setq node parent))))))

;;; 前方一致検索

;;;###autoload
(defun nskk-trie-prefix-search (trie prefix &optional limit)
  "トライ木TRIEからPREFIXで始まるすべてのキーを検索する。

引数:
  TRIE   - nskk-trie構造体
  PREFIX - 前方一致させるプレフィックス
  LIMIT  - 結果の最大数（nilの場合無制限）

戻り値:
  ((key . value) ...) のリスト

アルゴリズム:
  1. PREFIXをたどって開始ノードを見つける
  2. そのノードから深さ優先探索
  3. 終端ノードに到達したらキーと値を収集
  4. LIMITに達したら探索を中断"
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))

  ;; PREFIXのノードを見つける
  (let ((node (if (zerop (length prefix))
                 (nskk-trie-root trie)
               (nskk-trie--find-node trie prefix))))

    (if node
        (nskk-trie--collect-all node prefix limit 0)
      nil)))

(defun nskk-trie--collect-all (node prefix limit collected-count)
  "ノードNODEから始まるすべてのキーと値を収集する（内部関数）。

引数:
  NODE            - 開始ノード
  PREFIX          - 現在のプレフィックス
  LIMIT           - 収集する最大数（nilの場合無制限）
  COLLECTED-COUNT - 既に収集したアイテム数

戻り値:
  ((key . value) ...) のリスト"
  (let ((results nil)
        (count collected-count))

    ;; 深さ優先探索で収集
    (catch 'limit-reached
      (cl-labels ((dfs-collect (node prefix)
                    ;; 現在のノードが終端なら収集
                    (when (nskk-trie-node-is-end node)
                      (push (cons prefix (nskk-trie-node-value node)) results)
                      (cl-incf count)
                      (when (and limit (>= count limit))
                        (throw 'limit-reached nil)))

                    ;; 子ノードを探索
                    (when (nskk-trie-node-children node)
                      (maphash (lambda (char child-node)
                                (let ((new-prefix (concat prefix (char-to-string char))))
                                  (dfs-collect child-node new-prefix)))
                              (nskk-trie-node-children node)))))
        (dfs-collect node prefix)))

    (nreverse results)))

;;; ユーティリティ関数

(defun nskk-trie-size (trie)
  "トライ木TRIEに格納されているキーの総数を返す。

引数:
  TRIE - nskk-trie構造体

戻り値:
  キー数"
  (nskk-trie-size trie))

(defun nskk-trie-empty-p (trie)
  "トライ木TRIEが空か判定する。

引数:
  TRIE - nskk-trie構造体

戻り値:
  空の場合t、そうでない場合nil"
  (zerop (nskk-trie-size trie)))

(defun nskk-trie-clear (trie)
  "トライ木TRIEをクリアする。

引数:
  TRIE - nskk-trie構造体

戻り値:
  TRIE（クリア後）"
  (setf (nskk-trie-root trie) (nskk-trie-node--create))
  (setf (nskk-trie-size trie) 0)
  trie)

(defun nskk-trie-keys (trie)
  "トライ木TRIEのすべてのキーを返す。

引数:
  TRIE - nskk-trie構造体

戻り値:
  キーのリスト"
  (mapcar #'car (nskk-trie-prefix-search trie "")))

(defun nskk-trie-statistics (trie)
  "トライ木TRIEの統計情報を返す。

引数:
  TRIE - nskk-trie構造体

戻り値:
  plist形式の統計情報
    :size         - キー数
    :node-count   - ノード総数
    :max-depth    - 最大深度
    :avg-depth    - 平均深度
    :memory-usage - 推定メモリ使用量"
  (let ((node-count 0)
        (max-depth 0)
        (total-depth 0)
        (key-count (nskk-trie-size trie)))

    ;; 統計収集用の再帰関数
    (cl-labels ((count-nodes (node depth)
                  (cl-incf node-count)
                  (when (> depth max-depth)
                    (setq max-depth depth))
                  (when (nskk-trie-node-is-end node)
                    (cl-incf total-depth depth))
                  (when (nskk-trie-node-children node)
                    (maphash (lambda (_ child)
                              (count-nodes child (1+ depth)))
                            (nskk-trie-node-children node)))))
      (count-nodes (nskk-trie-root trie) 0))

    ;; メモリ使用量の推定
    ;; ノードごとに約200バイト + ハッシュテーブルオーバーヘッド
    (let ((memory-usage (* node-count 250)))

      (list :size key-count
            :node-count node-count
            :max-depth max-depth
            :avg-depth (if (> key-count 0)
                          (/ (float total-depth) key-count)
                        0.0)
            :memory-usage memory-usage))))

;;; シリアライズ

;;;###autoload
(defun nskk-trie-serialize (trie)
  "トライ木TRIEをシリアライズ可能な形式に変換する。

引数:
  TRIE - nskk-trie構造体

戻り値:
  plist形式のデータ
    (:version VERSION
     :size SIZE
     :metadata METADATA
     :entries ((KEY . VALUE) ...))"
  (let ((entries (nskk-trie-prefix-search trie "")))
    (list :version "1.0"
          :size (nskk-trie-size trie)
          :metadata (nskk-trie-metadata trie)
          :entries entries)))

;;;###autoload
(defun nskk-trie-deserialize (data)
  "シリアライズされたDATAからトライ木を復元する。

引数:
  DATA - nskk-trie-serializeの戻り値

戻り値:
  nskk-trie構造体"
  (let ((version (plist-get data :version))
        (entries (plist-get data :entries))
        (metadata (plist-get data :metadata))
        (trie (nskk-trie-create)))

    ;; バージョンチェック
    (unless (equal version "1.0")
      (error "Unsupported trie version: %s" version))

    ;; エントリを復元
    (dolist (entry entries)
      (nskk-trie-insert trie (car entry) (cdr entry)))

    ;; メタデータを復元
    (setf (nskk-trie-metadata trie) metadata)

    trie))

(defun nskk-trie-save-to-file (trie file-path)
  "トライ木TRIEをファイルに保存する。

引数:
  TRIE      - nskk-trie構造体
  FILE-PATH - 保存先ファイルパス"
  (let ((data (nskk-trie-serialize trie)))
    (with-temp-file file-path
      (insert ";; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
      (insert ";; NSKK Trie データファイル\n")
      (insert ";; このファイルは自動生成されました\n\n")
      (prin1 data (current-buffer)))))

(defun nskk-trie-load-from-file (file-path)
  "ファイルからトライ木を読み込む。

引数:
  FILE-PATH - 読み込むファイルパス

戻り値:
  nskk-trie構造体"
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    ;; コメント行をスキップ
    (while (looking-at ";;")
      (forward-line 1))
    (let ((data (read (current-buffer))))
      (nskk-trie-deserialize data))))

(provide 'nskk-trie)

;;; nskk-trie.el ends here
