;;; nskk-layer-core.el --- Core Engine Layer for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, architecture
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

;; Core Engine Layer - 変換エンジンと辞書エンジンのコア機能
;;
;; 責務:
;; - ローマ字→かな変換エンジン
;; - 辞書検索エンジン
;; - 文字変換ユーティリティ
;; - 変換アルゴリズム最適化
;; - コアロジック実装
;;
;; レイヤー依存:
;; - Infrastructure Layer (リソース管理)
;; - 既存のnskk-converter, nskk-search等を統合
;;
;; 主要コンポーネント:
;; - ローマ字変換エンジン
;; - かな⇔カタカナ変換
;; - 半角⇔全角変換
;; - 辞書検索インターフェース
;; - トライ木検索
;;
;; 使用例:
;; (nskk-core-convert-romaji "ka")
;; (nskk-core-search-dictionary "かんじ")
;; (nskk-core-hiragana-to-katakana "あ")

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-converter)
(require 'nskk-romaji-tables)
(require 'nskk-special-chars)

(declare-function nskk-data-search "nskk-layer-data" (query &optional options))
(declare-function nskk-data-lookup "nskk-layer-data" (query))
(declare-function nskk-data-prefix-search "nskk-layer-data" (prefix))

;;; カスタマイズ可能変数

(defgroup nskk-core nil
  "Core engine layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-core-")

(defcustom nskk-core-enable-optimization t
  "コア最適化を有効にするか。"
  :type 'boolean
  :group 'nskk-core)

(defcustom nskk-core-cache-size 10000
  "変換キャッシュのサイズ。"
  :type 'integer
  :group 'nskk-core)

;;; 内部変数

(defvar nskk-core--conversion-cache (make-hash-table :test 'equal)
  "変換結果のキャッシュ。")

(defvar nskk-core--romaji-table nil
  "ローマ字変換テーブル。")

(defvar nskk-core--dictionary-engine nil
  "辞書検索エンジンのディスパッチテーブル。")

(defconst nskk-core--fullwidth-offset (- #xFF01 #x21)
  "ASCII(0x21-0x7E) と対応する全角文字とのオフセット。")

(defconst nskk-core--fullwidth-space #x3000
  "全角スペースのコードポイント。")

(defvar nskk-core--initialized nil
  "初期化済みフラグ。")

;;; 初期化・シャットダウン

(defun nskk-core-initialize ()
  "Core Engine Layerを初期化する。"
  (unless nskk-core--initialized
    (nskk-core--initialize-romaji-table)
    (nskk-core--initialize-dictionary-engine)
    (nskk-core--initialize-cache)
    (setq nskk-core--initialized t)
    (nskk-core--log "Core Engine Layer initialized")))

(defun nskk-core-shutdown ()
  "Core Engine Layerをシャットダウンする。"
  (nskk-core--cleanup-cache)
  (setq nskk-core--romaji-table nil)
  (setq nskk-core--dictionary-engine nil)
  (setq nskk-core--initialized nil)
  (nskk-core--log "Core Engine Layer shutdown"))

(defun nskk-core--initialize-romaji-table ()
  "ローマ字変換テーブルを初期化する。"
  (setq nskk-core--romaji-table (nskk-core--build-romaji-table)))

(defun nskk-core--initialize-dictionary-engine ()
  "辞書検索エンジンを初期化する。"
  (setq nskk-core--dictionary-engine
        (or nskk-core--dictionary-engine
            (nskk-core--create-dictionary-engine))))

(defun nskk-core--initialize-cache ()
  "キャッシュを初期化する。"
  (clrhash nskk-core--conversion-cache))

(defun nskk-core--cleanup-cache ()
  "キャッシュをクリーンアップする。"
  (clrhash nskk-core--conversion-cache))

;;; ローマ字変換エンジン

(defun nskk-core-convert-romaji (input)
  "ローマ字をかなに変換する。
INPUTはローマ字文字列。
戻り値は (確定文字列 . 未確定文字列) のコンスセル。"
  (or (gethash input nskk-core--conversion-cache)
      (let ((result (nskk-core--convert-romaji-uncached input)))
        (when (< (hash-table-count nskk-core--conversion-cache)
                 nskk-core-cache-size)
          (puthash input result nskk-core--conversion-cache))
        result)))

(defun nskk-core--convert-romaji-uncached (input)
  "ローマ字をかなに変換する（キャッシュなし）。
INPUTはローマ字文字列。"
  (nskk-core--log "Converting romaji: %s" input)
  (let* ((result (nskk-convert-romaji input))
         (converted (and result (nskk-converter-result-converted result)))
         (pending (and result (nskk-converter-result-pending result))))
    (cons (or converted "") (or pending ""))))

(defun nskk-core--build-romaji-table ()
  "ローマ字変換テーブルを構築する。"
  (nskk-romaji-init-hash-table)
  nskk-romaji-hash-table)

;;; かな・カタカナ変換

(defun nskk-core-hiragana-to-katakana (hiragana)
  "ひらがなをカタカナに変換する。
HIRAGANAはひらがな文字列。"
  (nskk-hiragana-to-katakana hiragana))

(defun nskk-core-katakana-to-hiragana (katakana)
  "カタカナをひらがなに変換する。
KATAKANAはカタカナ文字列。"
  (nskk-katakana-to-hiragana katakana))

;;; 半角・全角変換

(defun nskk-core-hankaku-to-zenkaku (hankaku)
  "半角文字を全角文字に変換する。
HANKAKUは半角文字列。"
  (mapconcat
   (lambda (code-point)
     (pcase code-point
       (#x20 (string nskk-core--fullwidth-space))
       (_ (if (and (>= code-point #x21) (<= code-point #x7e))
              (string (+ code-point nskk-core--fullwidth-offset))
            (string code-point)))))
   (string-to-list hankaku)
   ""))

(defun nskk-core-zenkaku-to-hankaku (zenkaku)
  "全角文字を半角文字に変換する。
ZENKAKUは全角文字列。"
  (mapconcat
   (lambda (code-point)
     (cond
      ((= code-point nskk-core--fullwidth-space) " ")
      ((and (>= code-point #xFF01) (<= code-point #xFF5E))
       (string (- code-point nskk-core--fullwidth-offset)))
      (t (string code-point))))
   (string-to-list zenkaku)
   ""))

;;; 辞書検索エンジン

(defun nskk-core-search-dictionary (query &optional options)
  "辞書を検索する。
QUERYは検索クエリ、OPTIONSは検索オプション。
戻り値は候補のリスト。"
  (nskk-core--log "Searching dictionary: %s with options %S" query options)
  (if-let ((engine (nskk-core--ensure-dictionary-engine))
           (fn (plist-get engine :search)))
      (funcall fn query options)
    (nskk-core--log "No dictionary engine registered")
    nil))

(defun nskk-core--create-dictionary-engine ()
  "辞書検索エンジンを作成する。"
  (when (fboundp 'nskk-data-search)
    ;; Data Layerが利用可能なら、そちらのAPIに委譲する
    (list :search #'nskk-data-search
          :lookup (when (fboundp 'nskk-data-lookup)
                    #'nskk-data-lookup)
          :prefix (when (fboundp 'nskk-data-prefix-search)
                    #'nskk-data-prefix-search))))

(defun nskk-core-lookup (query)
  "辞書で完全一致検索を行う。
QUERYは検索クエリ。"
  (if-let ((engine (nskk-core--ensure-dictionary-engine))
           (fn (plist-get engine :lookup)))
      (funcall fn query)
    (nskk-core--log "No dictionary engine registered for lookup")
    nil))

(defun nskk-core-prefix-search (prefix)
  "辞書で前方一致検索を行う。
PREFIXは検索プレフィックス。"
  (nskk-core--log "Prefix search: %s" prefix)
  (if-let ((engine (nskk-core--ensure-dictionary-engine))
           (fn (plist-get engine :prefix)))
      (funcall fn prefix)
    nil))

;;; 特殊文字処理

(defun nskk-core-process-sokuon (input)
  "促音（っ）を処理する。
INPUTは入力文字列。"
  ;; 子音重複を促音に変換
  ;; 例: "kka" -> "っか"
  ;; 実装は統合時に完成
  input)

(defun nskk-core-process-hatsuon (input)
  "撥音（ん）を処理する。
INPUTは入力文字列。"
  ;; "nn" や "n'" を "ん" に変換
  ;; 実装は統合時に完成
  input)

;;; 最適化

(defun nskk-core-optimize-table ()
  "変換テーブルを最適化する。"
  (when nskk-core-enable-optimization
    ;; テーブル圧縮、頻出パターン事前計算など
    (nskk-core--log "Optimizing conversion table")))

(defun nskk-core-clear-cache ()
  "変換キャッシュをクリアする。"
  (interactive)
  (clrhash nskk-core--conversion-cache)
  (message "Core engine cache cleared"))

(defun nskk-core-cache-statistics ()
  "キャッシュ統計を表示する。"
  (interactive)
  (message "Cache entries: %d / %d"
           (hash-table-count nskk-core--conversion-cache)
           nskk-core-cache-size))

;;; パフォーマンス測定

(defvar nskk-core--performance-log nil
  "パフォーマンス測定ログ。")

(defun nskk-core-measure-performance (function &rest args)
  "関数のパフォーマンスを測定する。
FUNCTIONは測定する関数、ARGSは引数。"
  (let ((start-time (float-time))
        (result (apply function args))
        (end-time (float-time)))
    (push (list :function function
                :duration (- end-time start-time)
                :timestamp start-time)
          nskk-core--performance-log)
    result))

(defun nskk-core-get-performance-statistics ()
  "パフォーマンス統計を取得する。"
  (interactive)
  (if nskk-core--performance-log
      (let* ((durations (mapcar (lambda (log) (plist-get log :duration))
                                nskk-core--performance-log))
             (avg (/ (apply #'+ durations) (length durations)))
             (max-duration (apply #'max durations))
             (min-duration (apply #'min durations)))
        (message "Performance: avg=%.3fms max=%.3fms min=%.3fms"
                 (* avg 1000) (* max-duration 1000) (* min-duration 1000)))
    (message "No performance data available")))

;;; デバッグ・ロギング

(defvar nskk-core--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-core-enable-debug ()
  "デバッグモードを有効にする。"
  (setq nskk-core--debug-enabled t)
  (message "NSKK Core: Debug mode enabled"))

(defun nskk-core-disable-debug ()
  "デバッグモードを無効にする。"
  (setq nskk-core--debug-enabled nil)
  (message "NSKK Core: Debug mode disabled"))

(defun nskk-core--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-core--debug-enabled
    (apply #'message (concat "[NSKK-Core] " format-string) args)))

;;; ヘルスチェック

(defun nskk-core-health-check ()
  "Core Engine Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((issues '()))
    ;; 初期化状態チェック
    (unless nskk-core--initialized
      (push "Core engine not initialized" issues))
    ;; テーブル状態チェック
    (unless nskk-core--romaji-table
      (push "Romaji table not loaded" issues))
    ;; 辞書エンジンチェック
    (unless nskk-core--dictionary-engine
      (push "Dictionary engine not initialized" issues))
    ;; 結果表示
    (if issues
        (message "Core Engine issues: %s" (string-join issues ", "))
      (message "Core Engine: All systems operational"))))

;;; 統計情報

(defun nskk-core-get-statistics ()
  "Core Layerの統計情報を取得する。
戻り値: 統計情報のplist"
  (list :layer 'core
        :initialized nskk-core--initialized
        :cache-size (hash-table-count nskk-core--conversion-cache)
        :romaji-table-loaded (not (null nskk-core--romaji-table))))

(defun nskk-core-register-dictionary-engine (engine)
  "辞書検索エンジンを登録する。
ENGINEは :search/:lookup/:prefix をキーに持つplist。"
  (unless (plist-get engine :search)
    (error "Dictionary engine must provide :search function"))
  (setq nskk-core--dictionary-engine engine))

(defun nskk-core--ensure-dictionary-engine ()
  "辞書検索エンジンが存在することを保証する。"
  (or nskk-core--dictionary-engine
      (setq nskk-core--dictionary-engine (nskk-core--create-dictionary-engine))))

(provide 'nskk-layer-core)
;;; nskk-layer-core.el ends here
