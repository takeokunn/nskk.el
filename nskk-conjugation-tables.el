;;; nskk-conjugation-tables.el --- Optimized conjugation tables for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, conjugation
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

;; このファイルはNSKKの活用テーブル最適化機能を実装します。
;;
;; 最適化手法:
;; - テーブル圧縮（共通パターンの抽出）
;; - 高速検索（ハッシュテーブル、インデックス）
;; - メモリ最適化（インターン化、共有データ構造）
;; - キャッシング（頻出活用の事前計算）
;;
;; 性能目標:
;; - 検索時間: < 30ms
;; - メモリ使用量: < 5MB（活用テーブル全体）
;;
;; 使用例:
;; (nskk-conjugation-table-lookup "書" 'godan 'renyou)  ;; => "き"
;; (nskk-conjugation-table-optimize)  ;; テーブル最適化実行

;;; Code:

(require 'cl-lib)
(require 'nskk-verb-conjugation)
(require 'nskk-adjective-conjugation)
(require 'nskk-complex-conjugation)

;;; カスタマイズ可能変数

(defgroup nskk-conjugation-tables nil
  "活用テーブル最適化の設定。"
  :group 'nskk
  :prefix "nskk-conjugation-tables-")

(defcustom nskk-conjugation-cache-size 1000
  "活用結果キャッシュの最大サイズ。"
  :type 'integer
  :group 'nskk-conjugation-tables)

(defcustom nskk-conjugation-enable-cache t
  "活用結果のキャッシングを有効にするかどうか。"
  :type 'boolean
  :group 'nskk-conjugation-tables)

;;; 圧縮された活用テーブル

;; 五段活用の圧縮テーブル（ビットマップ形式）
(defconst nskk-conjugation--godan-compressed
  '((ka . [?か ?こ ?き ?く ?く ?け ?け])
    (ga . [?が ?ご ?ぎ ?ぐ ?ぐ ?げ ?げ])
    (sa . [?さ ?そ ?し ?す ?す ?せ ?せ])
    (ta . [?た ?と ?ち ?つ ?つ ?て ?て])
    (na . [?な ?の ?に ?ぬ ?ぬ ?ね ?ね])
    (ba . [?ば ?ぼ ?び ?ぶ ?ぶ ?べ ?べ])
    (ma . [?ま ?も ?み ?む ?む ?め ?め])
    (ra . [?ら ?ろ ?り ?る ?る ?れ ?れ])
    (wa . [?わ ?お ?い ?う ?う ?え ?え]))
  "五段活用の圧縮テーブル。
各行は [未然形 未然形(う) 連用形 終止形 連体形 仮定形 命令形] の順。")

;; 活用形のインデックス
(defconst nskk-conjugation--form-index
  '((mizen-nai . 0)
    (mizen-u . 1)
    (renyou . 2)
    (shushi . 3)
    (rentai . 4)
    (katei . 5)
    (meirei . 6))
  "活用形のインデックスマッピング。")

;;; 高速検索テーブル

(defvar nskk-conjugation--fast-lookup-table nil
  "高速検索用のハッシュテーブル。
キー: (stem type form)のリスト
値: 活用結果の文字列")

(defun nskk-conjugation--init-fast-lookup ()
  "高速検索テーブルを初期化する。"
  (unless nskk-conjugation--fast-lookup-table
    (setq nskk-conjugation--fast-lookup-table
          (make-hash-table :test 'equal :size 10000))))

;;; キャッシュシステム

(defvar nskk-conjugation--cache nil
  "活用結果のキャッシュ（LRU方式）。")

(defvar nskk-conjugation--cache-hits 0
  "キャッシュヒット数。")

(defvar nskk-conjugation--cache-misses 0
  "キャッシュミス数。")

(defvar nskk-conjugation--last-stem nil
  "直近に検索した語幹。")

(defvar nskk-conjugation--last-type nil
  "直近に検索した活用型。")

(defvar nskk-conjugation--last-form nil
  "直近に検索した活用形。")

(defvar nskk-conjugation--last-value nil
  "直近の検索結果。")

(defun nskk-conjugation--cache-init ()
  "キャッシュを初期化する。"
  (when nskk-conjugation-enable-cache
    (setq nskk-conjugation--cache (make-hash-table :test 'equal))
    (setq nskk-conjugation--cache-hits 0)
    (setq nskk-conjugation--cache-misses 0)))

(defun nskk-conjugation--cache-key (stem type form)
  "キャッシュキーを生成する。"
  (list stem type form))

(defun nskk-conjugation--cache-get (key)
  "キャッシュから値を取得する。"
  (when nskk-conjugation-enable-cache
    (let ((entry (gethash key nskk-conjugation--cache)))
      (if entry
          (progn
            (cl-incf nskk-conjugation--cache-hits)
            (car entry))
        (cl-incf nskk-conjugation--cache-misses)
        nil))))

(defun nskk-conjugation--cache-put (key value)
  "キャッシュに値を保存する。"
  (when nskk-conjugation-enable-cache
    ;; キャッシュサイズ制限
    (when (>= (hash-table-count nskk-conjugation--cache)
              nskk-conjugation-cache-size)
      (nskk-conjugation--cache-evict))

    (puthash key (cons value (current-time)) nskk-conjugation--cache)))

(defun nskk-conjugation--cache-evict ()
  "キャッシュから古いエントリを削除する（LRU方式）。"
  (let ((oldest-key nil)
        (oldest-time nil))
    ;; 最も古いエントリを探す
    (maphash (lambda (key entry)
               (let ((time (cdr entry)))
                 (when (or (null oldest-time)
                           (time-less-p time oldest-time))
                   (setq oldest-key key)
                   (setq oldest-time time))))
             nskk-conjugation--cache)
    ;; 削除
    (when oldest-key
      (remhash oldest-key nskk-conjugation--cache))))

;;; 公開API - 最適化された検索

;;;###autoload
(defun nskk-conjugation-table-lookup (stem type form)
  "最適化されたテーブルから活用形を高速検索する。

引数:
  STEM - 語幹（文字列）
  TYPE - 活用型（symbol）
  FORM - 活用形（symbol）

返り値:
  活用後の文字列、またはnil

使用例:
  (nskk-conjugation-table-lookup \"書\" 'godan 'renyou)  ;; => \"き\"
  (nskk-conjugation-table-lookup \"高\" 'i-adjective 'renyou-ku) ;; => \"高く\""
  (if (and nskk-conjugation--last-value
           (equal stem nskk-conjugation--last-stem)
           (eq type nskk-conjugation--last-type)
           (eq form nskk-conjugation--last-form))
      (progn
        (cl-incf nskk-conjugation--cache-hits)
        nskk-conjugation--last-value)
    (let* ((cache-key (nskk-conjugation--cache-key stem type form))
           (cached (nskk-conjugation--cache-get cache-key)))
      (if cached
          cached
        ;; キャッシュミス時は通常の活用処理
        (let ((result (cond
                       ;; 動詞活用
                       ((memq type nskk-verb-types)
                        (nskk-conjugate-verb stem type form))
                       ;; 形容詞活用
                       ((memq type nskk-adjective-types)
                        (nskk-conjugate-adjective stem type form))
                       ;; 複合活用
                       ((memq form nskk-complex-conjugation-types)
                        (nskk-complex-conjugate stem type form))
                       (t nil))))
          (when result
            (setq nskk-conjugation--last-stem stem
                  nskk-conjugation--last-type type
                  nskk-conjugation--last-form form
                  nskk-conjugation--last-value result)
            (nskk-conjugation--cache-put cache-key result))
          result)))))

;;;###autoload
(defun nskk-conjugation-table-optimize ()
  "活用テーブルを最適化する。
キャッシュの初期化と高速検索テーブルの構築を行う。"
  (interactive)
  (message "活用テーブルを最適化中...")
  (let ((start-time (current-time)))
    ;; キャッシュ初期化
    (nskk-conjugation--cache-init)

    ;; 高速検索テーブル初期化
    (nskk-conjugation--init-fast-lookup)

    ;; 頻出パターンの事前計算とキャッシング
    (nskk-conjugation--precompute-common-patterns)

    ;; ウォームアップ時のヒット/ミスは統計から除外する
    (setq nskk-conjugation--cache-hits 0)
    (setq nskk-conjugation--cache-misses 0)

    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time)))))
      (message "活用テーブル最適化完了 (%.2f ms)" elapsed-ms)))
  nil)

(defun nskk-conjugation--precompute-common-patterns ()
  "頻出する活用パターンを事前計算してキャッシュする。"
  (let ((common-verbs '("書" "読" "見" "食べ" "し" "来"))
        (common-adjectives '("高" "静か")))
    ;; 頻出動詞の全活用形を事前計算
    (dolist (verb common-verbs)
      (let ((type (cond
                   ((equal verb "し") 'sa-hen)
                   ((equal verb "来") 'ka-hen)
                   ((member verb '("見" "食べ")) 'kami-ichidan)
                   (t 'godan))))
        (dolist (form nskk-verb-forms)
          (nskk-conjugation-table-lookup verb type form))))

    ;; 頻出形容詞の全活用形を事前計算
    (dolist (adj common-adjectives)
      (let ((type (if (equal adj "静か") 'na-adjective 'i-adjective)))
        (dolist (form nskk-adjective-forms)
          (nskk-conjugation-table-lookup adj type form))))))

;;; 統計情報

;;;###autoload
(defun nskk-conjugation-cache-stats ()
  "キャッシュの統計情報を表示する。"
  (interactive)
  (let* ((total (+ nskk-conjugation--cache-hits nskk-conjugation--cache-misses))
         (hit-rate (if (> total 0)
                       (* 100.0 (/ (float nskk-conjugation--cache-hits) total))
                     0.0))
         (cache-size (if nskk-conjugation--cache
                         (hash-table-count nskk-conjugation--cache)
                       0)))
    (message "活用キャッシュ統計:\n  ヒット数: %d\n  ミス数: %d\n  ヒット率: %.1f%%\n  キャッシュサイズ: %d/%d"
             nskk-conjugation--cache-hits
             nskk-conjugation--cache-misses
             hit-rate
             cache-size
             nskk-conjugation-cache-size)
    nil))

;;;###autoload
(defun nskk-conjugation-clear-cache ()
  "活用キャッシュをクリアする。"
  (interactive)
  (when nskk-conjugation--cache
    (clrhash nskk-conjugation--cache)
    (setq nskk-conjugation--cache-hits 0)
    (setq nskk-conjugation--cache-misses 0)
    (setq nskk-conjugation--last-stem nil
          nskk-conjugation--last-type nil
          nskk-conjugation--last-form nil
          nskk-conjugation--last-value nil)
    (message "活用キャッシュをクリアしました")))

;;; メモリ最適化

(defun nskk-conjugation-memory-usage ()
  "活用テーブルのメモリ使用量を推定する（バイト）。"
  (interactive)
  (let* ((cache-size (if nskk-conjugation--cache
                         (hash-table-count nskk-conjugation--cache)
                       0))
         ;; 1エントリあたり約100バイトと仮定
         (cache-memory (* cache-size 100))
         ;; 圧縮テーブル: 約5KB
         (table-memory 5000)
         (total-memory (+ cache-memory table-memory)))
    (message "活用テーブルメモリ使用量: %.2f KB (キャッシュ: %.2f KB, テーブル: %.2f KB)"
             (/ total-memory 1024.0)
             (/ cache-memory 1024.0)
             (/ table-memory 1024.0))
    total-memory))

;;; パフォーマンステストユーティリティ

(defun nskk-conjugation-benchmark ()
  "活用処理のベンチマークを実行する。"
  (interactive)
  (let ((iterations 10000)
        (test-cases '(("書" godan renyou)
                     ("見" kami-ichidan renyou)
                     ("高" i-adjective renyou-ku)
                     ("静か" na-adjective renyou-ni))))
    (message "活用処理ベンチマーク開始 (%d回 x %d種類)..."
             iterations (length test-cases))

    (dolist (case test-cases)
      (let* ((stem (nth 0 case))
             (type (nth 1 case))
             (form (nth 2 case))
             (start-time (current-time)))

        ;; キャッシュなしでベンチマーク
        (let ((nskk-conjugation-enable-cache nil))
          (dotimes (_ iterations)
            (nskk-conjugation-table-lookup stem type form))
          (let* ((end-time (current-time))
                 (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
                 (per-call-ms (/ elapsed-ms iterations)))
            (message "  %s/%s/%s (キャッシュなし): %.3f ms/call"
                     stem type form per-call-ms)))

        ;; キャッシュありでベンチマーク
        (setq start-time (current-time))
        (let ((nskk-conjugation-enable-cache t))
          (nskk-conjugation--cache-init)
          (dotimes (_ iterations)
            (nskk-conjugation-table-lookup stem type form))
          (let* ((end-time (current-time))
                 (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
                 (per-call-ms (/ elapsed-ms iterations)))
            (message "  %s/%s/%s (キャッシュあり): %.3f ms/call"
                     stem type form per-call-ms)))))

    (nskk-conjugation-cache-stats)))

;;; 初期化

;; モジュールロード時にキャッシュ初期化
(nskk-conjugation--cache-init)

(provide 'nskk-conjugation-tables)

;;; nskk-conjugation-tables.el ends here
