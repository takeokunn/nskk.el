;;; nskk-history.el --- History management system for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, history
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

;; このファイルは履歴管理システムを実装します。
;;
;; 特徴:
;; - 変換履歴の記録
;; - 統計情報収集
;; - プライバシー保護
;; - データ匿名化
;;
;; 記録される情報:
;; - 見出し語と変換候補
;; - 変換時刻
;; - 変換にかかった時間
;; - 候補選択回数
;;
;; プライバシー保護:
;; - 個人情報の自動検出と匿名化
;; - 履歴の自動削除（期間設定可能）
;; - オプトアウト機能
;;
;; 使用例:
;; (nskk-record-history entry)
;; (nskk-history-statistics)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-history nil
  "History management system for NSKK."
  :group 'nskk
  :prefix "nskk-history-")

(defcustom nskk-history-enabled t
  "非nilの場合、履歴記録を有効にする。"
  :type 'boolean
  :group 'nskk-history)

(defcustom nskk-history-anonymize t
  "非nilの場合、個人情報を自動的に匿名化する。"
  :type 'boolean
  :group 'nskk-history)

(defcustom nskk-history-max-entries 10000
  "履歴の最大保存件数。"
  :type 'integer
  :group 'nskk-history)

(defcustom nskk-history-retention-days 30
  "履歴の保持期間（日数）。
この期間を過ぎた履歴は自動的に削除される。"
  :type 'integer
  :group 'nskk-history)

(defcustom nskk-history-collect-timing t
  "非nilの場合、変換処理時間を記録する。"
  :type 'boolean
  :group 'nskk-history)

(defcustom nskk-history-anonymize-patterns
  '("\\([0-9]\\{3\\}-[0-9]\\{4\\}-[0-9]\\{4\\}\\)"  ; 電話番号
    "\\([0-9]\\{7\\}\\)"                             ; 郵便番号
    "\\([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\)"  ; メール
    "\\([0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\)"   ; 日付
    "\\([0-9]\\{13,16\\}\\)")                        ; クレジットカード番号
  "匿名化対象パターンのリスト（正規表現）。"
  :type '(repeat regexp)
  :group 'nskk-history)

;;; データ構造

(cl-defstruct (nskk-history-entry
               (:constructor nskk-history-entry--create)
               (:copier nil))
  "履歴エントリ。

スロット:
  midashi          - 見出し語
  candidate        - 選択された候補
  timestamp        - 変換時刻
  conversion-time  - 変換にかかった時間（秒）
  candidate-index  - 選択した候補のインデックス
  total-candidates - 候補の総数
  okuri-ari        - 送り仮名ありフラグ
  anonymized       - 匿名化フラグ"
  (midashi nil :type string)
  (candidate nil :type string)
  (timestamp nil :type list)
  (conversion-time nil :type (or null float))
  (candidate-index 0 :type integer)
  (total-candidates 1 :type integer)
  (okuri-ari nil :type boolean)
  (anonymized nil :type boolean))

;;; グローバル変数

(defvar nskk-history-entries nil
  "履歴エントリのリスト。
最新のものが先頭。")

(defvar nskk-history-last-cleanup-time (current-time)
  "最後にクリーンアップを実行した時刻。")

;;; 履歴記録

;;;###autoload
(defun nskk-record-history (entry)
  "履歴エントリ ENTRY を記録する。
ENTRY は plist 形式: (:midashi :candidate :candidate-index :total-candidates ...)"
  (unless nskk-history-enabled
    (return))

  (unless (plistp entry)
    (error "Entry must be a plist"))

  (let* ((midashi (plist-get entry :midashi))
         (candidate (plist-get entry :candidate))
         (conversion-time (plist-get entry :conversion-time))
         (candidate-index (or (plist-get entry :candidate-index) 0))
         (total-candidates (or (plist-get entry :total-candidates) 1))
         (okuri-ari (plist-get entry :okuri-ari))
         (anonymized nil))

    (unless (and midashi candidate)
      (error "Entry must contain :midashi and :candidate"))

    ;; 匿名化処理
    (when nskk-history-anonymize
      (let ((anonymized-candidate (nskk-history--anonymize candidate)))
        (when (not (equal candidate anonymized-candidate))
          (setq candidate anonymized-candidate)
          (setq anonymized t))))

    ;; 履歴エントリを作成
    (let ((hist-entry (nskk-history-entry--create
                       :midashi midashi
                       :candidate candidate
                       :timestamp (current-time)
                       :conversion-time conversion-time
                       :candidate-index candidate-index
                       :total-candidates total-candidates
                       :okuri-ari okuri-ari
                       :anonymized anonymized)))

      ;; 履歴に追加（先頭に挿入）
      (push hist-entry nskk-history-entries)

      ;; エントリ数制限
      (when (> (length nskk-history-entries) nskk-history-max-entries)
        (setq nskk-history-entries
              (seq-take nskk-history-entries nskk-history-max-entries)))

      ;; 定期的なクリーンアップ
      (nskk-history--maybe-cleanup))))

;;; 匿名化

(defun nskk-history--anonymize (text)
  "TEXT 内の個人情報を匿名化する。
戻り値: 匿名化されたテキスト"
  (let ((result text))
    (dolist (pattern nskk-history-anonymize-patterns)
      (setq result (replace-regexp-in-string
                    pattern
                    (lambda (match)
                      (make-string (length match) ?*))
                    result)))
    result))

(defun nskk-history--contains-personal-info-p (text)
  "TEXT が個人情報を含むか判定する。"
  (cl-some (lambda (pattern)
            (string-match-p pattern text))
          nskk-history-anonymize-patterns))

;;; クリーンアップ

(defun nskk-history--maybe-cleanup ()
  "必要に応じて古い履歴を削除する。"
  (when (> (float-time (time-since nskk-history-last-cleanup-time))
           86400)  ; 1日ごと
    (nskk-history--cleanup-old-entries)
    (setq nskk-history-last-cleanup-time (current-time))))

(defconst nskk-history--cleanup-grace-seconds 1.0
  "クリーンアップ時に最新エントリを保持するための猶予秒数。")

(defun nskk-history--cleanup-old-entries ()
  "保持期間を過ぎた履歴エントリを削除する。"
  (let* ((cutoff-time (time-subtract (current-time)
                                     (days-to-time nskk-history-retention-days)))
         (cutoff-float (float-time cutoff-time)))
    (setq nskk-history-entries
          (cl-remove-if
           (lambda (entry)
             (< (+ (float-time (nskk-history-entry-timestamp entry))
                   nskk-history--cleanup-grace-seconds)
                cutoff-float))
           nskk-history-entries))))

;;; 統計情報

;;;###autoload
(defun nskk-history-statistics ()
  "履歴の統計情報を返す。
戻り値: plist形式の統計情報"
  (let ((total-entries (length nskk-history-entries))
        (okuri-ari-count 0)
        (okuri-nasi-count 0)
        (anonymized-count 0)
        (total-conversion-time 0.0)
        (first-choice-count 0)
        (avg-candidate-index 0.0)
        (unique-midashi (make-hash-table :test 'equal))
        (unique-candidates (make-hash-table :test 'equal)))

    ;; 統計収集
    (dolist (entry nskk-history-entries)
      (when (nskk-history-entry-okuri-ari entry)
        (cl-incf okuri-ari-count))
      (unless (nskk-history-entry-okuri-ari entry)
        (cl-incf okuri-nasi-count))
      (when (nskk-history-entry-anonymized entry)
        (cl-incf anonymized-count))
      (when (nskk-history-entry-conversion-time entry)
        (cl-incf total-conversion-time
                (nskk-history-entry-conversion-time entry)))
      (when (= (nskk-history-entry-candidate-index entry) 0)
        (cl-incf first-choice-count))
      (puthash (nskk-history-entry-midashi entry) t unique-midashi)
      (puthash (nskk-history-entry-candidate entry) t unique-candidates))

    ;; 平均候補インデックス
    (when (> total-entries 0)
      (setq avg-candidate-index
            (/ (float (apply #'+ (mapcar #'nskk-history-entry-candidate-index
                                        nskk-history-entries)))
               total-entries)))

    (list :total-entries total-entries
          :okuri-ari-count okuri-ari-count
          :okuri-nasi-count okuri-nasi-count
          :anonymized-count anonymized-count
          :unique-midashi (hash-table-count unique-midashi)
          :unique-candidates (hash-table-count unique-candidates)
          :total-conversion-time total-conversion-time
          :avg-conversion-time (if (> total-entries 0)
                                   (/ total-conversion-time total-entries)
                                 0.0)
          :first-choice-rate (if (> total-entries 0)
                                 (* 100.0 (/ (float first-choice-count)
                                           total-entries))
                               0.0)
          :avg-candidate-index avg-candidate-index)))

;;;###autoload
(defun nskk-history-print-statistics ()
  "履歴の統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-history-statistics)))
    (message "NSKK History Statistics:
  Total Entries: %d
  Okuri-ari: %d, Okuri-nasi: %d
  Anonymized: %d
  Unique Midashi: %d
  Unique Candidates: %d
  Total Conversion Time: %.2f seconds
  Average Conversion Time: %.4f seconds
  First Choice Rate: %.1f%%
  Average Candidate Index: %.2f"
             (plist-get stats :total-entries)
             (plist-get stats :okuri-ari-count)
             (plist-get stats :okuri-nasi-count)
             (plist-get stats :anonymized-count)
             (plist-get stats :unique-midashi)
             (plist-get stats :unique-candidates)
             (plist-get stats :total-conversion-time)
             (plist-get stats :avg-conversion-time)
             (plist-get stats :first-choice-rate)
             (plist-get stats :avg-candidate-index))))

;;; 検索・フィルター

;;;###autoload
(defun nskk-history-search (midashi)
  "見出し語 MIDASHI の履歴エントリを検索する。
戻り値: マッチした履歴エントリのリスト"
  (cl-remove-if-not
   (lambda (entry)
     (equal (nskk-history-entry-midashi entry) midashi))
   nskk-history-entries))

;;;###autoload
(defun nskk-history-recent (n)
  "最近の N 件の履歴エントリを取得する。"
  (seq-take nskk-history-entries (min n (length nskk-history-entries))))

;;;###autoload
(defun nskk-history-by-date (start-date end-date)
  "START-DATE から END-DATE の間の履歴エントリを取得する。
日付は Emacs time 形式。"
  (cl-remove-if-not
   (lambda (entry)
     (let ((ts (nskk-history-entry-timestamp entry)))
       (and (not (time-less-p ts start-date))
            (not (time-less-p end-date ts)))))
   nskk-history-entries))

;;; 分析機能

;;;###autoload
(defun nskk-history-most-used-words (&optional n)
  "最も使用された単語の上位 N 件（デフォルト10件）を返す。
戻り値: ((candidate . count) ...) のリスト"
  (let ((word-counts (make-hash-table :test 'equal)))
    ;; カウント集計
    (dolist (entry nskk-history-entries)
      (let ((candidate (nskk-history-entry-candidate entry)))
        (puthash candidate
                (1+ (gethash candidate word-counts 0))
                word-counts)))

    ;; リストに変換してソート
    (let ((result nil))
      (maphash (lambda (word count) (push (cons word count) result))
              word-counts)
      (seq-take (sort result (lambda (a b) (> (cdr a) (cdr b))))
                (or n 10)))))

;;;###autoload
(defun nskk-history-conversion-time-analysis ()
  "変換時間の分析結果を返す。
戻り値: plist形式の分析結果"
  (let ((times nil))
    ;; 変換時間のリストを作成
    (dolist (entry nskk-history-entries)
      (when (nskk-history-entry-conversion-time entry)
        (push (nskk-history-entry-conversion-time entry) times)))

    (if (null times)
        (list :count 0 :min 0.0 :max 0.0 :avg 0.0 :median 0.0)
      (let* ((sorted-times (sort times #'<))
             (count (length sorted-times))
             (min-time (car sorted-times))
             (max-time (car (last sorted-times)))
             (avg-time (/ (apply #'+ sorted-times) (float count)))
             (median-time (nth (/ count 2) sorted-times)))
        (list :count count
              :min min-time
              :max max-time
              :avg (/ (float (round (* avg-time 1000))) 1000.0)
              :median median-time)))))

;;; クリーンアップ

;;;###autoload
(defun nskk-history-clear ()
  "全ての履歴をクリアする。"
  (interactive)
  (when (yes-or-no-p "Clear all history entries? ")
    (setq nskk-history-entries nil)
    (message "History cleared.")))

;;;###autoload
(defun nskk-history-export (file-path)
  "履歴を FILE-PATH にエクスポートする。
CSV形式で出力。"
  (interactive "FExport history to: ")
  (with-temp-file file-path
    (insert "Midashi,Candidate,Timestamp,ConversionTime,CandidateIndex,TotalCandidates,OkuriAri,Anonymized\n")
    (dolist (entry nskk-history-entries)
      (insert (format "%s,%s,%s,%.6f,%d,%d,%s,%s\n"
                     (nskk-history-entry-midashi entry)
                     (nskk-history-entry-candidate entry)
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                        (nskk-history-entry-timestamp entry))
                     (or (nskk-history-entry-conversion-time entry) 0.0)
                     (nskk-history-entry-candidate-index entry)
                     (nskk-history-entry-total-candidates entry)
                     (if (nskk-history-entry-okuri-ari entry) "true" "false")
                     (if (nskk-history-entry-anonymized entry) "true" "false")))))
  (message "History exported to %s" file-path))

(provide 'nskk-history)

;;; nskk-history.el ends here
