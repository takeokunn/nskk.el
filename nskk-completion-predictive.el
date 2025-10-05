;;; nskk-completion-predictive.el --- Predictive completion for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, predictive
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

;; このファイルはマルコフ連鎖を利用した予測補完を実装します。
;;
;; 特徴:
;; - マルコフ連鎖による予測モデル
;; - 機械学習準備（将来的な拡張）
;; - 複数の予測アルゴリズム
;; - オンライン学習
;;
;; パフォーマンス目標:
;; - 予測計算: < 5ms
;; - モデル更新: < 3ms
;; - メモリ効率: < 10MB
;;
;; 使用例:
;;
;;   (require 'nskk-completion-predictive)
;;
;;   ;; 予測補完
;;   (nskk-completion-predictive-search index model :context '("これは" "便利"))
;;   ;; => 次に入力されそうな単語を予測
;;
;;   ;; モデル学習
;;   (nskk-completion-predictive-learn model '("これは" "便利" "です"))
;;
;;   ;; 複数候補の予測
;;   (nskk-completion-predictive-predict-next model '("とても") :n 5)

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-completion-predictive nil
  "Predictive completion for NSKK."
  :group 'nskk
  :prefix "nskk-completion-predictive-")

(defcustom nskk-completion-predictive-default-limit 15
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-predictive)

(defcustom nskk-completion-predictive-order 2
  "マルコフ連鎖の次数（1または2）。
  - 1: 1次マルコフ連鎖（直前の1単語のみ考慮）
  - 2: 2次マルコフ連鎖（直前の2単語を考慮）"
  :type '(choice (const :tag "First-order" 1)
                 (const :tag "Second-order" 2))
  :group 'nskk-completion-predictive)

(defcustom nskk-completion-predictive-min-confidence 0.01
  "予測に必要な最小信頼度（0.0-1.0）。"
  :type 'float
  :group 'nskk-completion-predictive)

(defcustom nskk-completion-predictive-learning-rate 1.0
  "学習率（0.0-1.0）。
オンライン学習時の重み更新の強さ。"
  :type 'float
  :group 'nskk-completion-predictive)

;;; データ構造

(cl-defstruct (nskk-completion-predictive-model
               (:constructor nskk-completion-predictive-model--create)
               (:copier nil))
  "予測モデル構造。

スロット:
  transition-table - 遷移確率テーブル（state → ((next-word . probability) ...)）
  state-count      - 状態出現回数（state → count）
  order            - マルコフ連鎖の次数
  total-transitions - 総遷移数"
  (transition-table nil :type hash-table)
  (state-count nil :type hash-table)
  (order 1 :type integer)
  (total-transitions 0 :type integer))

(cl-defstruct (nskk-completion-predictive-result
               (:constructor nskk-completion-predictive-result--create)
               (:copier nil))
  "予測補完結果。

スロット:
  midashi     - 見出し語
  entry       - 辞書エントリ
  probability - 予測確率
  confidence  - 信頼度
  rank        - ランク"
  (midashi nil :type string)
  (entry nil :type (or null nskk-dict-entry))
  (probability 0.0 :type float)
  (confidence 0.0 :type float)
  (rank 0 :type integer))

;;; モデルの初期化

;;;###autoload
(defun nskk-completion-predictive-model-create (&optional order)
  "空の予測モデルを作成する。

引数:
  ORDER - マルコフ連鎖の次数（省略時はnskk-completion-predictive-order）

戻り値:
  nskk-completion-predictive-model構造体"
  (let ((model-order (or order nskk-completion-predictive-order)))
    (nskk-completion-predictive-model--create
     :transition-table (make-hash-table :test 'equal :size 10000)
     :state-count (make-hash-table :test 'equal :size 10000)
     :order model-order
     :total-transitions 0)))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-predictive-search (index model &rest args)
  "予測補完検索を実行する。

引数:
  INDEX - nskk-dict-index構造体
  MODEL - 予測モデル

キーワード引数:
  :context    - 文脈（直前の単語のリスト）
  :limit      - 最大結果数
  :okuri-type - 送り仮名タイプ

戻り値:
  nskk-completion-predictive-result構造体のリスト（確率順）"
  (let* ((context (plist-get args :context))
         (limit (or (plist-get args :limit)
                   nskk-completion-predictive-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (predictions (nskk-completion-predictive-predict-next
                      model context :n limit)))

    ;; 予測結果を辞書エントリと結合
    (let ((results nil)
          (rank 1))
      (dolist (pred predictions)
        (let* ((word (car pred))
               (prob (cdr pred))
               (entry (nskk-dict-struct-lookup index word okuri-type)))
          (when entry
            (push (nskk-completion-predictive-result--create
                   :midashi word
                   :entry entry
                   :probability prob
                   :confidence (nskk-completion-predictive--calculate-confidence
                               model context word)
                   :rank rank)
                  results)
            (setq rank (1+ rank)))))

      (nreverse results))))

;;; 予測機能

;;;###autoload
(defun nskk-completion-predictive-predict-next (model context &rest args)
  "次の単語を予測する。

引数:
  MODEL   - 予測モデル
  CONTEXT - 文脈（直前の単語のリスト）

キーワード引数:
  :n - 予測候補数（デフォルト: nskk-completion-predictive-default-limit）

戻り値:
  ((word . probability) ...) のリスト（確率の高い順）"
  (unless model
    (error "Predictive model is required"))

  (let* ((n (or (plist-get args :n) nskk-completion-predictive-default-limit))
         (order (nskk-completion-predictive-model-order model))
         (state (nskk-completion-predictive--make-state context order))
         (transitions (gethash state
                              (nskk-completion-predictive-model-transition-table model)))
         (predictions nil))

    (if (null transitions)
        ;; 遷移データがない場合は空リスト
        nil
      ;; 確率順にソート
      (setq predictions (sort (copy-sequence transitions)
                             (lambda (a b) (> (cdr a) (cdr b)))))

      ;; 最小信頼度でフィルタ
      (setq predictions (seq-filter
                        (lambda (pred)
                          (>= (cdr pred) nskk-completion-predictive-min-confidence))
                        predictions))

      ;; 上位N件を返す
      (seq-take predictions n))))

(defun nskk-completion-predictive--make-state (context order)
  "文脈から状態を作成する（内部関数）。

引数:
  CONTEXT - 文脈（直前の単語のリスト、新しい順）
  ORDER   - マルコフ連鎖の次数

戻り値:
  状態（文字列またはリスト）"
  (cond
   ((= order 1)
    ;; 1次マルコフ連鎖: 直前の1単語
    (if context (car context) ""))
   ((= order 2)
    ;; 2次マルコフ連鎖: 直前の2単語
    (if (>= (length context) 2)
        (list (nth 1 context) (nth 0 context))
      (if context (list (car context)) (list ""))))
   (t
    "")))

(defun nskk-completion-predictive--calculate-confidence (model context word)
  "予測の信頼度を計算する（内部関数）。

引数:
  MODEL   - 予測モデル
  CONTEXT - 文脈
  WORD    - 予測単語

処理:
  状態の出現回数に基づく信頼度
  confidence = min(1.0, state-count / 100)

戻り値:
  信頼度（0.0-1.0）"
  (let* ((order (nskk-completion-predictive-model-order model))
         (state (nskk-completion-predictive--make-state context order))
         (count (gethash state (nskk-completion-predictive-model-state-count model) 0)))
    (min 1.0 (/ (float count) 100.0))))

;;; 学習機能

;;;###autoload
(defun nskk-completion-predictive-learn (model word-sequence)
  "予測モデルを更新する（学習）。

引数:
  MODEL         - 予測モデル
  WORD-SEQUENCE - 単語シーケンス（リスト）

処理:
  マルコフ連鎖の遷移確率を更新

戻り値:
  更新された予測モデル"
  (unless (listp word-sequence)
    (error "Word sequence must be a list: %s" word-sequence))

  (let ((order (nskk-completion-predictive-model-order model))
        (seq-len (length word-sequence)))

    (when (> seq-len order)
      ;; 各遷移を学習
      (dotimes (i (- seq-len order))
        (let* ((context-words (if (= order 1)
                                 (list (nth i word-sequence))
                               (list (nth i word-sequence) (nth (1+ i) word-sequence))))
               (next-word (nth (+ i order) word-sequence))
               (state (nskk-completion-predictive--make-state
                      (nreverse context-words) order)))

          ;; 状態カウント更新
          (let ((count (gethash state (nskk-completion-predictive-model-state-count model) 0)))
            (puthash state (1+ count) (nskk-completion-predictive-model-state-count model)))

          ;; 遷移テーブル更新
          (nskk-completion-predictive--update-transition
           model state next-word))))

    ;; 確率の正規化
    (nskk-completion-predictive--normalize-probabilities model)

    model))

(defun nskk-completion-predictive--update-transition (model state next-word)
  "遷移テーブルを更新する（内部関数）。

引数:
  MODEL     - 予測モデル
  STATE     - 状態
  NEXT-WORD - 次の単語"
  (let* ((transitions (gethash state (nskk-completion-predictive-model-transition-table model)))
         (current-count (or (cdr (assoc next-word transitions)) 0))
         (new-count (+ current-count nskk-completion-predictive-learning-rate)))

    ;; 遷移リストを更新
    (if transitions
        (let ((updated nil))
          (setq transitions
                (mapcar (lambda (pair)
                         (if (equal (car pair) next-word)
                             (progn
                               (setq updated t)
                               (cons next-word new-count))
                           pair))
                       transitions))
          (unless updated
            (push (cons next-word new-count) transitions)))
      (setq transitions (list (cons next-word new-count))))

    (puthash state transitions (nskk-completion-predictive-model-transition-table model))
    (cl-incf (nskk-completion-predictive-model-total-transitions model))))

(defun nskk-completion-predictive--normalize-probabilities (model)
  "遷移確率を正規化する（内部関数）。

引数:
  MODEL - 予測モデル

処理:
  各状態の遷移確率の合計が1.0になるように正規化"
  (maphash
   (lambda (state transitions)
     (let ((total (apply #'+ (mapcar #'cdr transitions))))
       (when (> total 0)
         (let ((normalized (mapcar (lambda (pair)
                                    (cons (car pair)
                                          (/ (cdr pair) (float total))))
                                  transitions)))
           (puthash state normalized
                   (nskk-completion-predictive-model-transition-table model))))))
   (nskk-completion-predictive-model-transition-table model)))

;;; 永続化

;;;###autoload
(defun nskk-completion-predictive-save-model (model file-path)
  "予測モデルをファイルに保存する。

引数:
  MODEL     - 予測モデル
  FILE-PATH - 保存先ファイルパス"
  (let ((data (list :version "1.0"
                   :order (nskk-completion-predictive-model-order model)
                   :total-transitions (nskk-completion-predictive-model-total-transitions model)
                   :states (nskk-completion-predictive--hash-to-alist
                           (nskk-completion-predictive-model-state-count model))
                   :transitions (nskk-completion-predictive--hash-to-alist
                                (nskk-completion-predictive-model-transition-table model)))))
    (with-temp-file file-path
      (insert ";; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
      (insert ";; NSKK 予測モデルデータ\n\n")
      (prin1 data (current-buffer)))))

;;;###autoload
(defun nskk-completion-predictive-load-model (file-path)
  "ファイルから予測モデルを読み込む。

引数:
  FILE-PATH - 読み込むファイルパス

戻り値:
  予測モデル"
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (while (looking-at ";;")
      (forward-line 1))
    (let* ((data (read (current-buffer)))
           (version (plist-get data :version))
           (order (plist-get data :order))
           (model (nskk-completion-predictive-model-create order)))
      (unless (equal version "1.0")
        (error "Unsupported predictive model version: %s" version))

      ;; データを復元
      (setf (nskk-completion-predictive-model-total-transitions model)
            (plist-get data :total-transitions))
      (nskk-completion-predictive--alist-to-hash
       (plist-get data :states)
       (nskk-completion-predictive-model-state-count model))
      (nskk-completion-predictive--alist-to-hash
       (plist-get data :transitions)
       (nskk-completion-predictive-model-transition-table model))

      model)))

(defun nskk-completion-predictive--hash-to-alist (hash-table)
  "ハッシュテーブルをalistに変換する（内部関数）。"
  (let ((alist nil))
    (maphash (lambda (k v) (push (cons k v) alist)) hash-table)
    alist))

(defun nskk-completion-predictive--alist-to-hash (alist hash-table)
  "alistをハッシュテーブルに変換する（内部関数）。"
  (dolist (pair alist)
    (puthash (car pair) (cdr pair) hash-table)))

;;; ユーティリティ関数

(defun nskk-completion-predictive-to-simple-list (results)
  "予測補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-predictive-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-predictive-result-midashi result)
                 (nskk-completion-predictive-result-entry result)))
          results))

(defun nskk-completion-predictive-statistics (model)
  "予測モデルの統計情報を取得する。

引数:
  MODEL - 予測モデル

戻り値:
  plist形式の統計情報"
  (let ((state-count (hash-table-count (nskk-completion-predictive-model-state-count model)))
        (total-transitions (nskk-completion-predictive-model-total-transitions model)))
    (list :order (nskk-completion-predictive-model-order model)
          :state-count state-count
          :total-transitions total-transitions
          :avg-transitions-per-state (if (> state-count 0)
                                        (/ (float total-transitions) state-count)
                                      0.0))))

;;; デバッグ用関数

(defun nskk-completion-predictive-debug-info (model context)
  "予測補完のデバッグ情報を表示する。

引数:
  MODEL   - 予測モデル
  CONTEXT - 文脈

戻り値:
  plist形式のデバッグ情報"
  (let* ((start-time (float-time))
         (predictions (nskk-completion-predictive-predict-next model context))
         (elapsed-time (- (float-time) start-time)))
    (list :context context
          :prediction-count (length predictions)
          :elapsed-time elapsed-time
          :order (nskk-completion-predictive-model-order model)
          :predictions (seq-take predictions 10))))

(provide 'nskk-completion-predictive)

;;; nskk-completion-predictive.el ends here
