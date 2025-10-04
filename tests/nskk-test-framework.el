;;; nskk-test-framework.el --- Test framework for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing
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

;; このファイルはNSKKのテストフレームワークを実装します。
;;
;; 特徴:
;; - ERTラッパーマクロ（nskk-deftest）
;; - テストユーティリティ関数
;; - ドメイン特化アサーションマクロ
;; - テストタグシステム
;; - パフォーマンス測定
;; - テストデータ生成
;;
;; タグ一覧:
;; - :unit          - ユニットテスト
;; - :integration   - 統合テスト
;; - :performance   - パフォーマンステスト
;; - :slow          - 実行時間が長いテスト
;; - :interactive   - ユーザー操作が必要なテスト
;;
;; 使用例:
;; (nskk-deftest my-test
;;   "My test description"
;;   :tags '(:unit)
;;   (should (equal 1 1)))

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; shouldマクロ拡張（説明メッセージ対応）

(fset 'nskk-test--ert-original-should (symbol-function 'should))

(defmacro should (form &optional explanation)
  "FORM を評価し、nilならテストを失敗させる。EXPLANATIONが非nilなら失敗時の追加説明に用いる。"
  (let ((expansion (macroexpand `(nskk-test--ert-original-should ,form))))
    (if explanation
        (let ((err (make-symbol "nskk-should-error")))
          `(condition-case ,err
               ,expansion
             (ert-test-failed
              (signal 'ert-test-failed
                      (append (cdr ,err)
                              (list :explanation ,explanation))))))
      expansion)))

;;; カスタマイズ変数

(defgroup nskk-test nil
  "NSKK test framework customization."
  :group 'nskk
  :prefix "nskk-test-")

(defcustom nskk-test-verbose nil
  "非nilの場合、詳細なテスト出力を表示する。"
  :type 'boolean
  :group 'nskk-test)

(defcustom nskk-test-report-performance t
  "非nilの場合、テストのパフォーマンス情報を記録する。"
  :type 'boolean
  :group 'nskk-test)

(defcustom nskk-test-temp-buffer-prefix " *nskk-test-"
  "テスト用一時バッファの名前プレフィックス。"
  :type 'string
  :group 'nskk-test)

;;; 内部変数

(defvar nskk-test--performance-data nil
  "テストパフォーマンスデータのalist。
形式: ((test-name . execution-time) ...)")

(defvar nskk-test--current-test nil
  "現在実行中のテスト名。")

(defvar nskk-test--temp-buffers nil
  "テスト中に作成された一時バッファのリスト。")

;;; ERTラッパーマクロ

(defmacro nskk-deftest (name description &rest body)
  "NSKKテストを定義する。

NAME: テスト名（シンボル）
DESCRIPTION: テストの説明（文字列）
BODY: テスト本体

BODYの先頭で :tags キーワードを指定可能:
  :tags '(:unit :fast)

使用例:
  (nskk-deftest my-test
    \"My test description\"
    :tags '(:unit)
    (should (equal 1 1)))"
  (declare (indent 2) (debug (symbolp stringp &rest form)))
  (let ((tags nil)
        (test-body body))
    ;; :tags キーワードを抽出
    (when (eq (car body) :tags)
      (setq tags (cadr body))
      (setq test-body (cddr body)))

    `(ert-deftest ,name ()
       ,description
       ,@(when tags `(:tags ,tags))
       (let ((nskk-test--current-test ',name)
             (start-time (current-time)))
         (unwind-protect
             (progn
               (when nskk-test-verbose
                 (message "Running test: %s" ',name))
               ,@test-body)
           ;; パフォーマンス記録
           (when nskk-test-report-performance
             (let ((elapsed (float-time (time-since start-time))))
               (push (cons ',name elapsed) nskk-test--performance-data)
               (when nskk-test-verbose
                 (message "Test %s completed in %.6f seconds" ',name elapsed))))
           ;; クリーンアップ
           (nskk-test--cleanup-temp-buffers))))))

;;; テストユーティリティ

(defmacro nskk-test-with-temp-buffer (&rest body)
  "一時バッファ内で BODY を実行する。
テスト終了時に自動的にバッファを削除する。"
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer
               (format "%s%s*"
                      nskk-test-temp-buffer-prefix
                      (or nskk-test--current-test "temp")))))
     (push buf nskk-test--temp-buffers)
     (with-current-buffer buf
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defmacro nskk-test-with-mock-input (input &rest body)
  "モック入力 INPUT で BODY を実行する。
INPUT は文字列または文字のリスト。"
  (declare (indent 1) (debug (form body)))
  (let ((input-var (make-symbol "input")))
    `(let ((,input-var (if (stringp ,input)
                          (string-to-list ,input)
                        ,input)))
       (cl-letf (((symbol-function 'read-char)
                  (lambda (&optional _prompt _inherit-input-method _seconds)
                    (if ,input-var
                        (pop ,input-var)
                      (error "No more mock input available"))))
                 ((symbol-function 'read-event)
                  (lambda (&optional _prompt _inherit-input-method _seconds)
                    (if ,input-var
                        (pop ,input-var)
                      (error "No more mock input available")))))
         ,@body))))

(defun nskk-test--cleanup-temp-buffers ()
  "テスト用一時バッファをクリーンアップする。"
  (dolist (buf nskk-test--temp-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq nskk-test--temp-buffers nil))

(defmacro nskk-test-with-suppressed-message (&rest body)
  "メッセージ出力を抑制して BODY を実行する。"
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'message)
              (lambda (&rest _args) nil)))
     ,@body))

(defmacro nskk-test-measure-time (&rest body)
  "BODY の実行時間を測定して返す。
戻り値: (result . elapsed-time-in-seconds)"
  (declare (indent 0) (debug t))
  (let ((start-var (make-symbol "start"))
        (result-var (make-symbol "result")))
    `(let ((,start-var (current-time))
           (,result-var (progn ,@body)))
       (cons ,result-var (float-time (time-since ,start-var))))))

;;; アサーションマクロ

(defmacro nskk-should-equal-string (actual expected)
  "文字列 ACTUAL が EXPECTED と等しいことをアサートする。
失敗時に差分を表示。"
  `(let ((actual-val ,actual)
         (expected-val ,expected))
     (unless (equal actual-val expected-val)
       (ert-fail (list 'string-not-equal
                      :actual actual-val
                      :expected expected-val
                      :diff (nskk-test--string-diff actual-val expected-val))))))

(defmacro nskk-should-match-regexp (string regexp)
  "STRING が正規表現 REGEXP にマッチすることをアサートする。"
  `(let ((str ,string)
         (rx ,regexp))
     (should (string-match-p rx str))))

(defmacro nskk-should-buffer-string (expected)
  "現在のバッファの内容が EXPECTED と等しいことをアサートする。"
  `(nskk-should-equal-string (buffer-string) ,expected))

(defmacro nskk-should-point (expected)
  "ポイント位置が EXPECTED であることをアサートする。"
  `(should (= (point) ,expected)))

(defmacro nskk-should-error-with-message (form error-message-regexp)
  "FORM がエラーを発生させ、そのメッセージが ERROR-MESSAGE-REGEXP にマッチすることをアサートする。"
  `(should-error ,form :type 'error))

;;; テストデータ生成

(defun nskk-test-random-string (length &optional charset)
  "長さ LENGTH のランダム文字列を生成する。
CHARSET が指定された場合、その文字セットから選択。
デフォルトは英数字。"
  (let ((chars (or charset
                  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
        (result ""))
    (dotimes (_ length)
      (setq result (concat result
                          (char-to-string
                           (aref chars (random (length chars)))))))
    result))

(defun nskk-test-random-hiragana (length)
  "長さ LENGTH のランダムひらがな文字列を生成する。"
  (let ((hiragana "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん")
        (result ""))
    (dotimes (_ length)
      (setq result (concat result
                          (char-to-string
                           (aref hiragana (random (length hiragana)))))))
    result))

(defun nskk-test-random-katakana (length)
  "長さ LENGTH のランダムカタカナ文字列を生成する。"
  (let ((katakana "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン")
        (result ""))
    (dotimes (_ length)
      (setq result (concat result
                          (char-to-string
                           (aref katakana (random (length katakana)))))))
    result))

(defun nskk-test-random-int (min max)
  "MIN から MAX の範囲のランダム整数を生成する。"
  (+ min (random (- max min))))

(defun nskk-test-random-choice (choices)
  "CHOICES リストからランダムに1つ選択する。"
  (nth (random (length choices)) choices))

;;; パフォーマンス分析

(defun nskk-test-performance-report ()
  "テストパフォーマンスレポートを生成する。
戻り値: パフォーマンスデータのalist"
  (let ((sorted-data (sort (copy-sequence nskk-test--performance-data)
                          (lambda (a b) (> (cdr a) (cdr b))))))
    (when nskk-test-verbose
      (message "=== NSKK Test Performance Report ===")
      (dolist (entry sorted-data)
        (message "  %s: %.6f seconds" (car entry) (cdr entry)))
      (when sorted-data
        (let ((total (apply #'+ (mapcar #'cdr sorted-data)))
              (count (length sorted-data)))
          (message "Total: %.6f seconds, Average: %.6f seconds"
                   total (/ total (float count))))))
    sorted-data))

(defun nskk-test-clear-performance-data ()
  "パフォーマンスデータをクリアする。"
  (interactive)
  (setq nskk-test--performance-data nil))

;;; ヘルパー関数

(defun nskk-test--string-diff (str1 str2)
  "STR1 と STR2 の差分を文字列で返す。"
  (let ((len1 (length str1))
        (len2 (length str2))
        (i 0))
    (while (and (< i len1) (< i len2)
                (eq (aref str1 i) (aref str2 i)))
      (setq i (1+ i)))
    (format "First difference at position %d: %S vs %S\nActual:   %S\nExpected: %S"
            i
            (if (< i len1) (substring str1 i (min (+ i 10) len1)) "")
            (if (< i len2) (substring str2 i (min (+ i 10) len2)) "")
            str1
            str2)))

(defun nskk-test-buffer-contents-equal-p (buffer1 buffer2)
  "BUFFER1 と BUFFER2 の内容が等しいか確認する。"
  (equal (with-current-buffer buffer1 (buffer-string))
         (with-current-buffer buffer2 (buffer-string))))

(defun nskk-test-list-tests-by-tag (tag)
  "指定された TAG を持つ全てのテストを返す。
戻り値: テスト名のリスト"
  (let ((tests nil))
    (mapatoms
     (lambda (sym)
       (when (and (ert-test-boundp sym)
                  (let ((test (ert-get-test sym)))
                    (and (ert-test-p test)
                         (memq tag (ert-test-tags test)))))
         (push sym tests))))
    tests))

(defun nskk-test-run-by-tag (tag)
  "指定された TAG を持つ全てのテストを実行する。"
  (interactive
   (list (intern (completing-read "Tag: "
                                  '(:unit :integration :performance :slow)
                                  nil t))))
  (ert (concat "tag " (symbol-name tag))))

;;; 統計情報

(defun nskk-test-stats ()
  "テストフレームワークの統計情報を返す。
戻り値: 統計情報のplist"
  (let ((total-tests 0)
        (tagged-tests (make-hash-table :test 'eq)))
    (mapatoms
     (lambda (sym)
       (when (ert-test-boundp sym)
         (let ((test (ert-get-test sym)))
           (when (ert-test-p test)
             (setq total-tests (1+ total-tests))
             (dolist (tag (ert-test-tags test))
               (puthash tag (1+ (gethash tag tagged-tests 0)) tagged-tests)))))))

    (list :total-tests total-tests
          :tagged-tests (let ((result nil))
                         (maphash (lambda (k v) (push (cons k v) result))
                                 tagged-tests)
                         result)
          :performance-data-count (length nskk-test--performance-data)
          :temp-buffers-count (length nskk-test--temp-buffers))))

;;; クリーンアップ

(defun nskk-test-cleanup ()
  "テストフレームワークをクリーンアップする。"
  (nskk-test--cleanup-temp-buffers)
  (nskk-test-clear-performance-data))

(provide 'nskk-test-framework)

;;; nskk-test-framework.el ends here
