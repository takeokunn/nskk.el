;;; nskk-input-commands.el --- Input command wrappers for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
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

;; このファイルはNSKKの入力コマンド群を実装します。
;;
;; キーマップから呼び出される入力操作コマンドのラッパー関数を提供し、
;; 内部的にnskk-buffer.elの機能を使用します。

;;; Code:

(require 'nskk-buffer)
(require 'nskk-state)

;;; 句読点設定

(defgroup nskk-input-punctuation nil
  "ddskk互換の句読点入力設定。"
  :group 'nskk
  :prefix "nskk-input-")

(defcustom nskk-input-kuten-touten-alist
  '((jp . ("。" . "、"))
    (en . ("." . ","))
    (jp-en . ("。" . ","))
    (en-jp . ("." . "、")))
  "句読点のスタイルを定義する連想リスト。
`nskk-input-kutouten-type'で指すシンボルに対応する句読点ペアを返す。"
  :type '(repeat (cons (choice :tag "スタイル"
                               (const :tag "日本語" jp)
                               (const :tag "英語" en)
                               (const :tag "句点:。／読点: ," jp-en)
                               (const :tag "句点:.／読点: 、" en-jp)
                               (cons :tag "カスタム" (string :tag "句点") (string :tag "読点")))
                       (cons :tag "句読点ペア"
                             (string :tag "句点" "。")
                             (string :tag "読点" "、"))))
  :group 'nskk-input-punctuation)

(defcustom nskk-input-kutouten-type 'jp
  "現在の句読点スタイル。`
nskk-input-toggle-kuten'でサイクルする。"
  :type '(choice (const jp)
                 (const en)
                 (const jp-en)
                 (const en-jp)
                 (cons (string :tag "句点") (string :tag "読点")))
  :group 'nskk-input-punctuation)

(defcustom nskk-input-use-auto-kutouten t
  "前の文字が数字の場合に句点・読点を自動切り替えするかどうか。"
  :type 'boolean
  :group 'nskk-input-punctuation)

(defconst nskk-input--kutouten-cycle '(jp en jp-en en-jp)
  "`nskk-input-toggle-kuten'が巡回するスタイル順。")

(defun nskk-input--resolve-kutouten ()
  "現在の句読点ペアを返す。"
  (let ((entry (assq nskk-input-kutouten-type nskk-input-kuten-touten-alist)))
    (cond
     (entry (cdr entry))
     ((consp nskk-input-kutouten-type) nskk-input-kutouten-type)
     (t (cdr (assq 'jp nskk-input-kuten-touten-alist))))))

(defun nskk-input--current-kuten ()
  "現在の句点文字列を返す。"
  (car (nskk-input--resolve-kutouten)))

(defun nskk-input--current-touten ()
  "現在の読点文字列を返す。"
  (cdr (nskk-input--resolve-kutouten)))

(defun nskk-input--fullwidth-string-p (str)
  "STRがASCII以外の文字を含むか判定する。"
  (and str (string-match-p "[^[:ascii:]]" str)))

(defun nskk-input--current-script ()
  "現在の入力モードに基づくスクリプト種別を返す。"
  (pcase (and nskk-current-state (nskk-state-mode nskk-current-state))
    ((or 'latin 'abbrev) 'ascii)
    ('zenkaku-latin 'fullwidth)
    (_ 'kana)))

(defun nskk-input--auto-kutouten (char)
  "CHARに対応する句読点文字列を返す。"
  (let* ((choices (pcase char
                    (?. (list "." "．" (nskk-input--current-kuten)))
                    (?, (list "," "，" (nskk-input--current-touten)))
                    (_ (error "Unsupported kutouten char: %s" char))))
         (script (nskk-input--current-script)))
    (pcase script
      ('ascii (nth 0 choices))
      ('fullwidth (nth 1 choices))
      (_
       (let ((prev (char-before (point))))
         (cond
          ((null prev) (nth 2 choices))
          ((and nskk-input-use-auto-kutouten
                (<= ?0 prev) (<= prev ?9)) (nth 0 choices))
          ((and nskk-input-use-auto-kutouten
                (<= ?０ prev) (<= prev ?９)) (nth 1 choices))
          (t (nth 2 choices))))))))

(defun nskk-input--japanese-punctuation-active-p ()
  "句点スタイルが日本語ベースか判定する。"
  (nskk-input--fullwidth-string-p (nskk-input--current-kuten)))

(defun nskk-input--general-punctuation (ascii fullwidth)
  "現在のモードに合わせてASCIIか全角記号を選択する。"
  (pcase (nskk-input--current-script)
    ('ascii ascii)
    ('fullwidth fullwidth)
    (_ (if (nskk-input--japanese-punctuation-active-p) fullwidth ascii))))

(defun nskk-input--insert-string (text)
  "TEXTをNSKKバッファに挿入する。"
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (nskk-buffer-insert text))

;;;###autoload
(defun nskk-input-toggle-kuten ()
  "句読点のスタイルをトグルで切り替える。"
  (interactive)
  (setq nskk-input-kutouten-type
        (or (cadr (member nskk-input-kutouten-type nskk-input--kutouten-cycle))
            (car nskk-input--kutouten-cycle)))
  (when (called-interactively-p 'interactive)
    (message "読点: %s  句点: %s"
             (nskk-input--current-touten)
             (nskk-input--current-kuten))))

;;;###autoload
(defun nskk-input-insert-period ()
  "句点を挿入する。"
  (interactive)
  (nskk-input--insert-string (nskk-input--auto-kutouten ?.)))

;;;###autoload
(defun nskk-input-insert-comma ()
  "読点を挿入する。"
  (interactive)
  (nskk-input--insert-string (nskk-input--auto-kutouten ?,)))

;;;###autoload
(defun nskk-input-insert-question ()
  "疑問符を挿入する。"
  (interactive)
  (nskk-input--insert-string (nskk-input--general-punctuation "?" "？")))

;;;###autoload
(defun nskk-input-insert-exclamation ()
  "感嘆符を挿入する。"
  (interactive)
  (nskk-input--insert-string (nskk-input--general-punctuation "!" "！")))

;;; 削除コマンド

(defun nskk-input-delete-backward-char (&optional n)
  "後退削除を行う。N 文字削除する。
キーマップから呼び出される入力操作コマンド。"
  (interactive "p")
  (nskk-buffer-delete-backward-char n))

(defun nskk-input-delete-forward-char (&optional n)
  "前方削除を行う。N 文字削除する。
キーマップから呼び出される入力操作コマンド。"
  (interactive "p")
  (nskk-buffer-delete-forward-char n))

(defun nskk-input-kill-line ()
  "カーソル位置から行末までを削除する。
未確定入力がある場合は、そのコンテキスト内でのみ削除を行う。"
  (interactive)
  (when (and nskk-current-state
             (nskk-state-marker-start nskk-current-state)
             (nskk-state-marker-end nskk-current-state))
    (let* ((current (point))
           (start (marker-position (nskk-state-marker-start nskk-current-state)))
           (end (marker-position (nskk-state-marker-end nskk-current-state))))
      (when (and (>= current start) (< current end))
        (nskk-buffer-delete-region current end)))))

(defun nskk-input-kill-whole-line ()
  "未確定入力を全てクリアする。
キーマップから呼び出される入力操作コマンド。"
  (interactive)
  (nskk-buffer-clear))

(defun nskk-input-kill-region ()
  "リージョンを削除する。
ただし、NSKKの未確定入力範囲内のみで動作する。"
  (interactive)
  (when (and nskk-current-state
             (nskk-state-marker-start nskk-current-state)
             (nskk-state-marker-end nskk-current-state)
             (use-region-p))
    (let* ((region-start (region-beginning))
           (region-end (region-end))
           (start (marker-position (nskk-state-marker-start nskk-current-state)))
           (end (marker-position (nskk-state-marker-end nskk-current-state))))
      ;; NSKK入力範囲内のリージョンのみ削除
      (when (and (>= region-start start)
                 (<= region-end end))
        (nskk-buffer-delete-region region-start region-end)
        (deactivate-mark)))))

(defun nskk-input-yank ()
  "キルリングからテキストをヤンクする。
NSKKの未確定入力に挿入する。"
  (interactive)
  (let ((text (current-kill 0 t)))
    (when text
      (nskk-buffer-insert text))))

;;; Obsolete Functions

;; ddskk完全互換のため、以下の関数は使用されなくなった
;; 標準Emacsコマンドを使用すること

(make-obsolete 'nskk-input-delete-forward-char 'delete-char "1.0.0")
(make-obsolete 'nskk-input-kill-line 'kill-line "1.0.0")
(make-obsolete 'nskk-input-kill-whole-line nil "1.0.0")
(make-obsolete 'nskk-input-kill-region 'kill-region "1.0.0")
(make-obsolete 'nskk-input-yank 'yank "1.0.0")

;; nskk-input-delete-backward-charは将来的なエミュレーション機構で使用する可能性があるため保持

(provide 'nskk-input-commands)

;;; nskk-input-commands.el ends here
