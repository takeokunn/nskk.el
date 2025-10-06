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
