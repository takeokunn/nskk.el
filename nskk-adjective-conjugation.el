;;; nskk-adjective-conjugation.el --- Japanese adjective conjugation engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, conjugation
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

;; このファイルはNSKKの形容詞活用エンジンを実装します。
;;
;; 対応する活用:
;; - イ形容詞活用（高い→高く、高かっ、高けれ）
;; - ナ形容詞活用（静か→静かに、静かだ、静かで）
;; - 形容動詞活用（きれい→きれいに、きれいだ）
;;
;; 活用形:
;; - 未然形（なかろ）
;; - 連用形（く/に、かっ/だっ）
;; - 終止形（い/だ）
;; - 連体形（い/な）
;; - 仮定形（けれ/なら）
;;
;; 使用例:
;; (nskk-conjugate-adjective "高" 'i-adjective 'renyou-ku)  ;; => "高く"
;; (nskk-conjugate-adjective "静か" 'na-adjective 'renyou-ni) ;; => "静かに"

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-adjective-conjugation nil
  "形容詞活用エンジンの設定。"
  :group 'nskk
  :prefix "nskk-adjective-conjugation-")

;;; 活用型の定義

(defconst nskk-adjective-types
  '(i-adjective      ; イ形容詞
    na-adjective)    ; ナ形容詞（形容動詞）
  "形容詞の活用型一覧。")

(defconst nskk-adjective-forms
  '(;; イ形容詞の活用形
    mizen            ; 未然形（かろ）
    renyou-ku        ; 連用形（く）
    renyou-katta     ; 連用形（かっ）
    shushi           ; 終止形（い）
    rentai           ; 連体形（い）
    katei            ; 仮定形（けれ）
    ;; ナ形容詞の活用形
    renyou-ni        ; 連用形（に）
    renyou-datta     ; 連用形（だっ）
    shushi-da        ; 終止形（だ）
    rentai-na        ; 連体形（な）
    katei-nara)      ; 仮定形（なら）
  "形容詞の活用形一覧。")

;;; イ形容詞のエンディングテーブル

(defconst nskk-i-adjective-endings
  '((mizen . "かろ")
    (renyou-ku . "く")
    (renyou-katta . "かっ")
    (shushi . "い")
    (rentai . "い")
    (katei . "けれ"))
  "イ形容詞の語尾変化テーブル。")

;;; ナ形容詞のエンディングテーブル

(defconst nskk-na-adjective-endings
  '((mizen . "だろ")
    (renyou-ni . "に")
    (renyou-datta . "だっ")
    (shushi . "だ")
    (shushi-da . "だ")
    (rentai . "な")
    (rentai-na . "な")
    (katei . "なら")
    (katei-nara . "なら"))
  "ナ形容詞（形容動詞）の語尾変化テーブル。")

;;; ヘルパー関数

(defun nskk-adjective--remove-i (stem)
  "イ形容詞の語幹から語尾「い」を除去する。
STEMが「い」で終わっている場合、それを除去した部分を返す。"
  (if (and stem
           (> (length stem) 0)
           (string-suffix-p "い" stem))
      (substring stem 0 -1)
    stem))

(defun nskk-adjective--conjugate-i-adjective (stem form)
  "イ形容詞の活用を返す。
STEMは語幹（例: \"高\"、\"高い\"のいずれも可）、FORMは活用形。"
  (let* ((base (nskk-adjective--remove-i stem))
         (ending (cdr (assq form nskk-i-adjective-endings))))
    (when ending
      (concat base ending))))

(defun nskk-adjective--conjugate-na-adjective (stem form)
  "ナ形容詞の活用を返す。
STEMは語幹（例: \"静か\"）、FORMは活用形。"
  (let ((ending (cdr (assq form nskk-na-adjective-endings))))
    (when ending
      (concat stem ending))))

;;; 公開API

;;;###autoload
(defun nskk-conjugate-adjective (stem type form)
  "形容詞STEMをTYPE（活用型）とFORM（活用形）に基づいて活用する。

引数:
  STEM - 形容詞の語幹（文字列）
  TYPE - 活用型（symbol）
         'i-adjective, 'na-adjective
  FORM - 活用形（symbol）
         イ形容詞: 'mizen, 'renyou-ku, 'renyou-katta, 'shushi, 'rentai, 'katei
         ナ形容詞: 'mizen, 'renyou-ni, 'renyou-datta, 'shushi-da, 'rentai-na, 'katei-nara

返り値:
  活用後の文字列、またはnil（無効な入力の場合）

使用例:
  (nskk-conjugate-adjective \"高\" 'i-adjective 'renyou-ku)     ;; => \"高く\"
  (nskk-conjugate-adjective \"高い\" 'i-adjective 'renyou-ku)   ;; => \"高く\"
  (nskk-conjugate-adjective \"高\" 'i-adjective 'renyou-katta)  ;; => \"高かっ\"
  (nskk-conjugate-adjective \"高\" 'i-adjective 'katei)         ;; => \"高けれ\"
  (nskk-conjugate-adjective \"静か\" 'na-adjective 'renyou-ni)  ;; => \"静かに\"
  (nskk-conjugate-adjective \"静か\" 'na-adjective 'shushi-da)  ;; => \"静かだ\"
  (nskk-conjugate-adjective \"静か\" 'na-adjective 'rentai-na)  ;; => \"静かな\""
  (unless (stringp stem)
    (signal 'wrong-type-argument (list 'stringp stem)))
  (unless (memq type nskk-adjective-types)
    (signal 'wrong-type-argument (list 'nskk-adjective-types type)))
  (unless (memq form nskk-adjective-forms)
    (signal 'wrong-type-argument (list 'nskk-adjective-forms form)))

  (pcase type
    ('i-adjective (nskk-adjective--conjugate-i-adjective stem form))
    ('na-adjective (nskk-adjective--conjugate-na-adjective stem form))
    (_ nil)))

;;;###autoload
(defun nskk-adjective-get-all-forms (stem type)
  "形容詞STEMの全活用形を返す。

引数:
  STEM - 形容詞の語幹（文字列）
  TYPE - 活用型（symbol）

返り値:
  活用形と活用後の文字列のalist

使用例:
  (nskk-adjective-get-all-forms \"高\" 'i-adjective)
  ;; => ((mizen . \"高かろ\") (renyou-ku . \"高く\") ...)"
  (let ((forms (if (eq type 'i-adjective)
                   '(mizen renyou-ku renyou-katta shushi rentai katei)
                 '(mizen renyou-ni renyou-datta shushi-da rentai-na katei-nara))))
    (mapcar (lambda (form)
              (cons form (nskk-conjugate-adjective stem type form)))
            forms)))

;;; 特殊形容詞の処理

(defun nskk-adjective-is-special-p (stem)
  "特殊な活用をする形容詞かどうかを判定する。
現在は「いい」「よい」などの特殊形容詞をサポート。"
  (member stem '("いい" "良い" "よい")))

(defun nskk-adjective-conjugate-special (stem form)
  "特殊形容詞の活用を返す。
「いい」「よい」などの不規則活用に対応。"
  (cond
   ;; 「いい」「よい」の活用
   ((or (equal stem "いい") (equal stem "良い") (equal stem "よい"))
    (pcase form
      ('mizen "よかろ")
      ('renyou-ku "よく")
      ('renyou-katta "よかっ")
      ('shushi (if (equal stem "いい") "いい" "よい"))
      ('rentai (if (equal stem "いい") "いい" "よい"))
      ('katei "よけれ")
      (_ nil)))
   (t nil)))

;;; 補助機能

(defun nskk-adjective-get-base-form (conjugated type)
  "活用形CONJUGATEDから基本形を推測する（逆変換）。
完全な実装ではなく、ヒューリスティックな推測を行う。"
  (cond
   ((eq type 'i-adjective)
    ;; イ形容詞の場合
    (cond
     ((string-suffix-p "く" conjugated)
      (concat (substring conjugated 0 -1) "い"))
     ((string-suffix-p "かっ" conjugated)
      (concat (substring conjugated 0 -2) "い"))
     ((string-suffix-p "けれ" conjugated)
      (concat (substring conjugated 0 -2) "い"))
     ((string-suffix-p "かろ" conjugated)
      (concat (substring conjugated 0 -2) "い"))
     ((string-suffix-p "い" conjugated)
      conjugated)
     (t nil)))
   ((eq type 'na-adjective)
    ;; ナ形容詞の場合
    (cond
     ((string-suffix-p "に" conjugated)
      (substring conjugated 0 -1))
     ((string-suffix-p "だっ" conjugated)
      (substring conjugated 0 -2))
     ((string-suffix-p "だ" conjugated)
      (substring conjugated 0 -1))
     ((string-suffix-p "な" conjugated)
      (substring conjugated 0 -1))
     ((string-suffix-p "なら" conjugated)
      (substring conjugated 0 -2))
     (t nil)))
   (t nil)))

;;; パフォーマンス最適化

(defvar nskk-adjective--conjugation-cache (make-hash-table :test 'equal)
  "形容詞活用結果のキャッシュ。")

(defun nskk-adjective-clear-cache ()
  "活用キャッシュをクリアする。"
  (interactive)
  (clrhash nskk-adjective--conjugation-cache))

;;; デバッグ用ユーティリティ

(defun nskk-adjective-print-all-forms (stem type)
  "形容詞STEMの全活用形を表形式で表示する（デバッグ用）。"
  (interactive "s語幹: \nS活用型 (i-adjective/na-adjective): ")
  (let ((forms (nskk-adjective-get-all-forms stem (intern type))))
    (with-output-to-temp-buffer "*NSKK Adjective Conjugation*"
      (princ (format "形容詞「%s」の活用（%s）\n" stem type))
      (princ "====================\n\n")
      (dolist (entry forms)
        (princ (format "%-15s: %s\n"
                       (symbol-name (car entry))
                       (cdr entry)))))))

(provide 'nskk-adjective-conjugation)

;;; nskk-adjective-conjugation.el ends here
