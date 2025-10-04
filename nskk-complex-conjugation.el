;;; nskk-complex-conjugation.el --- Complex conjugation engine for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKの複雑活用処理エンジンを実装します。
;;
;; 対応する複合活用:
;; - 使役形（書かせる、見させる）
;; - 受身形（書かれる、見られる）
;; - 可能形（書ける、見られる）
;; - 使役受身形（書かせられる）
;; - 尊敬形（書かれる、お書きになる）
;; - 謙譲形（お書きする）
;;
;; 特殊活用:
;; - 不規則動詞の処理
;; - エッジケース（ら抜き言葉等）
;;
;; 使用例:
;; (nskk-complex-conjugate "書" 'godan 'causative)  ;; => "書かせ"
;; (nskk-complex-conjugate "見" 'kami-ichidan 'passive) ;; => "見られ"
;; (nskk-complex-conjugate "書" 'godan 'potential)  ;; => "書け"

;;; Code:

(require 'cl-lib)
(require 'nskk-verb-conjugation)

;;; カスタマイズ可能変数

(defgroup nskk-complex-conjugation nil
  "複雑活用エンジンの設定。"
  :group 'nskk
  :prefix "nskk-complex-conjugation-")

(defcustom nskk-complex-allow-ra-nuki t
  "ら抜き言葉を許可するかどうか。
非nil の場合、「食べれる」のような形式も可能形として認識する。"
  :type 'boolean
  :group 'nskk-complex-conjugation)

;;; 複合活用型の定義

(defconst nskk-complex-conjugation-types
  '(causative          ; 使役形（せる/させる）
    passive            ; 受身形（れる/られる）
    potential          ; 可能形（れる/られる/える）
    causative-passive  ; 使役受身形（せられる/させられる）
    honorific          ; 尊敬形（れる/られる）
    humble)            ; 謙譲形（お〜する）
  "複合活用の種類一覧。")

;;; ヘルパー関数

(defun nskk-complex--get-mizen-form (stem type)
  "動詞の未然形を取得する。
STEMは語幹、TYPEは活用型。"
  (nskk-conjugate-verb stem type 'mizen-nai))

;;; 使役形の処理

(defun nskk-complex--causative-godan (stem)
  "五段活用の使役形を返す。
例: 書く→書かせ"
  (let ((mizen (nskk-complex--get-mizen-form stem 'godan)))
    (when mizen
      (concat mizen "せ"))))

(defun nskk-complex--causative-ichidan (stem)
  "一段活用の使役形を返す。
例: 見る→見させ、食べる→食べさせ"
  (concat stem "させ"))

(defun nskk-complex--causative-sa-hen (stem)
  "サ変活用の使役形を返す。
例: する→させ"
  "させ")

(defun nskk-complex--causative-ka-hen (stem)
  "カ変活用の使役形を返す。
例: 来る→来させ"
  "来させ")

;;; 受身形の処理

(defun nskk-complex--passive-godan (stem)
  "五段活用の受身形を返す。
例: 書く→書かれ"
  (let ((mizen (nskk-complex--get-mizen-form stem 'godan)))
    (when mizen
      (concat mizen "れ"))))

(defun nskk-complex--passive-ichidan (stem)
  "一段活用の受身形を返す。
例: 見る→見られ、食べる→食べられ"
  (concat stem "られ"))

(defun nskk-complex--passive-sa-hen (stem)
  "サ変活用の受身形を返す。
例: する→される"
  "され")

(defun nskk-complex--passive-ka-hen (stem)
  "カ変活用の受身形を返す。
例: 来る→来られ"
  "来られ")

;;; 可能形の処理

(defun nskk-complex--potential-godan (stem)
  "五段活用の可能形を返す。
例: 書く→書ける"
  (let ((katei (nskk-conjugate-verb stem 'godan 'katei)))
    (when katei
      ;; 「け」→「け」（そのまま）
      katei)))

(defun nskk-complex--potential-ichidan (stem)
  "一段活用の可能形を返す。
例: 見る→見られ、食べる→食べられ
ら抜き許可時: 見れ、食べれ"
  (if nskk-complex-allow-ra-nuki
      ;; ら抜き言葉を許可
      (concat stem "れ")
    ;; 標準形
    (concat stem "られ")))

(defun nskk-complex--potential-sa-hen (stem)
  "サ変活用の可能形を返す。
例: する→できる（特殊）"
  ;; 「できる」は特殊なので、ここでは基本形のみ
  "でき")

(defun nskk-complex--potential-ka-hen (stem)
  "カ変活用の可能形を返す。
例: 来る→来られる"
  "来られ")

;;; 使役受身形の処理

(defun nskk-complex--causative-passive-godan (stem)
  "五段活用の使役受身形を返す。
例: 書く→書かせられ"
  (let ((mizen (nskk-complex--get-mizen-form stem 'godan)))
    (when mizen
      (concat mizen "せられ"))))

(defun nskk-complex--causative-passive-ichidan (stem)
  "一段活用の使役受身形を返す。
例: 見る→見させられ、食べる→食べさせられ"
  (concat stem "させられ"))

(defun nskk-complex--causative-passive-sa-hen (stem)
  "サ変活用の使役受身形を返す。
例: する→させられ"
  "させられ")

(defun nskk-complex--causative-passive-ka-hen (stem)
  "カ変活用の使役受身形を返す。
例: 来る→来させられ"
  "来させられ")

;;; 尊敬形の処理

(defun nskk-complex--honorific-godan (stem)
  "五段活用の尊敬形を返す。
例: 書く→書かれ（受身形と同じ）"
  (nskk-complex--passive-godan stem))

(defun nskk-complex--honorific-ichidan (stem)
  "一段活用の尊敬形を返す。
例: 見る→見られ（受身形と同じ）"
  (nskk-complex--passive-ichidan stem))

(defun nskk-complex--honorific-sa-hen (stem)
  "サ変活用の尊敬形を返す。
例: する→される"
  "され")

(defun nskk-complex--honorific-ka-hen (stem)
  "カ変活用の尊敬形を返す。
例: 来る→来られる"
  "来られ")

;;; 謙譲形の処理

(defun nskk-complex--humble (stem verb-type)
  "謙譲形を返す。
例: 書く→お書きし、見る→お見し
これは「お〜する」の形式の語幹部分を返す。"
  (let ((renyou (nskk-conjugate-verb stem verb-type 'renyou)))
    (when renyou
      (concat "お" renyou "し"))))

;;; 公開API

;;;###autoload
(defun nskk-complex-conjugate (stem verb-type complex-type)
  "動詞STEMを複合活用する。

引数:
  STEM - 動詞の語幹（文字列）
  VERB-TYPE - 動詞の活用型（symbol）
              'godan, 'kami-ichidan, 'shimo-ichidan, 'sa-hen, 'ka-hen
  COMPLEX-TYPE - 複合活用の種類（symbol）
                 'causative, 'passive, 'potential, 'causative-passive,
                 'honorific, 'humble

返り値:
  複合活用後の文字列、またはnil（無効な入力の場合）

使用例:
  (nskk-complex-conjugate \"書\" 'godan 'causative)  ;; => \"書かせ\"
  (nskk-complex-conjugate \"見\" 'kami-ichidan 'passive) ;; => \"見られ\"
  (nskk-complex-conjugate \"書\" 'godan 'potential)  ;; => \"書け\"
  (nskk-complex-conjugate \"食べ\" 'shimo-ichidan 'potential) ;; => \"食べれ\" (ら抜き時)
  (nskk-complex-conjugate \"書\" 'godan 'causative-passive) ;; => \"書かせられ\"
  (nskk-complex-conjugate \"見\" 'kami-ichidan 'humble) ;; => \"お見し\""
  (unless (stringp stem)
    (signal 'wrong-type-argument (list 'stringp stem)))
  (unless (memq verb-type nskk-verb-types)
    (signal 'wrong-type-argument (list 'nskk-verb-types verb-type)))
  (unless (memq complex-type nskk-complex-conjugation-types)
    (signal 'wrong-type-argument (list 'nskk-complex-conjugation-types complex-type)))

  (pcase complex-type
    ('causative
     (pcase verb-type
       ('godan (nskk-complex--causative-godan stem))
       ('kami-ichidan (nskk-complex--causative-ichidan stem))
       ('shimo-ichidan (nskk-complex--causative-ichidan stem))
       ('sa-hen (nskk-complex--causative-sa-hen stem))
       ('ka-hen (nskk-complex--causative-ka-hen stem))))

    ('passive
     (pcase verb-type
       ('godan (nskk-complex--passive-godan stem))
       ('kami-ichidan (nskk-complex--passive-ichidan stem))
       ('shimo-ichidan (nskk-complex--passive-ichidan stem))
       ('sa-hen (nskk-complex--passive-sa-hen stem))
       ('ka-hen (nskk-complex--passive-ka-hen stem))))

    ('potential
     (pcase verb-type
       ('godan (nskk-complex--potential-godan stem))
       ('kami-ichidan (nskk-complex--potential-ichidan stem))
       ('shimo-ichidan (nskk-complex--potential-ichidan stem))
       ('sa-hen (nskk-complex--potential-sa-hen stem))
       ('ka-hen (nskk-complex--potential-ka-hen stem))))

    ('causative-passive
     (pcase verb-type
       ('godan (nskk-complex--causative-passive-godan stem))
       ('kami-ichidan (nskk-complex--causative-passive-ichidan stem))
       ('shimo-ichidan (nskk-complex--causative-passive-ichidan stem))
       ('sa-hen (nskk-complex--causative-passive-sa-hen stem))
       ('ka-hen (nskk-complex--causative-passive-ka-hen stem))))

    ('honorific
     (pcase verb-type
       ('godan (nskk-complex--honorific-godan stem))
       ('kami-ichidan (nskk-complex--honorific-ichidan stem))
       ('shimo-ichidan (nskk-complex--honorific-ichidan stem))
       ('sa-hen (nskk-complex--honorific-sa-hen stem))
       ('ka-hen (nskk-complex--honorific-ka-hen stem))))

    ('humble
     (nskk-complex--humble stem verb-type))

    (_ nil)))

;;;###autoload
(defun nskk-complex-get-all-forms (stem verb-type)
  "動詞STEMの全複合活用形を返す。

引数:
  STEM - 動詞の語幹（文字列）
  VERB-TYPE - 動詞の活用型（symbol）

返り値:
  複合活用種類と活用後の文字列のalist

使用例:
  (nskk-complex-get-all-forms \"書\" 'godan)
  ;; => ((causative . \"書かせ\") (passive . \"書かれ\") ...)"
  (mapcar (lambda (type)
            (cons type (nskk-complex-conjugate stem verb-type type)))
          nskk-complex-conjugation-types))

;;; 特殊活用の処理

(defvar nskk-complex--irregular-verbs
  '(("行" . ((potential . "行ける")))  ; 行く→行ける（特殊）
    ("問" . ((potential . "問える"))))  ; 問う→問える
  "不規則活用する動詞のリスト。")

(defun nskk-complex-is-irregular-p (stem)
  "不規則活用する動詞かどうかを判定する。"
  (assoc stem nskk-complex--irregular-verbs))

(defun nskk-complex-conjugate-irregular (stem complex-type)
  "不規則動詞の複合活用を返す。"
  (let* ((irregular (assoc stem nskk-complex--irregular-verbs))
         (forms (cdr irregular)))
    (when forms
      (cdr (assq complex-type forms)))))

;;; デバッグ用ユーティリティ

(defun nskk-complex-print-all-forms (stem verb-type)
  "動詞STEMの全複合活用形を表形式で表示する（デバッグ用）。"
  (interactive "s語幹: \nS活用型 (godan/kami-ichidan/shimo-ichidan/sa-hen/ka-hen): ")
  (let ((forms (nskk-complex-get-all-forms stem (intern verb-type))))
    (with-output-to-temp-buffer "*NSKK Complex Conjugation*"
      (princ (format "動詞「%s」の複合活用（%s活用）\n" stem verb-type))
      (princ "====================\n\n")
      (dolist (entry forms)
        (princ (format "%-20s: %s\n"
                       (symbol-name (car entry))
                       (cdr entry)))))))

(provide 'nskk-complex-conjugation)

;;; nskk-complex-conjugation.el ends here
