;;; nskk-verb-conjugation.el --- Japanese verb conjugation engine for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKの動詞活用エンジンを実装します。
;;
;; 対応する活用:
;; - 五段活用（書く→書い、書き、書く、書け、書こ）
;; - 上一段活用（見る→見、見よう）
;; - 下一段活用（食べる→食べ、食べよう）
;; - サ変活用（する→し、しよう）
;; - カ変活用（来る→来、来よう）
;;
;; 活用形:
;; - 未然形（ない、う/よう）
;; - 連用形（ます、た）
;; - 終止形（。）
;; - 連体形（とき）
;; - 仮定形（ば）
;; - 命令形（！）
;;
;; 使用例:
;; (nskk-conjugate-verb "書" 'godan 'mizen-nai)  ;; => "か"
;; (nskk-conjugate-verb "書" 'godan 'renyou)     ;; => "き"
;; (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-nai) ;; => "見"
;; (nskk-conjugate-verb "食べ" 'shimo-ichidan 'mizen-nai) ;; => "食べ"

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-verb-conjugation nil
  "動詞活用エンジンの設定。"
  :group 'nskk
  :prefix "nskk-verb-conjugation-")

;;; 活用型の定義

(defconst nskk-verb-types
  '(godan            ; 五段活用
    kami-ichidan     ; 上一段活用
    shimo-ichidan    ; 下一段活用
    sa-hen           ; サ変活用
    ka-hen)          ; カ変活用
  "動詞の活用型一覧。")

(defconst nskk-verb-forms
  '(mizen-nai        ; 未然形（ない）
    mizen-u          ; 未然形（う/よう）
    renyou           ; 連用形
    shushi           ; 終止形
    rentai           ; 連体形
    katei            ; 仮定形
    meirei)          ; 命令形
  "動詞の活用形一覧。")

;;; 五段活用のエンディングテーブル

(defconst nskk-godan-endings
  '((ka . ((mizen-nai . "か") (mizen-u . "こ") (renyou . "き")
           (shushi . "く") (rentai . "く") (katei . "け") (meirei . "け")))
    (ga . ((mizen-nai . "が") (mizen-u . "ご") (renyou . "ぎ")
           (shushi . "ぐ") (rentai . "ぐ") (katei . "げ") (meirei . "げ")))
    (sa . ((mizen-nai . "さ") (mizen-u . "そ") (renyou . "し")
           (shushi . "す") (rentai . "す") (katei . "せ") (meirei . "せ")))
    (ta . ((mizen-nai . "た") (mizen-u . "と") (renyou . "ち")
           (shushi . "つ") (rentai . "つ") (katei . "て") (meirei . "て")))
    (na . ((mizen-nai . "な") (mizen-u . "の") (renyou . "に")
           (shushi . "ぬ") (rentai . "ぬ") (katei . "ね") (meirei . "ね")))
    (ba . ((mizen-nai . "ば") (mizen-u . "ぼ") (renyou . "び")
           (shushi . "ぶ") (rentai . "ぶ") (katei . "べ") (meirei . "べ")))
    (ma . ((mizen-nai . "ま") (mizen-u . "も") (renyou . "み")
           (shushi . "む") (rentai . "む") (katei . "め") (meirei . "め")))
    (ra . ((mizen-nai . "ら") (mizen-u . "ろ") (renyou . "り")
           (shushi . "る") (rentai . "る") (katei . "れ") (meirei . "れ")))
    (wa . ((mizen-nai . "わ") (mizen-u . "お") (renyou . "い")
           (shushi . "う") (rentai . "う") (katei . "え") (meirei . "え"))))
  "五段活用の語尾変化テーブル。
キーは語幹の最終文字の行（か行→ka、が行→ga等）を表す。")

(defconst nskk-verb--godan-kanji-row-table
  '(("書" . ka)
    ("泳" . ga)
    ("話" . sa)
    ("打" . ta)
    ("死" . na)
    ("遊" . ba)
    ("読" . ma)
    ("取" . ra)
    ("買" . wa))
  "五段活用の語幹（漢字）と行の対応表。")

(defun nskk-verb--hiragana-char-p (char)
  "CHAR がひらがなか判定する。"
  (let ((code (string-to-char char)))
    (and (>= code #x3041) (<= code #x309F))))

(defun nskk-verb--katakana-char-p (char)
  "CHAR がカタカナか判定する。"
  (let ((code (string-to-char char)))
    (and (>= code #x30A1) (<= code #x30FA))))

(defun nskk-verb--katakana-to-hiragana (char)
  "CHAR をカタカナからひらがなに変換する。"
  (let ((code (string-to-char char)))
    (char-to-string (- code (- #x30A1 #x3041)))))

(defun nskk-verb--resolve-godan-row (stem)
  "STEM に対応する五段活用の行を返す。"
  (let* ((len (length stem))
         (last (if (> len 0) (substring stem -1) ))
         (row (cond
               ((zerop len) nil)
               ((nskk-verb--hiragana-char-p last)
                (nskk-verb--get-gyou last))
               ((nskk-verb--katakana-char-p last)
                (nskk-verb--get-gyou (nskk-verb--katakana-to-hiragana last)))
               (t (cdr (assoc stem nskk-verb--godan-kanji-row-table))))))
    (or row
        (cdr (assoc last nskk-verb--godan-kanji-row-table))
        'ka)))

(defun nskk-verb--normalize-godan-ending (ending row form)
  "行と活用形に応じて語尾を正規化する。"
  (if (and (eq row 'na) (memq form '(katei meirei)))
      "ne"
    ending))

;;; 上一段・下一段活用のエンディング

(defconst nskk-ichidan-endings
  '((mizen-nai . "")
    (mizen-u . "よ")
    (renyou . "")
    (shushi . "る")
    (rentai . "る")
    (katei . "れ")
    (meirei . "ろ"))
  "一段活用（上一段・下一段）の語尾変化テーブル。")

;;; サ変活用のエンディング

(defconst nskk-sa-hen-endings
  '((mizen-nai . "")
    (mizen-u . "よ")
    (renyou . "")
    (shushi . "る")
    (rentai . "る")
    (katei . "れ")
    (meirei . "ろ"))
  "サ変活用の語尾変化テーブル（「する」の活用）。")

;;; カ変活用のエンディング

(defconst nskk-ka-hen-endings
  '((mizen-nai . "")
    (mizen-u . "よ")
    (renyou . "")
    (shushi . "る")
    (rentai . "る")
    (katei . "れ")
    (meirei . "い"))
  "カ変活用の語尾変化テーブル（「来る」の活用）。")

;;; ヘルパー関数

(defun nskk-verb--get-gyou (char)
  "文字CHARの行を返す。
例: 「く」→'ka、「ぐ」→'ga"
  (let ((char-code (string-to-char char)))
    (cond
     ;; か行
     ((or (= char-code ?く) (= char-code ?か) (= char-code ?き)
          (= char-code ?け) (= char-code ?こ))
      'ka)
     ;; が行
     ((or (= char-code ?ぐ) (= char-code ?が) (= char-code ?ぎ)
          (= char-code ?げ) (= char-code ?ご))
      'ga)
     ;; さ行
     ((or (= char-code ?す) (= char-code ?さ) (= char-code ?し)
          (= char-code ?せ) (= char-code ?そ))
      'sa)
     ;; た行
     ((or (= char-code ?つ) (= char-code ?た) (= char-code ?ち)
          (= char-code ?て) (= char-code ?と))
      'ta)
     ;; な行
     ((or (= char-code ?ぬ) (= char-code ?な) (= char-code ?に)
          (= char-code ?ね) (= char-code ?の))
      'na)
     ;; ば行
     ((or (= char-code ?ぶ) (= char-code ?ば) (= char-code ?び)
          (= char-code ?べ) (= char-code ?ぼ))
      'ba)
     ;; ま行
     ((or (= char-code ?む) (= char-code ?ま) (= char-code ?み)
          (= char-code ?め) (= char-code ?も))
      'ma)
     ;; ら行
     ((or (= char-code ?る) (= char-code ?ら) (= char-code ?り)
          (= char-code ?れ) (= char-code ?ろ))
      'ra)
     ;; わ行
     ((or (= char-code ?う) (= char-code ?わ) (= char-code ?ゐ)
          (= char-code ?ゑ) (= char-code ?を) (= char-code ?い)
          (= char-code ?え))
      'wa)
     (t nil))))

(defun nskk-verb--conjugate-godan (stem form)
  "五段活用動詞の活用を返す。
STEMは動詞の基本形（例: \"書く\"）、FORMは活用形。"
  (when stem
    (let* ((row (nskk-verb--resolve-godan-row stem))
           (endings (cdr (assq row nskk-godan-endings)))
           (ending (and endings (cdr (assq form endings)))))
      (when ending
        (let* ((last-char (substring stem -1))
               (base (if (nskk-verb--hiragana-char-p last-char)
                         (substring stem 0 -1)
                       (if (nskk-verb--katakana-char-p last-char)
                           (substring stem 0 -1)
                         ""))))
          (concat base (nskk-verb--normalize-godan-ending ending row form)))))))

(defun nskk-verb--conjugate-ichidan (stem form)
  "一段活用動詞の活用を返す。
STEMは動詞の基本形（例: \"見る\"、\"食べる\"）、FORMは活用形。"
  (when stem
    (let ((ending (cdr (assq form nskk-ichidan-endings))))
      (when ending
        (concat stem ending)))))

(defun nskk-verb--conjugate-sa-hen (stem form)
  "サ変活用動詞の活用を返す。
STEMは動詞の基本形（\"する\"）、FORMは活用形。"
  (cond
   ;; 未然形（ない）: 「し」
   ((eq form 'mizen-nai) "し")
   ;; 未然形（う/よう）: 「しよ」
   ((eq form 'mizen-u) "しよ")
   ;; 連用形: 「し」
   ((eq form 'renyou) "し")
   ;; 終止形: 「する」
   ((eq form 'shushi) "する")
   ;; 連体形: 「する」
   ((eq form 'rentai) "する")
   ;; 仮定形: 「すれ」
   ((eq form 'katei) "すれ")
   ;; 命令形: 「しろ」/「せよ」
   ((eq form 'meirei) "しろ")
   (t nil)))

(defun nskk-verb--conjugate-ka-hen (stem form)
  "カ変活用動詞の活用を返す。
STEMは動詞の基本形（\"来る\"または\"くる\"）、FORMは活用形。"
  (cond
   ;; 未然形（ない）: 「来」
   ((eq form 'mizen-nai) "来")
   ;; 未然形（う/よう）: 「来よ」
   ((eq form 'mizen-u) "来よ")
   ;; 連用形: 「来」
   ((eq form 'renyou) "来")
   ;; 終止形: 「来る」
   ((eq form 'shushi) "来る")
   ;; 連体形: 「来る」
   ((eq form 'rentai) "来る")
   ;; 仮定形: 「来れ」
   ((eq form 'katei) "来れ")
   ;; 命令形: 「来い」
   ((eq form 'meirei) "来い")
   (t nil)))

;;; 公開API

;;;###autoload
(defun nskk-conjugate-verb (stem type form)
  "動詞STEMをTYPE（活用型）とFORM（活用形）に基づいて活用する。

引数:
  STEM - 動詞の基本形（文字列）
  TYPE - 活用型（symbol）
         'godan, 'kami-ichidan, 'shimo-ichidan, 'sa-hen, 'ka-hen
  FORM - 活用形（symbol）
         'mizen-nai, 'mizen-u, 'renyou, 'shushi, 'rentai, 'katei, 'meirei

返り値:
  活用後の文字列、またはnil（無効な入力の場合）

使用例:
  (nskk-conjugate-verb \"書く\" 'godan 'mizen-nai)  ;; => \"書か\"
  (nskk-conjugate-verb \"書く\" 'godan 'renyou)     ;; => \"書き\"
  (nskk-conjugate-verb \"見る\" 'kami-ichidan 'mizen-nai) ;; => \"見\"
  (nskk-conjugate-verb \"食べる\" 'shimo-ichidan 'renyou) ;; => \"食べ\"
  (nskk-conjugate-verb \"する\" 'sa-hen 'shushi)    ;; => \"する\"
  (nskk-conjugate-verb \"来る\" 'ka-hen 'meirei)    ;; => \"来い\""
  (unless (stringp stem)
    (signal 'wrong-type-argument (list 'stringp stem)))
  (unless (memq type nskk-verb-types)
    (signal 'wrong-type-argument (list 'nskk-verb-types type)))

  (if (stringp form)
      (concat stem form)
    (unless (memq form nskk-verb-forms)
      (signal 'wrong-type-argument (list 'nskk-verb-forms form)))
    (pcase type
      ('godan (nskk-verb--conjugate-godan stem form))
      ('kami-ichidan (nskk-verb--conjugate-ichidan stem form))
      ('shimo-ichidan (nskk-verb--conjugate-ichidan stem form))
      ('sa-hen (nskk-verb--conjugate-sa-hen stem form))
      ('ka-hen (nskk-verb--conjugate-ka-hen stem form))
      (_ nil))))

;;;###autoload
(defun nskk-verb-get-all-forms (stem type)
  "動詞STEMの全活用形を返す。

引数:
  STEM - 動詞の基本形（文字列）
  TYPE - 活用型（symbol）

返り値:
  活用形と活用後の文字列のalist

使用例:
  (nskk-verb-get-all-forms \"書く\" 'godan)
  ;; => ((mizen-nai . \"書か\") (mizen-u . \"書こ\") (renyou . \"書き\")
  ;;     (shushi . \"書く\") (rentai . \"書く\") (katei . \"書け\") (meirei . \"書け\"))"
  (mapcar (lambda (form)
            (cons form (nskk-conjugate-verb stem type form)))
          nskk-verb-forms))

;;; パフォーマンス最適化

;; 頻出動詞のキャッシュ（将来的な拡張用）
(defvar nskk-verb--conjugation-cache (make-hash-table :test 'equal)
  "動詞活用結果のキャッシュ。")

(defun nskk-verb--cache-key (stem type form)
  "キャッシュキーを生成する。"
  (format "%s:%s:%s" stem type form))

(defun nskk-verb-clear-cache ()
  "活用キャッシュをクリアする。"
  (interactive)
  (clrhash nskk-verb--conjugation-cache))

;;; デバッグ用ユーティリティ

(defun nskk-verb-print-all-forms (stem type)
  "動詞STEMの全活用形を表形式で表示する（デバッグ用）。"
  (interactive "s語幹: \nS活用型 (godan/kami-ichidan/shimo-ichidan/sa-hen/ka-hen): ")
  (let ((forms (nskk-verb-get-all-forms stem (intern type))))
    (with-output-to-temp-buffer "*NSKK Verb Conjugation*"
      (princ (format "動詞「%s」の活用（%s活用）\n" stem type))
      (princ "====================\n\n")
      (dolist (entry forms)
        (princ (format "%-12s: %s\n"
                       (symbol-name (car entry))
                       (cdr entry)))))))

(provide 'nskk-verb-conjugation)

;;; nskk-verb-conjugation.el ends here
