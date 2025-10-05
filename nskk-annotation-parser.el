;;; nskk-annotation-parser.el --- Annotation parser for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, annotation
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

;; このファイルはSKK注釈（アノテーション）のパーサーを実装します。
;;
;; 特徴:
;; - SKK標準注釈形式（;で始まる）のパース
;; - マルチメディア注釈（画像・音声パス）
;; - 構造化注釈（JSON/plist形式）
;; - 複数注釈形式の統合処理
;; - エスケープ処理
;;
;; 注釈形式の例:
;;
;;   基本形式:
;;     あい /愛;love/哀;sorrow/
;;
;;   マルチメディア:
;;     さくら /桜;img:/path/to/sakura.png/
;;     おと /音;snd:/path/to/sound.wav/
;;
;;   構造化（JSON風）:
;;     たんご /単語;{"読み":"たんご","品詞":"名詞"}/
;;
;;   複合:
;;     れい /例;example;img:/path/to/image.png/
;;
;; 使用例:
;;
;;   (require 'nskk-annotation-parser)
;;
;;   ;; 基本的な注釈のパース
;;   (nskk-parse-annotation "love")
;;   ;; => (:text "love" :type simple)
;;
;;   ;; マルチメディア注釈のパース
;;   (nskk-parse-annotation "img:/path/to/image.png")
;;   ;; => (:text "img:/path/to/image.png" :type multimedia :media-type image :path "/path/to/image.png")
;;
;;   ;; 構造化注釈のパース
;;   (nskk-parse-annotation "{\"読み\":\"たんご\"}")
;;   ;; => (:text "{\"読み\":\"たんご\"}" :type structured :data (("読み" . "たんご")))

;;; Code:

(require 'cl-lib)
(require 'json)

;;; カスタマイズ変数

(defgroup nskk-annotation-parser nil
  "NSKK annotation parser settings."
  :group 'nskk
  :prefix "nskk-annotation-parser-")

(defcustom nskk-annotation-parser-enable-multimedia t
  "非nilの場合、マルチメディア注釈を有効にする。"
  :type 'boolean
  :group 'nskk-annotation-parser)

(defcustom nskk-annotation-parser-enable-structured t
  "非nilの場合、構造化注釈（JSON/plist）を有効にする。"
  :type 'boolean
  :group 'nskk-annotation-parser)

(defcustom nskk-annotation-parser-max-length 1000
  "注釈の最大文字数。"
  :type 'integer
  :group 'nskk-annotation-parser)

(defcustom nskk-annotation-parser-multimedia-protocols
  '("img:" "snd:" "video:" "file:" "http:" "https:")
  "マルチメディア注釈として認識するプロトコル。"
  :type '(repeat string)
  :group 'nskk-annotation-parser)

;;; データ構造

(cl-defstruct (nskk-annotation
               (:constructor nskk-annotation--create)
               (:copier nil))
  "注釈データ構造。

スロット:
  text        - 元の注釈文字列
  type        - 注釈タイプ（'simple, 'multimedia, 'structured, 'compound）
  description - テキスト説明
  media-type  - メディアタイプ（'image, 'sound, 'video, 'file, 'url）
  path        - メディアファイルパス
  data        - 構造化データ（alist）
  components  - 複合注釈の要素リスト"
  (text nil :type (or null string))
  (type 'simple :type symbol)
  (description nil :type (or null string))
  (media-type nil :type (or null symbol))
  (path nil :type (or null string))
  (data nil :type list)
  (components nil :type list))

;;; パーサーメイン

;;;###autoload
(defun nskk-parse-annotation (text)
  "TEXT を注釈としてパースする。
戻り値: `nskk-annotation' 構造体"
  (when (and text (not (string-empty-p text)))
    ;; 長さチェック
    (when (> (length text) nskk-annotation-parser-max-length)
      (setq text (substring text 0 nskk-annotation-parser-max-length)))

    ;; パース実行（優先順位順にチェック）
    (cond
     ;; セミコロンを含まないマルチメディア注釈
     ((and nskk-annotation-parser-enable-multimedia
           (nskk-annotation-parser--multimedia-p text)
           (not (string-match-p ";" text)))
      (nskk-annotation-parser--parse-multimedia text))

     ;; セミコロンを含まない構造化注釈
     ((and nskk-annotation-parser-enable-structured
           (nskk-annotation-parser--structured-p text)
           (not (string-match-p ";" text)))
      (nskk-annotation-parser--parse-structured text))

     ;; セミコロンを含む複合注釈
     ;; ただし、マルチメディアまたは構造化が無効の場合でも、
     ;; 各要素は有効な機能のみでパースされる
     ((string-match-p ";" text)
      (nskk-annotation-parser--parse-compound text))

     ;; 上記に該当しない場合は単純注釈
     (t
      (nskk-annotation-parser--parse-simple text)))))

;;; 単純注釈パーサー

(defun nskk-annotation-parser--parse-simple (text)
  "TEXT を単純注釈としてパースする。"
  (nskk-annotation--create
   :text text
   :type 'simple
   :description text))

;;; マルチメディア注釈パーサー

(defun nskk-annotation-parser--multimedia-p (text)
  "TEXT がマルチメディア注釈かどうかを判定する。"
  (cl-some (lambda (protocol)
             (string-prefix-p protocol text))
           nskk-annotation-parser-multimedia-protocols))

(defun nskk-annotation-parser--parse-multimedia (text)
  "TEXT をマルチメディア注釈としてパースする。
形式: \"img:/path/to/file\" または \"http://example.com/file\""
  (let* ((media-type nil)
         (path nil))
    (cond
     ;; 画像
     ((string-prefix-p "img:" text)
      (setq media-type 'image)
      (setq path (substring text 4)))

     ;; 音声
     ((string-prefix-p "snd:" text)
      (setq media-type 'sound)
      (setq path (substring text 4)))

     ;; 動画
     ((string-prefix-p "video:" text)
      (setq media-type 'video)
      (setq path (substring text 6)))

     ;; ファイル
     ((string-prefix-p "file:" text)
      (setq media-type 'file)
      (setq path (substring text 5)))

     ;; URL
     ((or (string-prefix-p "http:" text)
          (string-prefix-p "https:" text))
      (setq media-type 'url)
      (setq path text))

     ;; その他（デフォルト）
     (t
      (setq media-type 'unknown)
      (setq path text)))

    (nskk-annotation--create
     :text text
     :type 'multimedia
     :media-type media-type
     :path path
     :description (format "[%s] %s" media-type path))))

;;; 構造化注釈パーサー

(defun nskk-annotation-parser--structured-p (text)
  "TEXT が構造化注釈かどうかを判定する。"
  (or (and (string-prefix-p "{" text) (string-suffix-p "}" text))
      (and (string-prefix-p "[" text) (string-suffix-p "]" text))
      (string-prefix-p "(" text)))

(defun nskk-annotation-parser--parse-structured (text)
  "TEXT を構造化注釈としてパースする。
JSON形式またはplist形式をサポート。"
  (let ((data nil)
        (description nil))
    (condition-case err
        (cond
         ;; JSON形式
         ((or (string-prefix-p "{" text) (string-prefix-p "[" text))
          (setq data (nskk-annotation-parser--parse-json text))
          (setq description (nskk-annotation-parser--format-structured data)))

         ;; plist形式（簡易）
         ((string-prefix-p "(" text)
          (setq data (nskk-annotation-parser--parse-plist text))
          (setq description (nskk-annotation-parser--format-structured data)))

         ;; パース不可
         (t
          (setq description text)))
      (error
       ;; パースエラー時は単純注釈として扱う
       (setq description text)))

    (nskk-annotation--create
     :text text
     :type 'structured
     :data data
     :description (or description text))))

(defun nskk-annotation-parser--parse-json (text)
  "TEXT をJSON形式としてパースする。"
  (condition-case nil
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read-from-string text))
    (error nil)))

(defun nskk-annotation-parser--parse-plist (text)
  "TEXT をplist形式としてパースする（簡易実装）。"
  (condition-case nil
      (let* ((trimmed (string-trim text "(" ")"))
             (pairs (split-string trimmed))
             (result nil))
        (while pairs
          (let ((key (pop pairs))
                (val (pop pairs)))
            (when (and key val)
              (push (cons key val) result))))
        (nreverse result))
    (error nil)))

(defun nskk-annotation-parser--format-structured (data)
  "構造化データ DATA を読みやすい文字列に整形する。"
  (cond
   ((null data) "")
   ((listp data)
    (mapconcat (lambda (pair)
                 (format "%s: %s"
                         (if (stringp (car pair)) (car pair) (format "%s" (car pair)))
                         (if (stringp (cdr pair)) (cdr pair) (format "%s" (cdr pair)))))
               data ", "))
   (t (format "%s" data))))

;;; 複合注釈パーサー

(defun nskk-annotation-parser--parse-compound (text)
  "TEXT を複合注釈としてパースする。
セミコロンで区切られた複数の注釈要素を処理。"
  (let* ((parts (split-string text ";" t "\\s-+"))
         (components (mapcar #'nskk-parse-annotation parts))
         (descriptions (mapcar (lambda (comp)
                                (or (nskk-annotation-description comp)
                                    (nskk-annotation-text comp)))
                              components)))
    (nskk-annotation--create
     :text text
     :type 'compound
     :components components
     :description (string-join descriptions "; "))))

;;; エスケープ処理

(defun nskk-annotation-parser-escape (text)
  "TEXT 内の特殊文字をエスケープする。"
  (when text
    (let ((result text))
      ;; セミコロンのエスケープ
      (setq result (replace-regexp-in-string ";" "\\\\;" result))
      ;; スラッシュのエスケープ
      (setq result (replace-regexp-in-string "/" "\\\\/" result))
      result)))

(defun nskk-annotation-parser-unescape (text)
  "TEXT 内のエスケープ文字を元に戻す。"
  (when text
    (let ((result text))
      ;; エスケープ解除
      (setq result (replace-regexp-in-string "\\\\;" ";" result))
      (setq result (replace-regexp-in-string "\\\\/" "/" result))
      result)))

;;; ユーティリティ関数

(defun nskk-annotation-parser-get-description (annotation)
  "ANNOTATION から表示用の説明文を取得する。"
  (when annotation
    (or (nskk-annotation-description annotation)
        (nskk-annotation-text annotation)
        "")))

(defun nskk-annotation-parser-has-media-p (annotation)
  "ANNOTATION がメディアファイルを持つかどうかを判定する。"
  (and annotation
       (eq (nskk-annotation-type annotation) 'multimedia)
       (nskk-annotation-path annotation)
       t))

(defun nskk-annotation-parser-get-media-path (annotation)
  "ANNOTATION からメディアファイルパスを取得する。"
  (when (nskk-annotation-parser-has-media-p annotation)
    (nskk-annotation-path annotation)))

(defun nskk-annotation-parser-to-string (annotation)
  "ANNOTATION を文字列形式に変換する。"
  (if annotation
      (nskk-annotation-text annotation)
    ""))

;;; デバッグ・統計機能

(defun nskk-annotation-parser-describe (annotation)
  "ANNOTATION の詳細情報を表示する。"
  (when annotation
    (message "NSKK Annotation:
  Text: %s
  Type: %s
  Description: %s
  Media Type: %s
  Path: %s
  Data: %S
  Components: %d"
             (nskk-annotation-text annotation)
             (nskk-annotation-type annotation)
             (nskk-annotation-description annotation)
             (nskk-annotation-media-type annotation)
             (nskk-annotation-path annotation)
             (nskk-annotation-data annotation)
             (length (nskk-annotation-components annotation)))))

(defun nskk-annotation-parser-statistics (annotations)
  "ANNOTATIONS のリストから統計情報を生成する。"
  (let ((total (length annotations))
        (simple 0)
        (multimedia 0)
        (structured 0)
        (compound 0))
    (dolist (ann annotations)
      (pcase (nskk-annotation-type ann)
        ('simple (cl-incf simple))
        ('multimedia (cl-incf multimedia))
        ('structured (cl-incf structured))
        ('compound (cl-incf compound))))
    (list :total total
          :simple simple
          :multimedia multimedia
          :structured structured
          :compound compound)))

(provide 'nskk-annotation-parser)

;;; nskk-annotation-parser.el ends here
