;;; nskk-dict-errors.el --- Error handling for SKK dictionary operations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, errors
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

;; このファイルはSKK辞書システムの包括的なエラーハンドリング機構を実装します。
;;
;; 特徴:
;; - 階層的なカスタムエラー型定義
;; - 自動エラーリカバリー機構
;; - フォールバック辞書機能
;; - エラーロギング・監視
;; - ユーザーフレンドリーな通知
;;
;; カスタムエラー型階層:
;;
;;   nskk-dict-error (ベース)
;;     ├─ nskk-dict-io-error (I/O関連)
;;     │   ├─ nskk-dict-io-file-not-found
;;     │   ├─ nskk-dict-io-permission-denied
;;     │   ├─ nskk-dict-io-checksum-mismatch
;;     │   └─ nskk-dict-io-backup-failed
;;     ├─ nskk-dict-parse-error (パース関連)
;;     │   ├─ nskk-dict-parse-invalid-format
;;     │   └─ nskk-dict-parse-encoding-error
;;     └─ nskk-dict-struct-error (構造関連)
;;         └─ nskk-dict-struct-invalid-entry
;;
;; リカバリー戦略:
;;
;;   - ファイルなし          → フォールバック辞書を使用
;;   - パースエラー          → バックアップから復旧
;;   - チェックサム不一致    → 強制再読み込み
;;   - 権限エラー            → 読み取り専用モード
;;   - エンコーディングエラー → 別エンコーディングで再試行
;;
;; 使用例:
;;
;;   (require 'nskk-dict-errors)
;;
;;   ;; エラーリカバリー付きで辞書読み込み
;;   (let ((index (nskk-dict-errors-load-with-recovery "/path/to/dict")))
;;     ;; 失敗時はフォールバック辞書が返される
;;     (message "Loaded: %s" index))
;;
;;   ;; カスタムエラーのシグナル
;;   (signal 'nskk-dict-io-file-not-found (list "/path/to/dict"))
;;
;;   ;; エラーログの表示
;;   (nskk-dict-errors-show-log)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-dict-errors nil
  "SKK dictionary error handling customization."
  :group 'nskk
  :prefix "nskk-dict-errors-")

(defcustom nskk-dict-errors-fallback-dict-path nil
  "フォールバック辞書のパス。
nilの場合、最小限の組み込み辞書を使用。"
  :type '(choice (const :tag "Built-in minimal" nil)
                 (file :tag "Custom fallback"))
  :group 'nskk-dict-errors)

(defcustom nskk-dict-errors-enable-logging t
  "非nilの場合、エラーログを記録する。"
  :type 'boolean
  :group 'nskk-dict-errors)

(defcustom nskk-dict-errors-log-file "~/.nskk/error.log"
  "エラーログファイルのパス。"
  :type 'file
  :group 'nskk-dict-errors)

(defcustom nskk-dict-errors-notification-method 'message
  "エラー通知方法。
- 'message: ミニバッファに表示
- 'popup: ポップアップ表示（将来実装）
- 'silent: 通知しない（ログのみ）"
  :type '(choice (const message)
                 (const popup)
                 (const silent))
  :group 'nskk-dict-errors)

(defcustom nskk-dict-errors-auto-recovery t
  "非nilの場合、エラー時に自動リカバリーを試みる。"
  :type 'boolean
  :group 'nskk-dict-errors)

(defcustom nskk-dict-errors-max-recovery-attempts 3
  "最大リカバリー試行回数。"
  :type 'integer
  :group 'nskk-dict-errors)

;;; エラー型定義

;; ベースエラー型
(define-error 'nskk-dict-error
  "SKK辞書エラー"
  'error)

;; I/Oエラー
(define-error 'nskk-dict-io-error
  "辞書I/Oエラー"
  'nskk-dict-error)

(define-error 'nskk-dict-io-file-not-found
  "辞書ファイルが見つかりません"
  'nskk-dict-io-error)

(define-error 'nskk-dict-io-permission-denied
  "権限がありません"
  'nskk-dict-io-error)

(define-error 'nskk-dict-io-checksum-mismatch
  "チェックサムが一致しません"
  'nskk-dict-io-error)

(define-error 'nskk-dict-io-backup-failed
  "バックアップに失敗しました"
  'nskk-dict-io-error)

;; パースエラー
(define-error 'nskk-dict-parse-error
  "辞書パースエラー"
  'nskk-dict-error)

(define-error 'nskk-dict-parse-invalid-format
  "辞書フォーマットが不正です"
  'nskk-dict-parse-error)

(define-error 'nskk-dict-parse-encoding-error
  "エンコーディングエラー"
  'nskk-dict-parse-error)

;; 構造エラー
(define-error 'nskk-dict-struct-error
  "辞書構造エラー"
  'nskk-dict-error)

(define-error 'nskk-dict-struct-invalid-entry
  "不正な辞書エントリです"
  'nskk-dict-struct-error)

;;; エラー情報構造

(cl-defstruct (nskk-dict-error-info
               (:constructor nskk-dict-error-info--create)
               (:copier nil))
  "エラー情報オブジェクト。

スロット:
  type      - エラー型シンボル
  message   - エラーメッセージ
  data      - エラーデータ（任意）
  timestamp - 発生時刻"
  (type nil :type symbol)
  (message nil :type string)
  (data nil :type list)
  (timestamp nil :type float))

;;; フォールバック辞書

(defvar nskk-dict-errors--builtin-dict
  '(;; 基本的なひらがな
    ("あ" ("亜" "阿" "吾"))
    ("い" ("以" "伊" "位"))
    ("う" ("宇" "羽" "右"))
    ("え" ("江" "絵" "恵"))
    ("お" ("於" "尾" "御"))
    ("か" ("可" "仮" "加"))
    ("き" ("木" "気" "基"))
    ("く" ("区" "句" "苦"))
    ("け" ("家" "毛" "化"))
    ("こ" ("子" "個" "古"))
    ("さ" ("左" "差" "沙"))
    ("し" ("四" "子" "詩"))
    ("す" ("図" "須" "数"))
    ("せ" ("世" "瀬" "施"))
    ("そ" ("素" "祖" "阻"))
    ("た" ("田" "他" "多"))
    ("ち" ("地" "値" "知"))
    ("つ" ("津" "通" "都"))
    ("て" ("手" "天" "転"))
    ("と" ("戸" "都" "土"))
    ("な" ("名" "那" "奈"))
    ("に" ("二" "荷" "似"))
    ("ぬ" ("奴" "怒" "努"))
    ("ね" ("根" "値" "寝"))
    ("の" ("之" "乃" "野"))
    ("は" ("歯" "派" "波"))
    ("ひ" ("日" "火" "費"))
    ("ふ" ("府" "不" "符"))
    ("へ" ("辺" "戸" "平"))
    ("ほ" ("帆" "保" "歩"))
    ("ま" ("真" "間" "摩"))
    ("み" ("見" "美" "三"))
    ("む" ("無" "務" "夢"))
    ("め" ("目" "芽" "女"))
    ("も" ("毛" "藻" "喪"))
    ("や" ("屋" "也" "夜"))
    ("ゆ" ("由" "遊" "湯"))
    ("よ" ("与" "余" "世"))
    ("ら" ("良" "羅" "等"))
    ("り" ("利" "理" "里"))
    ("る" ("留" "流" "類"))
    ("れ" ("例" "礼" "麗"))
    ("ろ" ("路" "炉" "露"))
    ("わ" ("和" "話" "輪"))
    ("を" ("於" "遠"))
    ("ん" ("无"))
    ;; 基本的な単語
    ("かんじ" ("漢字"))
    ("にほん" ("日本"))
    ("ひらがな" ("平仮名"))
    ("かたかな" ("片仮名"))
    ("じしょ" ("辞書"))
    ("へんかん" ("変換"))
    ("にゅうりょく" ("入力")))
  "組み込みの最小限辞書データ。
フォーマット: ((見出し語 (候補1 候補2 ...)) ...)")

(defvar nskk-dict-errors--fallback-active nil
  "フォールバックモードが有効かどうか。")

;;; ログ管理

(defvar nskk-dict-errors--log-buffer "*NSKK Error Log*"
  "エラーログバッファ名。")

(defvar nskk-dict-errors--log-entries nil
  "エラーログエントリのリスト。")

(defun nskk-dict-errors-log (severity type message &optional data)
  "エラーをログに記録する。

引数:
  SEVERITY - 'debug/'info/'warning/'error/'fatal
  TYPE     - エラー型シンボル
  MESSAGE  - エラーメッセージ
  DATA     - 追加データ（オプション）"
  (when nskk-dict-errors-enable-logging
    (let* ((timestamp (float-time))
           (entry (list :timestamp timestamp
                       :severity severity
                       :type type
                       :message message
                       :data data)))
      ;; メモリに保存
      (push entry nskk-dict-errors--log-entries)

      ;; ファイルに追記
      (when nskk-dict-errors-log-file
        (condition-case err
            (let ((log-file (expand-file-name nskk-dict-errors-log-file))
                  (log-dir (file-name-directory
                           (expand-file-name nskk-dict-errors-log-file))))
              ;; ログディレクトリ作成
              (unless (file-directory-p log-dir)
                (make-directory log-dir t))
              ;; ログ追記
              (with-temp-buffer
                (insert (format "[%s] [%s] %s: %s"
                              (format-time-string "%Y-%m-%d %H:%M:%S"
                                                 (seconds-to-time timestamp))
                              (upcase (symbol-name severity))
                              type
                              message))
                (when data
                  (insert (format " - Data: %S" data)))
                (insert "\n")
                (append-to-file (point-min) (point-max) log-file)))
          (error
           ;; ログ書き込みエラーは無視（無限ループ防止）
           (message "Failed to write error log: %s" (error-message-string err))))))))

;;;###autoload
(defun nskk-dict-errors-show-log ()
  "エラーログを表示する。"
  (interactive)
  (let ((buffer (get-buffer-create nskk-dict-errors--log-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "NSKK Error Log\n")
      (insert "==============\n\n")
      (if (null nskk-dict-errors--log-entries)
          (insert "No errors logged.\n")
        (dolist (entry (reverse nskk-dict-errors--log-entries))
          (insert (format "[%s] [%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (seconds-to-time (plist-get entry :timestamp)))
                        (upcase (symbol-name (plist-get entry :severity)))
                        (plist-get entry :type)
                        (plist-get entry :message)))
          (when (plist-get entry :data)
            (insert (format "  Data: %S\n" (plist-get entry :data))))
          (insert "\n")))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

;;;###autoload
(defun nskk-dict-errors-clear-log ()
  "エラーログをクリアする。"
  (interactive)
  (setq nskk-dict-errors--log-entries nil)
  (when (get-buffer nskk-dict-errors--log-buffer)
    (kill-buffer nskk-dict-errors--log-buffer))
  (when (and nskk-dict-errors-log-file
            (file-exists-p nskk-dict-errors-log-file))
    (delete-file nskk-dict-errors-log-file))
  (message "Error log cleared"))

;;; エラーヘルパー関数

(defun nskk-dict-errors-create (type message &optional data)
  "エラー情報オブジェクトを作成する。

引数:
  TYPE    - エラー型シンボル
  MESSAGE - エラーメッセージ
  DATA    - エラーデータ（オプション）

戻り値:
  nskk-dict-error-info構造体"
  (nskk-dict-error-info--create
   :type type
   :message message
   :data data
   :timestamp (float-time)))

(defun nskk-dict-errors-format-user-message (error-info)
  "ユーザー向けのエラーメッセージをフォーマットする。

引数:
  ERROR-INFO - エラー型シンボルまたはnskk-dict-error-info構造体

戻り値:
  フォーマットされたメッセージ文字列"
  (let ((type (if (nskk-dict-error-info-p error-info)
                 (nskk-dict-error-info-type error-info)
               error-info))
        (data (when (nskk-dict-error-info-p error-info)
               (nskk-dict-error-info-data error-info))))
    (pcase type
      ('nskk-dict-io-file-not-found
       (format "辞書ファイルが見つかりません: %s\nフォールバック辞書を使用します。\n設定を確認してください: M-x customize-group RET nskk-dict-errors RET"
              (or (car data) "不明")))
      ('nskk-dict-io-permission-denied
       (format "辞書ファイルへのアクセス権限がありません: %s\n読み取り専用モードで動作します。"
              (or (car data) "不明")))
      ('nskk-dict-io-checksum-mismatch
       (format "辞書ファイルのチェックサムが一致しません: %s\nファイルが破損している可能性があります。\nバックアップから復旧を試みます。"
              (or (car data) "不明")))
      ('nskk-dict-io-backup-failed
       "辞書のバックアップに失敗しました。\nディスク容量を確認してください。")
      ('nskk-dict-parse-invalid-format
       (format "辞書フォーマットが不正です: %s\nバックアップから復旧を試みます。"
              (or (car data) "不明")))
      ('nskk-dict-parse-encoding-error
       (format "エンコーディングエラーが発生しました: %s\n別のエンコーディングで再試行します。"
              (or (car data) "不明")))
      ('nskk-dict-struct-invalid-entry
       (format "不正な辞書エントリが見つかりました: %s\nエントリをスキップして続行します。"
              (or (car data) "不明")))
      (_
       (format "辞書エラーが発生しました: %s"
              (if (nskk-dict-error-info-p error-info)
                  (nskk-dict-error-info-message error-info)
                (symbol-name type)))))))

(defun nskk-dict-errors-notify (error-info)
  "ユーザーにエラーを通知する。

引数:
  ERROR-INFO - エラー型シンボルまたはnskk-dict-error-info構造体"
  (let ((message-text (nskk-dict-errors-format-user-message error-info)))
    (pcase nskk-dict-errors-notification-method
      ('message
       (message "%s" message-text))
      ('popup
       ;; 将来実装: ポップアップ表示
       (message "%s" message-text))
      ('silent
       nil))))

;;; フォールバック辞書機能

(defun nskk-dict-errors-create-fallback-index ()
  "フォールバック辞書のインデックスを作成する。

優先順位:
1. nskk-dict-errors-fallback-dict-path が設定されていればそれを使用
2. 組み込み辞書を使用

戻り値:
  nskk-dict-index構造体（nskk-dict-struct.el）"
  (require 'nskk-dict-struct)

  (nskk-dict-errors-log 'info 'fallback-dict
                       "Creating fallback dictionary index")
  (setq nskk-dict-errors--fallback-active t)

  (if (and nskk-dict-errors-fallback-dict-path
          (file-exists-p nskk-dict-errors-fallback-dict-path))
      ;; カスタムフォールバック辞書を使用
      (condition-case err
          (progn
            (require 'nskk-dict-io)
            (nskk-dict-errors-log 'info 'fallback-dict
                                 (format "Loading custom fallback: %s"
                                        nskk-dict-errors-fallback-dict-path))
            (nskk-load-dictionary nskk-dict-errors-fallback-dict-path))
        (error
         (nskk-dict-errors-log 'warning 'fallback-dict
                              (format "Failed to load custom fallback, using built-in: %s"
                                     (error-message-string err)))
         (nskk-dict-errors--create-builtin-index)))
    ;; 組み込み辞書を使用
    (nskk-dict-errors--create-builtin-index)))

(defun nskk-dict-errors--create-builtin-index ()
  "組み込み辞書からインデックスを作成する。

戻り値:
  nskk-dict-index構造体"
  (require 'nskk-dict-struct)

  (let ((index (nskk-dict-index--create
               :okuri-ari-table (make-hash-table :test 'equal)
               :okuri-nasi-table (make-hash-table :test 'equal))))
    (dolist (entry-data nskk-dict-errors--builtin-dict)
      (let* ((midashi (car entry-data))
             (words (cadr entry-data))
             ;; 候補リストを作成
             (candidates (mapcar (lambda (word)
                                  (nskk-dict-candidate--create
                                   :word word
                                   :annotation nil
                                   :score 0))
                                words))
             ;; エントリを作成（全スロットを明示的に指定）
             (entry (nskk-dict-entry--create
                    :midashi midashi
                    :candidates candidates)))
        ;; インデックスに追加
        (puthash midashi entry
                (nskk-dict-index-okuri-nasi-table index))))

    (nskk-dict-errors-log 'info 'fallback-dict
                         (format "Built-in dictionary loaded: %d entries"
                                (hash-table-count
                                 (nskk-dict-index-okuri-nasi-table index))))
    index))

(defun nskk-dict-errors-is-fallback-active-p ()
  "現在フォールバックモードか判定する。

戻り値:
  フォールバックモードの場合t、それ以外nil"
  nskk-dict-errors--fallback-active)

;;; リカバリー機構

(defun nskk-dict-errors--recover-from-backup (file-path)
  "バックアップから辞書を復旧する。

引数:
  FILE-PATH - 復旧対象のファイルパス

戻り値:
  (success . result) または (failure . error-info)"
  (require 'nskk-dict-io)

  (nskk-dict-errors-log 'info 'recovery
                       (format "Attempting recovery from backup: %s" file-path))

  (condition-case err
      (let* ((backup-dir (expand-file-name nskk-dict-io-backup-dir))
             (basename (file-name-nondirectory file-path))
             (backup-files (when (file-directory-p backup-dir)
                            (directory-files backup-dir t
                                           (concat "^" (regexp-quote basename)
                                                  "\\..*\\.bak$")))))
        (if (null backup-files)
            (cons 'failure
                 (nskk-dict-errors-create 'nskk-dict-io-backup-failed
                                         "No backup files found"
                                         (list file-path)))
          ;; 最新のバックアップを使用
          (let ((latest-backup (car (sort backup-files
                                         (lambda (a b)
                                           (time-less-p
                                            (file-attribute-modification-time
                                             (file-attributes b))
                                            (file-attribute-modification-time
                                             (file-attributes a))))))))
            (nskk-dict-errors-log 'info 'recovery
                                 (format "Restoring from: %s" latest-backup))
            (copy-file latest-backup file-path t)
            (cons 'success file-path))))
    (error
     (cons 'failure
          (nskk-dict-errors-create 'nskk-dict-io-backup-failed
                                  (error-message-string err)
                                  (list file-path))))))

(defun nskk-dict-errors--retry-with-encoding (file-path encoding)
  "別のエンコーディングで辞書読み込みを再試行する。

引数:
  FILE-PATH - ファイルパス
  ENCODING  - エンコーディング ('utf-8 または 'euc-jp)

戻り値:
  (success . index) または (failure . error-info)"
  (require 'nskk-dict-parser)

  (nskk-dict-errors-log 'info 'recovery
                       (format "Retrying with encoding %s: %s" encoding file-path))

  (condition-case err
      (let* ((coding-system-for-read encoding)
             (parsed (nskk-parse-dictionary file-path))
             (index (nskk-dict-struct-from-parser parsed)))
        (cons 'success index))
    (error
     (cons 'failure
          (nskk-dict-errors-create 'nskk-dict-parse-encoding-error
                                  (error-message-string err)
                                  (list file-path encoding))))))

(defun nskk-dict-errors-auto-recover (error dict-path)
  "エラーから自動的にリカバリーを試みる。

引数:
  ERROR     - エラーシンボルまたはエラーリスト
  DICT-PATH - 辞書ファイルパス

戻り値:
  (success . result) または (failure . error-info)

リカバリー戦略:
- ファイルなし          → フォールバック辞書を使用
- パースエラー          → バックアップから復旧
- チェックサム不一致    → バックアップから復旧
- エンコーディングエラー → 別エンコーディングで再試行
- その他                → フォールバック辞書を使用"
  (let ((error-type (if (listp error) (car error) error)))
    (nskk-dict-errors-log 'warning 'auto-recovery
                         (format "Auto-recovery started for: %s (error: %s)"
                                dict-path error-type))

    (pcase error-type
      ;; ファイルなし → フォールバック辞書
      ('nskk-dict-io-file-not-found
       (nskk-dict-errors-log 'info 'auto-recovery
                            "Using fallback dictionary (file not found)")
       (cons 'success (nskk-dict-errors-create-fallback-index)))

      ;; パースエラー → バックアップから復旧
      ((or 'nskk-dict-parse-error 'nskk-dict-parse-invalid-format)
       (let ((result (nskk-dict-errors--recover-from-backup dict-path)))
         (if (eq (car result) 'success)
             ;; バックアップから復旧成功 → 再読み込み
             (condition-case err
                 (progn
                   (require 'nskk-dict-io)
                   (cons 'success (nskk-load-dictionary dict-path)))
               (error
                ;; 再読み込みも失敗 → フォールバック
                (nskk-dict-errors-log 'warning 'auto-recovery
                                     "Backup recovery failed, using fallback")
                (cons 'success (nskk-dict-errors-create-fallback-index))))
           ;; バックアップなし → フォールバック
           (nskk-dict-errors-log 'warning 'auto-recovery
                                "No backup available, using fallback")
           (cons 'success (nskk-dict-errors-create-fallback-index)))))

      ;; チェックサム不一致 → バックアップから復旧
      ('nskk-dict-io-checksum-mismatch
       (let ((result (nskk-dict-errors--recover-from-backup dict-path)))
         (if (eq (car result) 'success)
             (condition-case err
                 (progn
                   (require 'nskk-dict-io)
                   (cons 'success (nskk-load-dictionary dict-path t)))
               (error
                (nskk-dict-errors-log 'warning 'auto-recovery
                                     "Recovery failed, using fallback")
                (cons 'success (nskk-dict-errors-create-fallback-index))))
           (nskk-dict-errors-log 'warning 'auto-recovery
                                "No backup available, using fallback")
           (cons 'success (nskk-dict-errors-create-fallback-index)))))

      ;; エンコーディングエラー → 別エンコーディングで再試行
      ('nskk-dict-parse-encoding-error
       (let ((encodings '(utf-8 euc-jp)))
         (cl-loop for encoding in encodings
                 for result = (nskk-dict-errors--retry-with-encoding dict-path encoding)
                 when (eq (car result) 'success)
                 return result
                 finally return (cons 'success (nskk-dict-errors-create-fallback-index)))))

      ;; その他 → フォールバック辞書
      (_
       (nskk-dict-errors-log 'info 'auto-recovery
                            (format "Using fallback dictionary (unknown error: %s)" error-type))
       (cons 'success (nskk-dict-errors-create-fallback-index))))))

;;; リカバリー付き辞書操作

;;;###autoload
(defun nskk-dict-errors-load-with-recovery (dict-path)
  "エラーリカバリー付きで辞書を読み込む。

引数:
  DICT-PATH - 辞書ファイルパス

戻り値:
  nskk-dict-index構造体（失敗時はフォールバック辞書）"
  (require 'nskk-dict-io)

  (setq nskk-dict-errors--fallback-active nil)

  (condition-case err
      (progn
        (nskk-dict-errors-log 'info 'load "Loading dictionary" (list dict-path))
        (nskk-load-dictionary dict-path))

    ;; NSKKカスタムエラー
    (nskk-dict-error
     (nskk-dict-errors-log 'error (car err)
                          (error-message-string err)
                          (cdr err))
     (nskk-dict-errors-notify (car err))
     (if nskk-dict-errors-auto-recovery
         (let ((recovery-result (nskk-dict-errors-auto-recover (car err) dict-path)))
           (if (eq (car recovery-result) 'success)
               (cdr recovery-result)
             ;; リカバリー失敗 → フォールバック
             (nskk-dict-errors-create-fallback-index)))
       ;; 自動リカバリー無効 → エラーを再シグナル
       (signal (car err) (cdr err))))

    ;; 一般エラー
    (error
     (nskk-dict-errors-log 'error 'general-error
                          (error-message-string err)
                          err)
     (if nskk-dict-errors-auto-recovery
         (progn
           (message "辞書読み込みエラー: %s\nフォールバック辞書を使用します。"
                   (error-message-string err))
           (nskk-dict-errors-create-fallback-index))
       (signal (car err) (cdr err))))))

(provide 'nskk-dict-errors)

;;; nskk-dict-errors.el ends here
