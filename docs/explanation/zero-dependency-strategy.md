# NSKK外部依存ゼロ戦略

## 基本方針

### 依存ゼロの利点
- **シンプルなインストール**: `require 'nskk`のみで動作
- **競合回避**: 他パッケージとの競合リスクなし
- **軽量性**: 最小限のリソース使用
- **保守性**: 外部ライブラリの変更に影響されない

### 使用可能なEmacs標準機能
```elisp
;; Emacs内蔵ライブラリ（使用可能）
(require 'subr-x)      ; 文字列・リスト操作拡張
(require 'seq)         ; シーケンス操作
(require 'map)         ; マップ操作
(require 'pcase)       ; パターンマッチング
(require 'rx)          ; 正規表現構築
(require 'cl-lib)      ; Common Lisp拡張（標準バンドル）
```

## 外部依存を避ける実装パターン

### 1. オブジェクトシステム
```elisp
;; cl-defstructの代替（plistベース）
(defmacro nskk-defstruct (name &rest slots)
  "構造体定義マクロ（外部依存なし）"
  (let ((constructor (intern (format "make-%s" name)))
        (predicate (intern (format "%s-p" name))))
    `(progn
       (defun ,constructor (&rest args)
         ,(format "%s構造体のコンストラクター" name)
         (cons ',name (nskk--plist-from-args ',slots args)))

       (defun ,predicate (obj)
         ,(format "%s構造体かどうかの判定" name)
         (and (consp obj) (eq (car obj) ',name)))

       ,@(mapcar (lambda (slot)
                   (let ((accessor (intern (format "%s-%s" name slot))))
                     `(defun ,accessor (obj)
                        ,(format "%s構造体の%sアクセサー" name slot)
                        (plist-get (cdr obj) ,(intern (format ":%s" slot))))))
                 slots))))

;; 使用例
(nskk-defstruct candidate
  text probability source)
```

### 2. ハッシュテーブル代替
```elisp
;; alistベースの高速検索
(defun nskk--alist-get (key alist &optional default)
  "alist検索（hash-table代替）"
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) default)))

(defun nskk--alist-put (key value alist)
  "alist更新（破壊的変更なし）"
  (cons (cons key value)
        (assq-delete-all key (copy-alist alist))))

;; トライ木構造（alistベース）
(defun nskk--trie-insert (trie key value)
  "トライ木への挿入"
  (if (null key)
      (nskk--alist-put :value value trie)
    (let* ((char (car key))
           (subtrie (nskk--alist-get char trie)))
      (nskk--alist-put char
                       (nskk--trie-insert subtrie (cdr key) value)
                       trie))))
```

### 3. 非同期処理
```elisp
;; run-with-timerによる非同期処理
(defvar nskk--async-tasks nil)

(defun nskk-async-run (function &optional delay)
  "非同期タスク実行"
  (let ((timer (run-with-timer (or delay 0.001) nil
                               (lambda ()
                                 (condition-case err
                                     (funcall function)
                                   (error
                                    (message "NSKK async error: %s" err)))))))
    (push timer nskk--async-tasks)
    timer))

(defun nskk-async-cancel-all ()
  "全非同期タスクのキャンセル"
  (dolist (timer nskk--async-tasks)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq nskk--async-tasks nil))
```

### 4. ファイルI/O最適化
```elisp
;; バッファベースの高速ファイル処理
(defun nskk--read-file-lines (filename)
  "ファイル行読み込み（メモリ効率重視）"
  (when (file-readable-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (split-string (buffer-string) "\n" t))))

(defun nskk--write-file-lines (filename lines)
  "ファイル行書き込み"
  (with-temp-file filename
    (dolist (line lines)
      (insert line "\n"))))

;; ストリーミング読み込み（大容量ファイル対応）
(defun nskk--process-file-streaming (filename processor)
  "ファイルのストリーミング処理"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        (funcall processor line))
      (forward-line 1))))
```

### 5. 状態管理
```elisp
;; グローバル状態管理（シングルトンパターン）
(defvar nskk--state nil
  "アプリケーション状態")

(defun nskk--state-get (key &optional default)
  "状態値取得"
  (plist-get nskk--state key default))

(defun nskk--state-set (key value)
  "状態値設定"
  (setq nskk--state (plist-put nskk--state key value)))

(defmacro nskk-with-state-lock (&rest body)
  "状態変更の排他制御"
  `(let ((inhibit-quit t))
     ,@body))
```

## パフォーマンス最適化

### メモ化パターン
```elisp
;; 関数結果のキャッシュ
(defmacro nskk-memoize (function)
  "関数のメモ化"
  (let ((cache-var (intern (format "%s--cache" function))))
    `(progn
       (defvar ,cache-var nil)
       (advice-add ',function :around
                   (lambda (orig-func &rest args)
                     (let ((cached (nskk--alist-get args ,cache-var)))
                       (if cached
                           cached
                         (let ((result (apply orig-func args)))
                           (setq ,cache-var
                                 (nskk--alist-put args result ,cache-var))
                           result))))))))
```

### 遅延評価
```elisp
;; 遅延初期化パターン
(defmacro nskk-lazy-init (var init-form)
  "遅延初期化"
  `(or ,var (setq ,var ,init-form)))

;; 使用例
(defvar nskk--dictionary nil)
(defun nskk--get-dictionary ()
  (nskk-lazy-init nskk--dictionary
                  (nskk--load-dictionary-from-file)))
```

## テスト戦略

### ミニマルテストフレームワーク
```elisp
;; ERT使用（Emacs標準）
(require 'ert)

(defmacro nskk-deftest (name &rest body)
  "NSKKテスト定義"
  `(ert-deftest ,(intern (format "nskk-test-%s" name)) ()
     ,@body))

;; 使用例
(nskk-deftest conversion-basic
  (should (equal (nskk-convert "ka") "か")))
```

この戦略により、外部依存なしでありながら高機能なSKK実装を実現できます。