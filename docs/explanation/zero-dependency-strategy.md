# NSKK外部依存ゼロ戦略（Emacs 30以上最適化版）

## 基本方針

### 依存ゼロの利点
- **シンプルなインストール**: `require 'nskk`のみで動作
- **競合回避**: 他パッケージとの競合リスクなし
- **軽量性**: 最小限のリソース使用とネイティブコンパイル効率
- **保守性**: 外部ライブラリの変更に影響されない
- **Emacs 30以上対応**: setopt（Emacs 29.1導入）や協調スレッド（Emacs 26導入）など標準APIを活用

### 使用可能なEmacs標準機能
```elisp
;; Emacs内蔵ライブラリ（Emacs 30以上対応）
(require 'subr-x)      ; 文字列・リスト操作拡張
(require 'seq)         ; シーケンス操作
(require 'map)         ; マップ操作
(require 'pcase)       ; パターンマッチング
(require 'rx)          ; 正規表現構築
(require 'cl-lib)      ; Common Lisp拡張（標準バンドル）
(require 'thread)      ; 協調スレッド（Emacs 26以降）
```

## 外部依存を避ける実装パターン

### 1. オブジェクトシステム（Emacs 30以上最適化）
```elisp
;; ネイティブコンパイル対応構造体定義
(defmacro nskk-defstruct (name &rest slots)
  "構造体定義マクロ（defsubstによるインライン化最適化）"
  (declare (indent 1))
  (let ((constructor (intern (format "make-%s" name)))
        (predicate (intern (format "%s-p" name))))
    `(progn
       (defsubst ,constructor (&rest args)  ; defsubstでインライン化
         ,(format "%s構造体のコンストラクター" name)
         (cons ',name (nskk--plist-from-args ',slots args)))

       (defsubst ,predicate (obj)  ; 高速判定のためdefsubst
         ,(format "%s構造体かどうかの判定" name)
         (and (consp obj) (eq (car obj) ',name)))

       ,@(mapcar (lambda (slot)
                   (let ((accessor (intern (format "%s-%s" name slot))))
                     `(defsubst ,accessor (obj)  ; アクセサーもインライン化
                        ,(format "%s構造体の%sアクセサー" name slot)
                        (plist-get (cdr obj) ,(intern (format ":%s" slot))))))
                 slots))))

;; 使用例
(nskk-defstruct candidate
  text probability source)

;; 候補の逐次処理（mapcar使用）
;; 注意: Emacsのスレッドは協調的（GILあり）のため、CPU集約的な処理を
;; スレッド分割してもパフォーマンスは向上しない。候補処理はmapcarで十分。
(defun nskk--process-candidates (candidates processor)
  "候補をPROCESSORで処理する。"
  (mapcar processor candidates))
```

### 2. ハッシュテーブル代替（Emacs 30以上最適化）
```elisp
;; alistベースの高速検索（ネイティブコンパイル最適化）
(defsubst nskk--alist-get (key alist &optional default)
  "alist検索（hash-table代替、インライン化）"
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) default)))

(defsubst nskk--alist-put (key value alist)
  "alist更新（破壊的変更なし、インライン化）"
  (cons (cons key value)
        (assq-delete-all key (copy-alist alist))))

;; トライ木構造
(defun nskk--trie-insert (trie key value)
  "トライ木への挿入"
  (if (null key)
      (nskk--alist-put :value value trie)
    (let* ((char (car key))
           (subtrie (nskk--alist-get char trie)))
      (nskk--alist-put char
                       (nskk--trie-insert subtrie (cdr key) value)
                       trie))))

;; obarray最適化
(defvar nskk--symbol-cache (obarray-make 1024)
  "シンボルキャッシュ（obarray使用）")

(defsubst nskk--intern-cached (name)
  "キャッシュ済みシンボル取得"
  (intern name nskk--symbol-cache))
```

### 3. 非同期処理（Emacs協調スレッド）
```elisp
;; Emacsの協調スレッドによる並行処理（Emacs 26以降で利用可能）
;; 重要: Emacsスレッドは協調的（GILあり）であり、真の並列実行ではない。
;; I/O待ちの重ね合わせによる効率化に有効。

(defvar nskk--thread-pool nil
  "スレッドプール")

(defvar nskk--max-threads 4
  "最大スレッド数")

(defun nskk-async-run-native (function)
  "協調スレッドによる非同期実行。
I/O待ちを含む処理に有効。CPU集約処理には効果がない。"
  (let ((thread (make-thread
                 (lambda ()
                   (condition-case err
                       (funcall function)
                     (error
                      (message "NSKK thread error: %s" err)))))))
    (push thread nskk--thread-pool)
    thread))

(defun nskk-async-run-timer (function &optional delay)
  "タイマーベース非同期処理（スレッドを使わない代替手段）"
  (run-with-timer (or delay 0.001) nil
                  (lambda ()
                    (condition-case err
                        (funcall function)
                      (error
                       (message "NSKK async error: %s" err))))))

;; Promise風実装（thread-joinで結果を受け取る）
(defmacro nskk-async-then (async-expr then-fn &optional catch-fn)
  "Promise風非同期チェーン。
thread-joinはスレッド完了まで（協調的に）ブロックする。"
  (let ((thread-sym (gensym "thread"))
        (result-sym (gensym "result")))
    `(let ((,thread-sym ,async-expr))
       (make-thread
        (lambda ()
          (condition-case err
              (let ((,result-sym (thread-join ,thread-sym)))
                (funcall ,then-fn ,result-sym))
            (error
             (when ,catch-fn
               (funcall ,catch-fn err)))))))))

(defun nskk-async-cancel-all ()
  "全スレッドにquitシグナルを送信し、終了を待機する。
thread-joinはタイムアウト引数を受け付けないため、
シグナルによる終了を試みる。"
  (dolist (thread nskk--thread-pool)
    (when (thread-live-p thread)
      (condition-case nil
          (progn
            (thread-signal thread 'quit nil)
            (thread-join thread))
        (error nil))))
  (setq nskk--thread-pool nil))
```

### 4. ファイルI/O最適化
```elisp
;; ファイルI/O処理の効率化
(defun nskk--read-file-lines (filename)
  "ファイル行読み込み（ネイティブコンパイル最適化）"
  (when (file-readable-p filename)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))  ; 明示的エンコーディング
        (insert-file-contents filename))
      (split-string (buffer-string) "\n" t))))

(defun nskk--write-file-lines (filename lines)
  "ファイル行書き込み（原子的書き込み）。
with-temp-fileは一時バッファを作成し、bodyを評価後、指定ファイルに書き込む。
原子性を確保するため、一時ファイルを経由してrenameする方式を採用する。"
  (let ((temp-file (make-temp-file (file-name-nondirectory filename)))
        (coding-system-for-write 'utf-8))
    (with-temp-file temp-file
      (dolist (line lines)
        (insert line "\n")))
    (rename-file temp-file filename t)))

;; ストリーミング処理
(defun nskk--process-file-streaming (filename processor)
  "ファイルを行単位でストリーミング処理する。
Emacsスレッドは協調的（GILあり）のため、ファイル内容の行処理を
スレッド分割してもパフォーマンスは向上しない。逐次処理が適切。"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        (funcall processor line))
      (forward-line 1))))

;; file-notifyによるファイル変更監視
(defvar nskk--file-watchers nil
  "ファイル監視ハンドル")

(defun nskk--watch-file (filename callback)
  "ファイル変更監視"
  (when (file-exists-p filename)
    (let ((watcher (file-notify-add-watch
                    filename '(change) callback)))
      (push watcher nskk--file-watchers)
      watcher)))
```

### 5. 状態管理（スレッドセーフ）
```elisp
;; mutexによるスレッドセーフ状態管理
(defvar nskk--state nil
  "アプリケーション状態")

(defvar nskk--state-mutex (make-mutex)
  "状態変更用mutex")

(defsubst nskk--state-get (key &optional default)
  "状態値取得（読み込み専用、ロックなし）"
  (plist-get nskk--state key default))

(defun nskk--state-set (key value)
  "状態値設定（スレッドセーフ）"
  (with-mutex nskk--state-mutex
    (setq nskk--state (plist-put nskk--state key value))))

(defmacro nskk-with-state-lock (&rest body)
  "状態変更の排他制御（mutex使用）"
  `(with-mutex nskk--state-mutex
     ,@body))

;; setopt vs setq の使い分け
;; setopt: defcustomで定義されたユーザー向け変数にのみ使用（Emacs 29.1導入）
;; setq: defvarで定義された内部変数に使用

(defcustom nskk-enable-threading t
  "スレッド機能の有効化。"
  :type 'boolean
  :group 'nskk)

(defun nskk--configure-state ()
  "状態管理の設定。
setoptはdefcustom変数にのみ使用し、内部defvar変数にはsetqを使用する。"
  ;; defcustom変数にはsetoptを使用
  (setopt nskk-enable-threading (and (fboundp 'make-thread)
                                     nskk-enable-threading)))

;; Atomic操作のためのカウンター
(defvar nskk--atomic-counter 0
  "アトミックカウンター")

(defsubst nskk--atomic-incf ()
  "アトミック増分"
  (with-mutex nskk--state-mutex
    (cl-incf nskk--atomic-counter)))
```

## パフォーマンス最適化（Emacs 30以上対応）

### メモ化パターン（スレッドセーフ）
```elisp
;; mutexによるスレッドセーフメモ化
(defmacro nskk-memoize (function)
  "関数のメモ化（スレッドセーフ）"
  (let ((cache-var (intern (format "%s--cache" function)))
        (mutex-var (intern (format "%s--mutex" function))))
    `(progn
       (defvar ,cache-var nil)
       (defvar ,mutex-var (make-mutex))
       (advice-add ',function :around
                   (lambda (orig-func &rest args)
                     (let ((cached (nskk--alist-get args ,cache-var)))
                       (if cached
                           cached
                         (with-mutex ,mutex-var
                           ;; Double-checked locking
                           (let ((cached-again (nskk--alist-get args ,cache-var)))
                             (if cached-again
                                 cached-again
                               (let ((result (apply orig-func args)))
                                 (setq ,cache-var
                                       (nskk--alist-put args result ,cache-var))
                                 result)))))))))))

;; ハッシュテーブル使用高速メモ化
(defmacro nskk-memoize-hash (function &optional max-size)
  "ハッシュテーブル使用メモ化（LRU付き）"
  (let ((cache-var (intern (format "%s--hash-cache" function)))
        (mutex-var (intern (format "%s--hash-mutex" function)))
        (size-var (intern (format "%s--cache-size" function))))
    `(progn
       (defvar ,cache-var (make-hash-table :test 'equal))
       (defvar ,mutex-var (make-mutex))
       (defvar ,size-var 0)
       (advice-add ',function :around
                   (lambda (orig-func &rest args)
                     (let ((key (prin1-to-string args)))
                       (with-mutex ,mutex-var
                         (or (gethash key ,cache-var)
                             (let ((result (apply orig-func args)))
                               (when (and ,max-size (>= ,size-var ,max-size))
                                 ;; LRU eviction (simplified)
                                 (clrhash ,cache-var)
                                 (setq ,size-var 0))
                               (puthash key result ,cache-var)
                               (cl-incf ,size-var)
                               result)))))))))
```

### 遅延評価
```elisp
;; 遅延初期化パターン
;; 注意: setoptはdefcustom変数にのみ使用。内部defvar変数にはsetqを使用。
(defmacro nskk-lazy-init (var init-form)
  "遅延初期化マクロ。VARが未設定なら初期化する。"
  `(or ,var (setq ,var ,init-form)))

;; 協調スレッドによる遅延読み込み
(defvar nskk--dictionary nil)
(defvar nskk--dictionary-loading nil)
(defvar nskk--dictionary-mutex (make-mutex))

(defun nskk--get-dictionary ()
  "辞書の遅延読み込み（I/O待ちを協調スレッドで処理）。"
  (cond
   (nskk--dictionary nskk--dictionary)
   (nskk--dictionary-loading
    ;; 読み込み中の場合は待機（thread-joinは引数1つのみ）
    (thread-join nskk--dictionary-loading)
    nskk--dictionary)
   (t
    (with-mutex nskk--dictionary-mutex
      (unless nskk--dictionary
        (setq nskk--dictionary-loading
              (nskk-async-run-native
               (lambda ()
                 ;; 内部defvar変数にはsetqを使用
                 (setq nskk--dictionary
                       (nskk--load-dictionary-from-file))
                 (setq nskk--dictionary-loading nil)))))
      (when nskk--dictionary-loading
        (thread-join nskk--dictionary-loading))
      nskk--dictionary))))
```

## テスト戦略（Emacs 30以上対応）

### ミニマルテストフレームワーク
```elisp
;; ERT使用（Emacs標準テストフレームワーク）
(require 'ert)

(defmacro nskk-deftest (name &rest body)
  "NSKKテスト定義マクロ。"
  `(ert-deftest ,(intern (format "nskk-test-%s" name)) ()
     ,@body))

;; テスト実行
(defun nskk-run-tests (&optional selector)
  "テストを実行する。"
  (interactive)
  (ert-run-tests-batch-and-exit (or selector "nskk-test-")))

;; 使用例
(nskk-deftest conversion-basic
  (should (equal (nskk-convert "ka") "か")))

;; ミニバッファUIを使ったテスト管理
(defun nskk-test-menu ()
  "NSKK テスト実行メニュー (ミニバッファ版)。"
  (interactive)
  (let* ((choices '("全テスト実行" "単体テスト" "ベンチマーク"))
         (selection (completing-read "NSKK test action: " choices nil t)))
    (pcase selection
      ("全テスト実行" (nskk-run-tests))
      ("単体テスト" (call-interactively #'ert-run-tests-interactively))
      ("ベンチマーク" (nskk-run-benchmarks)))))

(defun nskk-run-benchmarks ()
  "候補処理のベンチマーク。"
  (interactive)
  (let ((start-time (float-time)))
    (nskk--process-candidates
     (make-list 10000 "test")
     ;; string-reverseは存在しない。seq-reverseまたはreverse+string-to-listを使用。
     (lambda (x) (concat (reverse (string-to-list x)))))
    (message "処理時間: %.3f秒" (- (float-time) start-time))))
```

## まとめ

この戦略により、Emacs 30以上の標準機能を活用しながら外部依存なしで高機能なSKK実装を目指す。

### 主な方針
- **setopt**: defcustomで定義されたユーザー向け変数にsetoptを使用（Emacs 29.1導入）。内部defvar変数にはsetqを使用
- **協調スレッド**: I/O待ちの重ね合わせによる効率化（Emacs 26導入）。CPU集約処理には効果がない点に注意
- **ネイティブコンパイル**: defsubstによるインライン化。declareで有効なのは(pure t)と(side-effect-free t)のみ
- **ミニバッファUI**: completing-readによる統一的なユーザーインターフェース
- **スレッドセーフ**: mutexによる安全な並行アクセス
- **原子的操作**: ファイルI/Oと状態管理の安全性向上

これらにより、外部依存ゼロでありながらEmacs標準機能を最大限活用する設計とする。
