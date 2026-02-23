# Emacs 30以上 ベストプラクティス：NSKK実装のためのモダンEmacs Lispガイド

## エグゼクティブサマリー

Emacs 30以上の機能を活用し、外部依存ゼロで高性能を目指すNSKK実装のための包括的ベストプラクティス集です。setopt（Emacs 29.1導入）、ネイティブコンパイル、協調スレッド、ミニバッファメニュー等の機能を活用したモダンEmacs Lispプログラミング手法を提供します。

## 1. Emacs 30以上 機能の活用

### 1.1 ネイティブコンパイル（Native Compilation）

```elisp
;;; Emacs 30以上のネイティブコンパイル最適化

;; setoptによるモダンな設定（Emacs 29.1で導入）
;; 注意: setoptはdefcustomで定義された変数専用
(setopt native-comp-speed 3                          ; 最大速度最適化
        native-comp-debug 0                          ; デバッグ情報削除
        native-comp-verbose nil                      ; 冗長出力抑制
        native-comp-async-report-warnings-errors nil ; 非同期エラー報告無効
        native-comp-jit-compilation t                ; JITコンパイル有効（Emacs 29で名称変更）
        native-comp-enable-subr-trampolines t)       ; サブルーチントランポリン有効

;; declare構文で使用可能なのは (pure t) と (side-effect-free t) のみ
(defun nskk-critical-path-function (input)
  "純粋関数として宣言する最適化例。
declareで使用可能: pure, side-effect-free。
インライン化にはdefsubstまたはdefine-inlineを使用する。"
  (declare (pure t)                     ; 純粋関数指定
           (side-effect-free t))        ; 副作用なし保証
  (when (stringp input)
    (string-trim input)))

;; インライン化にはdefsubstを使用する（declareの(inline ...)は無効）
(defsubst nskk-hot-function (x)
  "defsubstによりバイトコンパイル時にインライン展開される。"
  (declare (side-effect-free t)
           (pure t))
  (* x x))

;; コンパイラマクロはdefine-compiler-macroで別途定義する
;; （declareの(compiler-macro ...)は無効）
(define-compiler-macro nskk-hot-function (&whole form x)
  (if (numberp x)
      (* x x)  ; コンパイル時定数計算
    form))

;; ネイティブコンパイルの非同期バッチ処理
;; native-compile-async: (native-compile-async FILES &optional RECURSIVELY LOAD SELECTOR)
(when (native-comp-available-p)
  (native-compile-async
   (list user-emacs-directory)
   t))    ; RECURSIVELY=t でサブディレクトリも対象
```

### 1.2 Threads（協調スレッド）

```elisp
;;; Emacsの協調スレッドシステム（Emacs 26以降で利用可能）
;;;
;;; 重要: Emacsのスレッドは「協調的」（cooperative）であり、プリエンプティブではない。
;;; GIL（Global Interpreter Lock）があるため、同時に実行されるスレッドは常に1つのみ。
;;; スレッドはI/O待ち、mutex操作、thread-yield呼び出し時にのみ切り替わる。
;;; CPU集約的な処理を複数スレッドに分割してもパフォーマンスは向上しない。
;;; スレッドはI/O待ちの並行処理（例: ネットワーク通信中に他の処理を進める）に有効。

;; スレッドプールの実装（mutex + リストによるキュー）
(defclass nskk-thread-pool ()
  ((workers :initform nil
            :type list
            :documentation "ワーカースレッドのリスト")
   (queue :initform nil
          :type list
          :documentation "タスクキュー（mutexで保護）")
   (queue-mutex :initform (make-mutex "nskk-queue-mutex")
                :type mutex
                :documentation "キュー操作の排他制御")
   (condition :initform nil
              :type t
              :documentation "タスク通知用condition-variable")
   (pool-mutex :initform (make-mutex "nskk-pool-mutex")
               :type mutex
               :documentation "プール状態保護")
   (active-count :initform 0
                 :type integer
                 :documentation "アクティブスレッド数")
   (max-workers :initarg :max-workers
                :initform 4
                :type integer
                :documentation "最大ワーカー数")))

(cl-defmethod nskk-thread-pool-initialize ((pool nskk-thread-pool))
  "スレッドプールを初期化する。"
  (oset pool condition (make-condition-variable (oref pool pool-mutex))))

(cl-defmethod nskk-thread-pool-enqueue ((pool nskk-thread-pool) task)
  "タスクをキューに追加する。"
  (with-mutex (oref pool queue-mutex)
    (setf (oref pool queue)
          (append (oref pool queue) (list task))))
  ;; condition-variableで待機中のワーカーに通知
  (with-mutex (oref pool pool-mutex)
    (condition-notify (oref pool condition))))

(cl-defmethod nskk-thread-pool-dequeue ((pool nskk-thread-pool))
  "キューからタスクを取り出す。キューが空ならnilを返す。"
  (with-mutex (oref pool queue-mutex)
    (pop (oref pool queue))))

(cl-defmethod nskk-thread-pool-execute ((pool nskk-thread-pool) task)
  "タスクをワーカースレッドで実行する。"
  (with-slots (queue-mutex pool-mutex active-count condition max-workers) pool
    ;; ワーカー数制限チェック
    (when (< active-count max-workers)
      ;; タスクをキューに追加
      (nskk-thread-pool-enqueue pool task)

      ;; ワーカースレッド生成
      (let ((worker-thread
             (make-thread
              (lambda ()
                (unwind-protect
                    (let ((current-task t))
                      (while current-task
                        (setq current-task (nskk-thread-pool-dequeue pool))
                        (when current-task
                          (condition-case err
                              (funcall current-task)
                            (error
                             (message "[NSKK Thread Pool] Worker error: %s" err))))
                        ;; キューが空なら協調的に譲る
                        (unless current-task
                          (thread-yield))))
                  ;; スレッド終了時のクリーンアップ
                  (with-mutex pool-mutex
                    (cl-decf active-count)
                    (condition-notify condition))))
              (format "nskk-worker-%d" (random 10000)))))

        ;; アクティブスレッド数更新
        (with-mutex pool-mutex
          (cl-incf active-count))

        (thread-yield)
        worker-thread))))

;; 辞書検索の協調的並行処理
;; 注意: Emacsスレッドは協調的なため、真の並列検索にはならないが、
;; I/O待ち（辞書ファイル読み込み等）を重ね合わせることで効率化できる。
(defun nskk-concurrent-dictionary-search (query)
  "複数辞書を協調スレッドで並行検索する。
I/O待ちを重ね合わせることで、逐次検索より高速化を図る。"
  (let* ((results-mutex (make-mutex "nskk-search-results"))
         (results (make-hash-table :test 'equal))
         (threads nil))

    ;; 辞書ごとにスレッドを起動
    (dolist (dict nskk-dictionaries)
      (push
       (make-thread
        (lambda ()
          (condition-case err
              (let ((dict-results (nskk-search-single-dictionary dict query)))
                (with-mutex results-mutex
                  (puthash dict dict-results results)))
            (error
             (message "[NSKK] Search error in %s: %s" dict err))))
        (format "nskk-search-%s" dict))
       threads))

    ;; 全スレッドの完了を待機
    ;; thread-joinは引数1つのみ（タイムアウトなし）
    (dolist (thread threads)
      (thread-join thread))

    results))
```

### 1.3 Minibuffer + Emacs 30以上 UIシステム（軽量UI構築）

```elisp
;;; Emacs 30以上 Minibuffer UIによるシンプルなインターフェース

(require 'nskk-config-menu)   ; NSKK標準設定メニュー

(defconst nskk-main-menu-options
  '(("設定メニューを開く" . nskk-config-menu)
    ("プラグイン管理" . nskk-plugins-menu)
    ("デバッグツール" . nskk-debug-menu)
    ("終了" . ignore))
  "メインメニューの選択肢。")

(defun nskk-main-menu ()
  "NSKK ミニバッファメニュー。"
  (interactive)
  (catch 'exit
    (while t
      (let* ((choice (completing-read
                      "NSKKメニュー: "
                      (mapcar #'car nskk-main-menu-options) nil t))
             (command (cdr (assoc choice nskk-main-menu-options))))
        (if (eq command #'ignore)
            (throw 'exit nil)
          (call-interactively command))))))

;; コンテキスト依存メニュー（簡易版）
(defun nskk-context-menu ()
  "入力状態に基づく簡易メニュー。"
  (interactive)
  (let ((choices '(("次候補" . nskk-next-candidate)
                   ("前候補" . nskk-previous-candidate)
                   ("確定" . nskk-commit)
                   ("キャンセル" . nskk-quit)
                   ("終了" . ignore)))
        selection)
    (setq selection (completing-read "Context: "
                                     (mapcar #'car choices) nil t))
    (let ((command (cdr (assoc selection choices))))
      (unless (eq command #'ignore)
        (call-interactively command)))))
```

### 1.4 JSONRPC（Emacs標準ライブラリによる外部連携）

```elisp
;;; Emacs標準のJSONRPCによる外部辞書サーバー連携
;;; jsonrpcはEmacs標準バンドルライブラリ（外部依存なし）

(require 'jsonrpc)

;; 辞書サーバークライアント実装
(defclass nskk-jsonrpc-client (jsonrpc-connection)
  ((dictionary-cache :initform (make-hash-table :test 'equal))))

(cl-defmethod nskk-jsonrpc-search ((client nskk-jsonrpc-client) query)
  "JSONRPCによる辞書検索"
  (jsonrpc-request client 'search
                   :params `(:query ,query
                            :limit 100
                            :timeout 1000)))

;; 非同期リクエスト
(cl-defmethod nskk-jsonrpc-async-search ((client nskk-jsonrpc-client)
                                          query callback)
  "非同期JSONRPC検索"
  (jsonrpc-async-request
   client 'search
   :params `(:query ,query)
   :success-fn callback
   :error-fn (lambda (err)
               (message "JSONRPC error: %s" err))))
```

## 2. マクロ活用による最適化

### 2.1 コンパイル時最適化マクロ

```elisp
;;; マクロテクニック

;; 変換ルールのコンパイル時展開
(defmacro nskk-define-conversion-rule (input output &rest options)
  "変換ルールをコンパイル時に最適化"
  (let ((optimized-code
         (nskk--optimize-conversion-code input output options)))
    `(progn
       ;; ルールを直接関数として定義
       (defsubst ,(intern (format "nskk-rule-%s" input)) ()
         ,optimized-code)
       ;; ハッシュテーブルにも登録
       (puthash ,input #',(intern (format "nskk-rule-%s" input))
                nskk-conversion-table))))

;; 使用例
(nskk-define-conversion-rule "ka" "か" :priority high)
;; → コンパイル時に最適化された関数が生成される

;; パフォーマンスクリティカルなループ展開
(defmacro nskk-unroll-loop (var from to &rest body)
  "ループ展開マクロ"
  (if (<= (- to from) 10)  ; 10回以下なら展開
      `(progn
         ,@(cl-loop for i from from below to
                    collect `(let ((,var ,i)) ,@body)))
    ;; 大きなループは通常のループ
    `(cl-loop for ,var from ,from below ,to
              do (progn ,@body))))

;; バイトコンパイル時定数畳み込み
(defmacro nskk-const-fold (&rest exprs)
  "定数式をコンパイル時に評価"
  (if (cl-every #'constantp exprs)
      (eval `(progn ,@exprs))
    `(progn ,@exprs)))
```

### 2.2 DSL（Domain Specific Language）構築

```elisp
;;; NSKKドメイン特化言語

(defmacro nskk-define-input-method (name &rest specs)
  "入力メソッド定義DSL"
  `(progn
     (defvar ,(intern (format "nskk-%s-table" name))
       (make-hash-table :test 'equal))

     ,@(mapcar
        (lambda (spec)
          (pcase spec
            (`(rule ,from ,to)
             `(puthash ,from ,to
                       ,(intern (format "nskk-%s-table" name))))
            (`(inherit ,parent)
             `(nskk--inherit-rules
               ',(intern (format "nskk-%s-table" name))
               ',(intern (format "nskk-%s-table" parent))))
            (`(modifier ,key ,func)
             `(define-key ,(intern (format "nskk-%s-map" name))
                          ,key ',func))))
        specs)

     (defun ,(intern (format "nskk-%s-convert" name)) (input)
       (gethash input ,(intern (format "nskk-%s-table" name))))))

;; 使用例
(nskk-define-input-method azik
  (inherit romaji)
  (rule "z " "　")
  (rule "z." "。")
  (rule "z," "、")
  (modifier "C-j" nskk-azik-special))
```

## 3. 外部依存ゼロの実現戦略

### 3.1 Pure Emacs Lisp実装パターン

```elisp
;;; 外部ライブラリを使わない実装テクニック

;; 独自HTTPクライアント（url.elのみ使用）
(defun nskk-http-request (url &optional callback)
  "外部依存なしのHTTPリクエスト"
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("User-Agent" . "NSKK/1.0"))))
    (if callback
        ;; 非同期リクエスト
        (url-retrieve url
                      (lambda (status)
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (funcall callback
                                 (buffer-substring (point) (point-max))))
                      nil t)
      ;; 同期リクエスト
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (buffer-substring (point) (point-max))))))

;; 独自JSON parser（json.elのみ使用）
(defun nskk-json-parse (string)
  "最適化されたJSONパーサー"
  (condition-case err
      (json-parse-string string
                         :object-type 'hash-table
                         :array-type 'vector
                         :null-object nil
                         :false-object nil)
    (json-parse-error
     (nskk--fallback-json-parse string))))

;; 独自暗号化（GnuTLS不要）
(defun nskk-simple-encrypt (text key)
  "簡易XOR暗号化"
  (let ((key-len (length key)))
    (concat
     (cl-loop for i from 0 below (length text)
              for c = (aref text i)
              for k = (aref key (mod i key-len))
              collect (logxor c k)))))
```

### 3.2 Emacs標準機能の最大活用

```elisp
;;; 標準機能の活用

;; overlayによる高度な表示制御
(defun nskk-create-candidate-overlay (start end candidates)
  "候補表示オーバーレイ"
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'category 'nskk-candidate)
    (overlay-put ov 'display
                 (propertize (car candidates)
                            'face 'nskk-candidate-face))
    (overlay-put ov 'after-string
                 (propertize
                  (format " [%s]" (mapconcat #'identity
                                             (cdr candidates) "/"))
                  'face 'nskk-annotation-face))
    ov))

;; text-propertyによるメタデータ管理
(defun nskk-annotate-text (text metadata)
  "テキストプロパティでメタデータ付与"
  (propertize text
              'nskk-metadata metadata
              'nskk-timestamp (current-time)
              'nskk-confidence (plist-get metadata :confidence)
              'help-echo (plist-get metadata :description)))

;; advice活用による既存関数拡張
(define-advice self-insert-command (:around (orig-fun &rest args) nskk-intercept)
  "NSKKによる入力インターセプト"
  (if nskk-mode
      (nskk-handle-self-insert last-command-event)
    (apply orig-fun args)))
```

## 4. パフォーマンス最適化テクニック

### 4.1 メモリ効率向上

```elisp
;;; メモリ使用量削減戦略

;; オブジェクトプール実装
(defclass nskk-object-pool ()
  ((pool :initform nil
         :type list)
   (factory :initarg :factory
            :type function)
   (reset :initarg :reset
          :type function)
   (max-size :initarg :max-size
             :initform 100
             :type integer)))

(cl-defmethod nskk-pool-acquire ((pool nskk-object-pool))
  "プールからオブジェクト取得"
  (if-let ((obj (pop (oref pool pool))))
      (progn
        (funcall (oref pool reset) obj)
        obj)
    (funcall (oref pool factory))))

(cl-defmethod nskk-pool-release ((pool nskk-object-pool) obj)
  "プールにオブジェクト返却"
  (when (< (length (oref pool pool)) (oref pool max-size))
    (push obj (oref pool pool))))

;; 文字列インターン
(defvar nskk-string-pool (make-hash-table :test 'equal))

(defun nskk-intern-string (string)
  "文字列インターン化"
  (or (gethash string nskk-string-pool)
      (puthash string string nskk-string-pool)))

;; 弱参照によるキャッシュ
(defvar nskk-weak-cache (make-hash-table :test 'equal :weakness 'value))
```

### 4.2 CPU最適化

```elisp
;;; CPU効率向上

;; ホットパス最適化（defsubstによるインライン展開）
(defsubst nskk-fast-member (item list)
  "最適化されたmember関数（defsubstでインライン化）"
  (declare (side-effect-free t))
  (catch 'found
    (while list
      (when (eq item (car list))
        (throw 'found list))
      (setq list (cdr list)))
    nil))

;; ビット演算活用
(defsubst nskk-fast-hash (string)
  "高速ハッシュ関数"
  (let ((hash 5381))
    (cl-loop for char across string
             do (setq hash (logxor (ash hash 5) hash char)))
    (logand hash #x7FFFFFFF)))

;; 注意: Emacsスレッドは協調的（GILあり）のため、CPU集約的な処理を
;; スレッド分割してもパフォーマンスは向上しない。
;; 以下はI/O待ちを含む処理向けの並行map実装。
(defun nskk-concurrent-map (function list)
  "I/O待ちを含む処理を協調スレッドで並行実行する。
CPU集約的な処理には効果がない点に注意。"
  (let* ((results-mutex (make-mutex "nskk-map-results"))
         (results (make-hash-table :test 'eql))
         (threads nil))
    (cl-loop for item in list
             for idx from 0
             do (push (make-thread
                       (lambda ()
                         (let ((result (funcall function item)))
                           (with-mutex results-mutex
                             (puthash idx result results)))))
                      threads))
    (dolist (thread threads)
      (thread-join thread))
    (cl-loop for idx from 0 below (length list)
             collect (gethash idx results))))
```

## 5. コーディング規約とスタイルガイド

### 5.1 命名規則

```elisp
;;; NSKKプロジェクト命名規則

;; パッケージプレフィックス（Emacs標準規約）
;; nskk-     : 公開API
;; nskk--    : 内部関数・内部マクロ（private）

;; 変数命名
(defvar nskk-public-variable nil
  "公開変数：ユーザーが設定可能")

(defvar nskk--private-variable nil
  "内部変数：直接触らない")

(defconst nskk-constant-value 42
  "定数：変更不可")

;; 関数命名パターン
(defun nskk-verb-noun ()
  "動詞-名詞パターン：アクションを表す"
  )

(defun nskk-noun-p (obj)
  "述語：真偽値を返す関数は-p suffix"
  )

(defun nskk-noun->other-noun (input)
  "変換関数：->記法"
  )

;; カスタム変数
(defcustom nskk-enable-feature t
  "機能有効化フラグ"
  :type 'boolean
  :group 'nskk
  :set (lambda (sym val)
         (set-default sym val)
         (when (boundp 'nskk-mode)
           (nskk--refresh-feature))))
```

### 5.2 ドキュメント規約

```elisp
;;; ドキュメント記述標準

(defun nskk-example-function (input &optional flag &rest args)
  "一行要約：関数の目的を簡潔に記述。

詳細説明：
この関数は入力INPUTを処理し、結果を返します。
FLAGが非nilの場合、特別な処理を行います。
ARGSは追加のオプション引数です。

引数：
  INPUT -- 処理対象の文字列
  FLAG  -- (optional) 処理モードフラグ
  ARGS  -- (optional) 追加オプション

戻り値：
  処理結果の文字列、またはnil

使用例：
  (nskk-example-function \"test\")
  => \"TEST\"

関連：
  `nskk-related-function', `nskk-another-function'"
  (when flag
    (setq input (upcase input)))
  (apply #'concat input args))

;; マクロのドキュメント
(defmacro nskk-with-environment (env &rest body)
  "環境ENV内でBODYを実行。

\(fn ENV BODY...)

このマクロは以下のように展開されます：

  (nskk-with-environment ((var1 val1) (var2 val2))
    body-forms...)

=>
  (let ((var1 val1)
        (var2 val2))
    body-forms...)"
  (declare (indent 1) (debug (sexp body)))
  `(let ,env ,@body))
```

## 6. エラーハンドリングとデバッグ

### 6.1 堅牢なエラーハンドリング

```elisp
;;; エラーハンドリングパターン

;; カスタムエラー定義
(define-error 'nskk-error "NSKK Error" 'error)
(define-error 'nskk-conversion-error "Conversion Error" 'nskk-error)
(define-error 'nskk-dictionary-error "Dictionary Error" 'nskk-error)

;; エラーハンドリングマクロ
(defmacro nskk-with-error-handling (&rest body)
  "エラーを適切に処理"
  `(condition-case err
       (progn ,@body)
     (nskk-conversion-error
      (nskk--handle-conversion-error err))
     (nskk-dictionary-error
      (nskk--handle-dictionary-error err))
     (error
      (nskk--handle-generic-error err))))

;; リトライ機構
(defmacro nskk-with-retry (max-attempts delay &rest body)
  "リトライ機構付き実行"
  (let ((attempt (gensym))
        (result (gensym))
        (err (gensym)))
    `(let ((,attempt 0)
           (,result nil))
       (while (< ,attempt ,max-attempts)
         (condition-case ,err
             (progn
               (setq ,result (progn ,@body))
               (setq ,attempt ,max-attempts))
           (error
            (cl-incf ,attempt)
            (when (< ,attempt ,max-attempts)
              (sleep-for ,delay)
              (message "Retry %d/%d: %s"
                      ,attempt ,max-attempts
                      (error-message-string ,err))))))
       ,result)))
```

### 6.2 デバッグ支援

```elisp
;;; デバッグユーティリティ

;; トレースマクロ
(defmacro nskk-trace (expr)
  "式の評価をトレース"
  `(let ((result ,expr))
     (message "TRACE: %s => %s" ',expr result)
     result))

;; プロファイリング
(defmacro nskk-profile (name &rest body)
  "実行時間計測"
  `(let ((start-time (current-time)))
     (prog1
         (progn ,@body)
       (message "Profile [%s]: %.3f ms"
               ,name
               (* 1000 (float-time
                       (time-subtract (current-time)
                                     start-time)))))))

;; アサーション
(defmacro nskk-assert (condition &optional message)
  "開発時アサーション"
  (when nskk-debug-mode
    `(unless ,condition
       (error "Assertion failed: %s"
              ,(or message (format "%s" condition))))))
```

## 7. テスト駆動開発

### 7.1 単体テストフレームワーク

```elisp
;;; ERTを活用したテスト

(require 'ert)

;; テストヘルパー
(defmacro nskk-deftest (name &rest body)
  "NSKKテスト定義マクロ"
  `(ert-deftest ,(intern (format "nskk-test-%s" name)) ()
     (nskk-with-test-environment
       ,@body)))

(defmacro nskk-with-test-environment (&rest body)
  "テスト環境セットアップ"
  `(let ((nskk-test-mode t)
         (nskk-dictionaries (nskk--create-test-dictionaries))
         (nskk-cache (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (nskk--cleanup-test-environment))))

;; 実際のテスト例
(nskk-deftest basic-conversion
  "基本変換のテスト"
  (should (equal (nskk-convert "aiueo") "あいうえお"))
  (should (equal (nskk-convert "kanji") "かんじ"))
  (should-not (nskk-convert "invalid")))

;; プロパティベーステスト
(defun nskk-generate-random-input ()
  "ランダム入力生成"
  (apply #'string
         (cl-loop repeat (1+ (random 20))
                  collect (+ ?a (random 26)))))

(nskk-deftest property-based-conversion
  "プロパティベース変換テスト"
  (cl-loop repeat 100
           for input = (nskk-generate-random-input)
           do (should (stringp (nskk-convert input)))))
```

## 8. 最適化チェックリスト

### 8.1 パフォーマンス確認項目

```elisp
;;; パフォーマンス最適化項目

;; ✅ ネイティブコンパイル有効化
(cl-assert (native-comp-available-p))

;; ✅ 適切なコンパイルフラグ
(cl-assert (= native-comp-speed 3))

;; ✅ ホットパス関数のインライン化
(cl-assert (subrp (symbol-function 'nskk-hot-function)))

;; ✅ マクロの適切な使用
(cl-assert (macrop 'nskk-define-conversion-rule))

;; ✅ キャッシュの実装
(cl-assert (hash-table-p nskk-cache))

;; ✅ 文字列インターン
(cl-assert (eq (nskk-intern-string "test")
              (nskk-intern-string "test")))

;; ✅ オブジェクトプール
(cl-assert (nskk-object-pool-p nskk-candidate-pool))

;; ✅ 非同期処理
(cl-assert (threadp (nskk-async-operation test)))

;; ✅ メモリ効率
(cl-assert (< (nskk-memory-usage) (* 20 1024 1024))) ; 20MB以下

;; ✅ 応答時間
(cl-assert (< (nskk-measure-response-time) 1.0)) ; 1ms以下
```

## 9. 結論：Emacs Lisp実装のポイント

NSKKは、以下のベストプラクティスにより、Emacs Lispの機能を活用します：

### 技術的特徴
1. **Emacs 30以上の機能の活用**
2. **マクロによる最適化**
3. **外部依存ゼロの実装**

### パフォーマンス向上
1. **ネイティブコンパイル最適化**
2. **協調スレッドによるI/O並行処理の活用**
3. **メモリ効率向上**

### 品質保証
1. **テスト戦略**
2. **エラーハンドリング**
3. **パフォーマンス監視**

これらのベストプラクティスにより、NSKKは実用的な日本語入力システムを実現します。
