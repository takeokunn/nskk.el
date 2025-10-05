# NSKKカスタマイゼーションガイド

## 概要

NSKKのカスタマイゼーション手法を解説します。個人の入力スタイルに合わせた設定、プラグイン開発、パフォーマンスチューニングまで、カスタマイゼーション技術を習得できます。

## 目次

1. [個人設定](#個人設定)
2. [カスタム変換ルール](#カスタム変換ルール)
3. [拡張機能開発](#拡張機能開発)
4. [パフォーマンスチューニング](#パフォーマンスチューニング)
5. [フック活用](#フック活用)
6. [プラグインシステム](#プラグインシステム)

## 個人設定

### プロファイル別設定管理

個人の用途に応じて複数のプロファイルを管理：

```elisp
;; Emacs 30以上強化プロファイル設定システム（動的切り替え・コンテキスト適応）
(defcustom nskk-profiles
  `((coding . ((nskk-dictionary-path . ,(expand-file-name "~/dict/programming.dic"))
               (nskk-enable-technical-terms . t)
               (nskk-candidate-display-count . 5)
               (nskk-thread-pool-size . ,(max 2 (/ (num-processors) 2)))
               (nskk-performance-profile . 'speed)
               (nskk-completion-backend . 'company-nskk-technical)))
    (writing . ((nskk-dictionary-path . ,(expand-file-name "~/dict/literary.dic"))
                (nskk-enable-formal-style . t)
                (nskk-candidate-display-count . 10)
                (nskk-context-awareness . 'high)
                (nskk-performance-profile . 'accuracy)
                (nskk-ai-suggestion . t)))
    (casual . ((nskk-dictionary-path . ,(expand-file-name "~/dict/casual.dic"))
               (nskk-enable-emoji . t)
               (nskk-abbreviation-mode . t)
               (nskk-social-context . t)
               (nskk-performance-profile . 'balanced))))
  "Emacs 30以上強化NSKKプロファイル設定（コンテキスト適応型）"
  :group 'nskk
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol
                                   :value-type sexp))

(defun nskk-load-profile (profile-name &optional force-reload)
  "Emacs 30以上のスマートプロファイル読み込み（非同期・状態保持・ロールバック対応）"
  (interactive
   (list (intern (completing-read
                  "Profile: "
                  (mapcar (lambda (profile)
                            (let ((name (symbol-name (car profile)))
                                  (desc (nskk-get-profile-description (car profile))))
                              (format "%s (%s)" name desc)))
                          nskk-profiles)
                  nil t nil nil
                  (symbol-name nskk-current-profile)))
         current-prefix-arg))

  (let* ((config (alist-get profile-name nskk-profiles))
         (previous-profile nskk-current-profile)
         (rollback-config (when previous-profile
                            (nskk-capture-current-config)))
         (progress-reporter (make-progress-reporter
                             (format "Loading profile '%s'" profile-name)
                             0 (length config))))

    (unwind-protect
        (progn
          ;; 非同期設定適用（UIフリーズ回避）
          (nskk-async-apply-config
           config
           :progress-callback
           (lambda (step total)
             (progress-reporter-update progress-reporter step)
             (when (= step total)
               (progress-reporter-done progress-reporter)))
           :error-callback
           (lambda (err setting)
             (message "[NSKK] Profile loading error at %s: %s" setting err)
             (when rollback-config
               (nskk-rollback-config rollback-config))))

          ;; プロファイル固有の初期化処理
          (when-let ((init-func (nskk-get-profile-initializer profile-name)))
            (funcall init-func))

          ;; 状態更新
          (setopt nskk-current-profile profile-name)
          (nskk-save-profile-state)

          ;; 成功メッセージ
          (message "[NSKK] Profile '%s' loaded successfully (%.2fs)"
                   profile-name (nskk-get-loading-time)))

      ;; エラー時のロールバック
      (when (and rollback-config (not (eq nskk-current-profile profile-name)))
        (message "[NSKK] Rolling back to previous profile due to errors")
        (nskk-rollback-config rollback-config)))))

;; Emacs 30以上のAIベースコンテキスト適応型プロファイル自動切り替え
(defun nskk-auto-switch-profile (&optional force-analysis)
  "Emacs 30以上のAIコンテキスト分析によるスマートプロファイル選択"
  (interactive "P")
  (let* ((context-data (nskk-analyze-current-context))
         (mode-score (nskk-calculate-mode-scores context-data))
         (time-context (nskk-get-time-context))
         (user-patterns (nskk-get-user-behavior-patterns))
         (optimal-profile (nskk-ai-select-profile
                           :context-data context-data
                           :mode-scores mode-score
                           :time-context time-context
                           :user-patterns user-patterns
                           :confidence-threshold 0.7)))

    (when (and optimal-profile
               (not (eq optimal-profile nskk-current-profile))
               (or force-analysis
                   (> (nskk-get-profile-confidence optimal-profile) 0.8)))
      ;; スムーズなプロファイル遷移
      (nskk-transition-to-profile optimal-profile
                                 :animation t
                                 :preserve-state t)

      ;; 学習データ更新
      (nskk-update-profile-learning-data optimal-profile context-data))))

;; コンテキスト変化検知フック（非同期）
(add-hook 'nskk-mode-hook #'nskk-auto-switch-profile)
(add-hook 'window-configuration-change-hook #'nskk-context-change-detector)
(add-hook 'buffer-list-update-hook #'nskk-buffer-context-analyzer)
```

### 学習パターンの調整

```elisp
;; Emacs 30以上のAI強化学習システム設定（深層学習・リアルタイム適応）
(setopt nskk-learning-parameters
        `((:frequency-weight . ,(nskk-adaptive-weight 'frequency 0.7))   ; 動的重み調整
          (:recency-weight . ,(nskk-adaptive-weight 'recency 0.2))       ; 時間減衰適応
          (:context-weight . ,(nskk-adaptive-weight 'context 0.1))       ; 文脈理解重み
          (:semantic-weight . 0.15)                                      ; 意味的関連性
          (:decay-factor . ,(nskk-calculate-optimal-decay))               ; 動的減衰係数
          (:min-occurrences . ,(nskk-adaptive-threshold))                 ; 適応的闾値
          (:neural-boost . 0.3)                                          ; ニューラルネットワーク加算
          (:confidence-threshold . 0.85)                                 ; 信頼度闾値
          (:learning-rate . ,(nskk-get-optimal-learning-rate))           ; 動的学習率
          (:batch-size . ,(max 32 (* (num-processors) 8)))               ; バッチサイズ最適化
          (:gradient-clipping . 1.0)                                     ; 勾配クリッピング
          (:dropout-rate . 0.1)))                                        ; ドロップアウト率

(defun nskk-customize-learning (user-preference)
  "ユーザー嗜好に基づく学習調整"
  (interactive
   (list (completing-read "Learning style: "
                         '("aggressive" "conservative" "adaptive"))))
  (pcase user-preference
    ("aggressive"
     (setq nskk-learning-factor 1.2
           nskk-learning-threshold 1))
    ("conservative"
     (setq nskk-learning-factor 0.8
           nskk-learning-threshold 5))
    ("adaptive"
     (setq nskk-learning-factor 1.0
           nskk-learning-threshold 3))))
```

## カスタム変換ルール

### 個人専用変換ルール

```elisp
;; 個人専用ローマ字変換ルール
(defvar nskk-personal-romaji-rules nil
  "個人カスタムローマ字ルール")

(defun nskk-add-personal-rule (romaji hiragana &optional description)
  "個人変換ルール追加"
  (interactive "sRomaji: \nsHiragana: \nsDescription: ")
  (let ((rule (list romaji hiragana description (current-time))))
    (push rule nskk-personal-romaji-rules)
    (nskk--rebuild-conversion-table)
    (message "Added rule: %s → %s" romaji hiragana)))

;; 特殊入力パターン
(defvar nskk-special-patterns
  '(("email" . (lambda () (nskk--expand-email-template)))
    ("date" . (lambda () (format-time-string "%Y年%m月%d日")))
    ("time" . (lambda () (format-time-string "%H時%M分")))
    ("sig" . (lambda () (nskk--insert-signature))))
  "特殊入力パターン")

(defun nskk-add-special-pattern (trigger function)
  "特殊パターン追加"
  (push (cons trigger function) nskk-special-patterns))

;; 動的略語展開
(defvar nskk-abbreviations
  '(("ns" . "namespace")
    ("impl" . "implementation")
    ("perf" . "performance")
    ("config" . "configuration"))
  "略語辞書")

(defun nskk-expand-abbreviation (abbrev)
  "略語展開"
  (or (alist-get abbrev nskk-abbreviations nil nil #'string=)
      abbrev))
```

### プログラミング特化カスタマイゼーション

```elisp
;; プログラミング言語特化設定
(defvar nskk-programming-modes
  '((emacs-lisp-mode . nskk-elisp-config)
    (python-mode . nskk-python-config)
    (go-mode . nskk-go-config)
    (rust-mode . nskk-rust-config))
  "プログラミングモード別設定")

(defun nskk-elisp-config ()
  "Emacs Lisp専用設定"
  (setq-local nskk-special-patterns
              (append nskk-special-patterns
                      '(("def" . nskk-insert-defun-template)
                        ("let" . nskk-insert-let-template)
                        ("lambda" . nskk-insert-lambda-template))))
  (nskk-enable-symbol-completion))

(defun nskk-insert-defun-template ()
  "defun テンプレート挿入"
  (insert "(defun ")
  (save-excursion
    (insert " ()\n  \"\")\n  ")))

;; コメント自動生成
(defun nskk-smart-comment ()
  "文脈に応じたコメント生成"
  (interactive)
  (let ((comment (nskk--generate-context-comment)))
    (insert comment)))

(defun nskk--generate-context-comment ()
  "文脈に基づくコメント生成"
  (cond
   ((nskk--in-function-p) "# 関数の説明")
   ((nskk--in-class-p) "# クラスの説明")
   ((nskk--in-loop-p) "# ループ処理")
   (t "# コメント")))
```

## 拡張機能開発

### プラグインアーキテクチャ

```elisp
;; プラグインシステム基盤
(defvar nskk-plugins nil
  "読み込み済みプラグイン一覧")

(defmacro nskk-define-plugin (name &rest body)
  "NSKKプラグイン定義マクロ"
  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "nskk-plugin-%s" name))
       (list :name ',name
             :version "1.0.0"
             :init (lambda () ,@body)
             :cleanup nil))
     (add-to-list 'nskk-plugins ',name)))

;; プラグイン例：音声フィードバック
(nskk-define-plugin sound-feedback
  (defun nskk-play-sound (sound-type)
    "音声フィードバック再生"
    (when nskk-enable-sound
      (let ((sound-file (nskk--get-sound-file sound-type)))
        (when (file-exists-p sound-file)
          (start-process "nskk-sound" nil "afplay" sound-file)))))

  (add-hook 'nskk-after-conversion-hook
            (lambda () (nskk-play-sound 'conversion)))
  (add-hook 'nskk-mode-change-hook
            (lambda () (nskk-play-sound 'mode-change))))

;; プラグイン例：統計収集
(nskk-define-plugin usage-statistics
  (defvar nskk-stats-data (make-hash-table :test 'equal))

  (defun nskk-record-usage (event data)
    "使用統計記録"
    (let ((timestamp (current-time))
          (entry (list :event event :data data :time timestamp)))
      (push entry (gethash (format-time-string "%Y-%m-%d")
                          nskk-stats-data))))

  (add-hook 'nskk-after-input-hook
            (lambda (char) (nskk-record-usage 'input char)))
  (add-hook 'nskk-after-conversion-hook
            (lambda (result) (nskk-record-usage 'conversion result))))
```

### カスタムUIコンポーネント

```elisp
;; 候補表示のカスタマイゼーション
(defun nskk-create-custom-candidate-display ()
  "カスタム候補表示システム"
  (let ((buffer (get-buffer-create "*NSKK Candidates*")))
    (with-current-buffer buffer
      (nskk-candidate-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (nskk--render-candidates nskk--current-candidates)
      (setq buffer-read-only t))
    (display-buffer buffer '(display-buffer-at-bottom))))

(defun nskk--render-candidates (candidates)
  "候補レンダリング"
  (let ((index 1))
    (dolist (candidate candidates)
      (let ((text (nskk-candidate-text candidate))
            (score (nskk-candidate-score candidate))
            (source (nskk-candidate-source candidate)))
        (insert (format "%d. %s" index text))
        (when nskk-show-candidate-info
          (insert (format " [%.2f|%s]" score source)))
        (insert "\n")
        (cl-incf index)))))

;; ミニマル候補表示
(defun nskk-minimal-candidate-display (candidates)
  "ミニマル候補表示"
  (when candidates
    (let ((display-string
           (mapconcat (lambda (cand)
                       (nskk-candidate-text cand))
                     (cl-subseq candidates 0 (min 3 (length candidates)))
                     " | ")))
      (message "候補: %s%s"
               display-string
               (if (> (length candidates) 3) " ..." "")))))
```

## パフォーマンスチューニング

### メモリ調整

```elisp
;; メモリプール管理
(defvar nskk-memory-pools
  '((candidates . nil)
    (strings . nil)
    (conversions . nil))
  "メモリプール")

(defun nskk-get-pooled-object (pool-type constructor)
  "プールからオブジェクト取得"
  (or (pop (alist-get pool-type nskk-memory-pools))
      (funcall constructor)))

(defun nskk-return-to-pool (pool-type object reset-function)
  "オブジェクトをプールに返却"
  (when object
    (funcall reset-function object)
    (push object (alist-get pool-type nskk-memory-pools))))

;; ガベージコレクション制御
(defvar nskk-gc-optimization-enabled t
  "GC調整有効フラグ")

(defmacro nskk-with-gc-optimization (&rest body)
  "GC調整実行"
  `(if nskk-gc-optimization-enabled
       (let ((gc-cons-threshold most-positive-fixnum)
             (gc-cons-percentage 0.6))
         (unwind-protect
             (progn ,@body)
           (garbage-collect)))
     (progn ,@body)))
```

### 辞書調整

```elisp
;; 辞書キャッシュ戦略
(defvar nskk-dictionary-cache-strategy 'lru
  "辞書キャッシュ戦略")

(defun nskk-optimize-dictionary-access ()
  "辞書アクセス調整"
  (pcase nskk-dictionary-cache-strategy
    ('lru (nskk--setup-lru-cache))
    ('lfu (nskk--setup-lfu-cache))
    ('adaptive (nskk--setup-adaptive-cache))))

(defun nskk--setup-lru-cache ()
  "LRUキャッシュ設定"
  (setq nskk--dictionary-cache
        (nskk--make-lru-cache nskk-dictionary-cache-size)))

;; 非同期辞書プリロード
(defun nskk-preload-dictionary-async ()
  "非同期辞書プリロード"
  (nskk-async-run
   (lambda ()
     (nskk--preload-common-words)
     (nskk--preload-user-patterns)
     (message "Dictionary preloading completed"))))

(defun nskk--preload-common-words ()
  "頻出語の事前読み込み"
  (let ((common-words '("こんにちは" "ありがとう" "よろしく")))
    (dolist (word common-words)
      (nskk--search-dictionary word))))
```

### 並行処理調整

```elisp
;; タスクキューによる並行処理
(defvar nskk-task-queue nil
  "タスクキュー")

(defvar nskk-worker-timer nil
  "ワーカータイマー")

(defun nskk-schedule-task (task priority)
  "タスクスケジューリング"
  (push (list :task task :priority priority :timestamp (current-time))
        nskk-task-queue)
  (setq nskk-task-queue
        (sort nskk-task-queue
              (lambda (a b) (> (plist-get a :priority)
                              (plist-get b :priority)))))
  (nskk--ensure-worker-running))

(defun nskk--ensure-worker-running ()
  "ワーカー実行確保"
  (unless (timerp nskk-worker-timer)
    (setq nskk-worker-timer
          (run-with-idle-timer 0.01 t #'nskk--process-task-queue))))

(defun nskk--process-task-queue ()
  "タスクキュー処理"
  (when nskk-task-queue
    (let ((task (pop nskk-task-queue)))
      (condition-case err
          (funcall (plist-get task :task))
        (error
         (nskk--log-task-error task err))))))
```

## フック活用

### カスタムフック実装

```elisp
;; フックシステム
(defvar nskk-custom-hooks
  '((before-candidate-selection . nil)
    (after-learning-update . nil)
    (on-dictionary-change . nil)
    (on-performance-alert . nil))
  "カスタムフック")

(defmacro nskk-define-hook (hook-name doc)
  "カスタムフック定義"
  `(progn
     (defvar ,(intern (format "nskk-%s-hook" hook-name)) nil ,doc)
     (push '(,(intern (format "%s" hook-name)) . nil)
           nskk-custom-hooks)))

;; フック実行システム
(defun nskk-run-hook-with-args (hook &rest args)
  "引数付きフック実行"
  (let ((hook-symbol (if (symbolp hook) hook (intern hook))))
    (when (boundp hook-symbol)
      (dolist (function (symbol-value hook-symbol))
        (condition-case err
            (apply function args)
          (error
           (nskk--log-hook-error hook function err)))))))

;; 条件付きフック
(defun nskk-add-conditional-hook (hook condition function)
  "条件付きフック追加"
  (add-hook hook
            (lambda (&rest args)
              (when (funcall condition)
                (apply function args)))))

;; フック例：文脈認識変換
(nskk-define-hook context-analysis
  "文脈分析フック")

(defun nskk-context-aware-conversion (input)
  "文脈認識変換"
  (let ((context (nskk--analyze-context)))
    (nskk-run-hook-with-args 'nskk-context-analysis-hook input context)))

(add-hook 'nskk-context-analysis-hook
          (lambda (input context)
            (when (eq context 'formal)
              (nskk--prefer-formal-candidates input))))
```

### イベント駆動システム

```elisp
;; イベントシステム
(defvar nskk-event-listeners (make-hash-table :test 'eq)
  "イベントリスナー")

(defun nskk-add-event-listener (event listener)
  "イベントリスナー追加"
  (let ((listeners (gethash event nskk-event-listeners)))
    (puthash event (cons listener listeners) nskk-event-listeners)))

(defun nskk-emit-event (event &rest data)
  "イベント発火"
  (let ((listeners (gethash event nskk-event-listeners)))
    (dolist (listener listeners)
      (apply listener data))))

;; 使用例
(nskk-add-event-listener 'conversion-completed
                        (lambda (input output)
                          (nskk--update-conversion-stats input output)))

(nskk-add-event-listener 'dictionary-updated
                        (lambda (entries)
                          (nskk--invalidate-cache)
                          (nskk--rebuild-index)))
```

## プラグインシステム

### プラグインマネージャー

```elisp
;; プラグインマネージャー
(defvar nskk-plugin-directory "~/.emacs.d/nskk-plugins"
  "プラグインディレクトリ")

(defun nskk-install-plugin (plugin-name source)
  "プラグインインストール"
  (interactive "sPlugin name: \nsSource (file/url): ")
  (let ((plugin-path (expand-file-name plugin-name nskk-plugin-directory)))
    (nskk--download-plugin source plugin-path)
    (nskk--register-plugin plugin-name plugin-path)
    (message "Plugin '%s' installed successfully" plugin-name)))

(defun nskk-load-plugins ()
  "全プラグイン読み込み"
  (when (file-directory-p nskk-plugin-directory)
    (dolist (plugin-file (directory-files nskk-plugin-directory t "\\.el$"))
      (condition-case err
          (load-file plugin-file)
        (error
         (nskk--log-plugin-error plugin-file err))))))

;; プラグイン設定システム
(defvar nskk-plugin-configs (make-hash-table :test 'equal)
  "プラグイン設定")

(defun nskk-configure-plugin (plugin-name config)
  "プラグイン設定"
  (puthash plugin-name config nskk-plugin-configs))

(defun nskk-get-plugin-config (plugin-name key &optional default)
  "プラグイン設定取得"
  (let ((config (gethash plugin-name nskk-plugin-configs)))
    (plist-get config key default)))
```

### サンプルプラグイン

```elisp
;; 予測入力プラグイン
(nskk-define-plugin predictive-input
  (defvar nskk-prediction-cache (make-hash-table :test 'equal))

  (defun nskk-predict-next-char (current-input)
    "次文字予測"
    (let ((predictions (gethash current-input nskk-prediction-cache)))
      (when predictions
        (car (sort predictions (lambda (a b) (> (cdr a) (cdr b))))))))

  (defun nskk-update-predictions (input next-char)
    "予測データ更新"
    (let ((predictions (gethash input nskk-prediction-cache)))
      (if-let ((entry (assoc next-char predictions)))
          (cl-incf (cdr entry))
        (push (cons next-char 1) predictions))
      (puthash input predictions nskk-prediction-cache)))

  (add-hook 'nskk-after-input-hook
            (lambda (char)
              (when (> (length nskk--input-buffer) 1)
                (nskk-update-predictions
                 (substring nskk--input-buffer 0 -1)
                 char)))))

;; 多言語対応プラグイン
(nskk-define-plugin multilingual-support
  (defvar nskk-language-detectors
    '((english . nskk--detect-english)
      (korean . nskk--detect-korean)
      (chinese . nskk--detect-chinese)))

  (defun nskk-auto-language-switch ()
    "自動言語切り替え"
    (let ((detected-lang (nskk--detect-input-language)))
      (when (and detected-lang
                 (not (eq detected-lang nskk-current-language)))
        (nskk-switch-language detected-lang))))

  (add-hook 'nskk-before-conversion-hook #'nskk-auto-language-switch))
```

## 設定例

### パワーユーザー向け統合設定

```elisp
;; 統合カスタマイゼーション
(defun nskk-ultimate-setup ()
  "NSKKの総合設定"
  (interactive)

  ;; パフォーマンス調整
  (setq nskk-performance-mode t
        nskk-dictionary-cache-size 100000
        nskk-use-memory-mapped-dictionary t
        nskk-enable-parallel-processing t)

  ;; 学習機能強化
  (setq nskk-learning-factor 1.5
        nskk-context-learning-enabled t
        nskk-temporal-learning-enabled t)

  ;; UI強化
  (setq nskk-candidate-display-count 10
        nskk-show-candidate-info t
        nskk-enable-candidate-preview t)

  ;; 拡張機能
  (nskk-enable-plugin 'predictive-input)
  (nskk-enable-plugin 'multilingual-support)
  (nskk-enable-plugin 'usage-statistics)

  ;; カスタムフック
  (add-hook 'nskk-mode-hook #'nskk-auto-switch-profile)
  (add-hook 'nskk-after-conversion-hook #'nskk-context-aware-learning)

  (message "NSKK ultimate setup completed!"))

;; 開発者向け設定
(defun nskk-developer-setup ()
  "開発者向け設定"
  (interactive)

  ;; デバッグ機能
  (setq nskk-debug-mode t
        nskk-performance-monitoring t
        nskk-log-level 'debug)

  ;; 開発支援
  (nskk-enable-symbol-completion)
  (nskk-enable-code-templates)
  (nskk-enable-smart-indentation)

  ;; プログラミング特化
  (dolist (mode nskk-programming-modes)
    (add-hook (intern (format "%s-hook" (car mode)))
              (cdr mode)))

  (message "NSKK developer setup completed!"))
```

## トラブルシューティング

### カスタマイゼーション問題の診断

```elisp
(defun nskk-diagnose-customization ()
  "カスタマイゼーション診断"
  (interactive)
  (let ((issues nil))

    ;; 設定値検証
    (unless (file-exists-p nskk-dictionary-path)
      (push "Dictionary file not found" issues))

    ;; パフォーマンス検証
    (when (> (nskk--measure-response-time) 0.1)
      (push "Response time too slow" issues))

    ;; プラグイン検証
    (dolist (plugin nskk-plugins)
      (unless (nskk--plugin-loaded-p plugin)
        (push (format "Plugin '%s' failed to load" plugin) issues)))

    (if issues
        (message "Issues found: %s" (mapconcat 'identity issues ", "))
      (message "All customizations are working correctly"))))
```

## まとめ

NSKKのカスタマイゼーション機能により、以下が実現できます：

### ✅ 個人設定
- プロファイル別設定管理
- 学習パターンの調整
- 用途別自動切り替え

### ✅ 機能拡張
- カスタム変換ルール
- プラグイン開発
- UI コンポーネントカスタマイズ

### ✅ パフォーマンス調整
- メモリ調整
- 並行処理
- キャッシュ戦略

### ✅ 開発支援
- プログラミング特化機能
- テンプレートシステム
- コンテキスト認識

これらのカスタマイゼーション技術をマスターすることで、NSKKを自分仕様にカスタマイズし、日本語入力環境を構築できます。