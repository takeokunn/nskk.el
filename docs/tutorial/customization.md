# NSKKカスタマイズガイド：完全個人最適化

## 本ガイドについて

NSKKを自分の使い方に最適化するための包括的なカスタマイズガイドです。設定ファイルの構成から高度なカスタマイズまで、段階的に学びます。

### 学習目標

- ⚙️ 基本設定の理解と調整
- 🎨 UI・表示のカスタマイズ
- 📚 辞書管理とチューニング
- ⌨️ キーバインドのカスタマイズ
- 🚀 パフォーマンスチューニング
- 🔧 プロファイル管理

**総学習時間**: 約60分

## 第1章：設定ファイルの構成

### 1.1 基本的な設定ファイル

```elisp
;; ~/.emacs.d/init.el または ~/.emacs

;; NSKKのロード
(add-to-list 'load-path "~/path/to/nskk.el")
(require 'nskk)

;; 基本設定
(setq nskk-user-directory "~/.nskk/")
(setq nskk-japanese-message-and-error t)

;; モード有効化
(global-set-key (kbd "C-x C-j") 'nskk-mode)
```

### 1.2 設定の分割管理

推奨ディレクトリ構造：

```
~/.emacs.d/
├── init.el                 # メイン設定
└── nskk-config/
    ├── nskk-basic.el      # 基本設定
    ├── nskk-dict.el       # 辞書設定
    ├── nskk-ui.el         # UI設定
    ├── nskk-keys.el       # キーバインド
    └── nskk-advanced.el   # 高度設定
```

init.elでの読み込み：

```elisp
;; NSKK設定の読み込み
(mapc #'load
      '("~/.emacs.d/nskk-config/nskk-basic"
        "~/.emacs.d/nskk-config/nskk-dict"
        "~/.emacs.d/nskk-config/nskk-ui"
        "~/.emacs.d/nskk-config/nskk-keys"
        "~/.emacs.d/nskk-config/nskk-advanced"))
```

## 第2章：基本設定のカスタマイズ

### 2.1 入力方式の選択

```elisp
;; ローマ字入力方式の選択
(setq nskk-romaji-style 'default)
; 選択肢: 'default, 'azik, 'act, 'tut-code

;; AZIK入力方式の有効化
(setq nskk-use-azik t)
(setq nskk-azik-keyboard-type 'jp106)

;; かな入力の有効化
(setq nskk-use-kana-input nil) ; t で有効
```

### 2.2 モード切り替え

```elisp
;; 自動起動
(setq nskk-mode-auto-start t)

;; バッファ種類ごとの自動有効化
(setq nskk-mode-enable-in-buffer
      '(text-mode
        org-mode
        markdown-mode))

;; 除外バッファ
(setq nskk-mode-disable-in-buffer
      '(minibuffer-inactive-mode
        special-mode))
```

### 2.3 変換動作

```elisp
;; 変換方式
(setq nskk-conversion-mode 'smart) ; 'basic, 'smart, 'ai

;; 学習機能
(setq nskk-enable-learning t)
(setq nskk-learning-factor 0.8)

;; 候補の最大表示数
(setq nskk-max-candidates 10)

;; 自動確定
(setq nskk-auto-commit-delay nil) ; 数値で自動確定秒数
```

## 第3章：UI・表示のカスタマイズ

### 3.1 候補ウィンドウ

```elisp
;; 候補表示方式
(setq nskk-candidate-window-type 'popup)
; 'popup, 'inline, 'tooltip, 'posframe, 'overlay

;; ポップアップウィンドウの位置
(setq nskk-candidate-window-position 'bottom)
; 'top, 'bottom, 'cursor

;; 候補ウィンドウのサイズ
(setq nskk-candidate-window-max-height 15)

;; 候補の表示形式
(setq nskk-candidate-display-format
      '((:index . " %d. ")
        (:candidate . "%s")
        (:annotation . " [%s]")))
```

カスタム表示例：

```elisp
;; 番号付き・注釈表示
(defun my-nskk-format-candidate (index candidate annotation)
  "Custom candidate formatting."
  (format "【%d】%s <%s>"
          index
          (propertize candidate 'face 'nskk-candidate-face)
          (propertize annotation 'face 'nskk-annotation-face)))

(setq nskk-candidate-formatter #'my-nskk-format-candidate)
```

### 3.2 モードライン

```elisp
;; モードライン表示
(setq nskk-show-mode-line t)

;; モードインジケーターのカスタマイズ
(setq nskk-mode-line-indicators
      '((hiragana . " [あ]")
        (katakana . " [ア]")
        (latin . " [_A]")
        (wide-latin . " [Ａ]")
        (converting . " [▽]")
        (selecting . " [▼]")))

;; 色のカスタマイズ
(custom-set-faces
 '(nskk-indicator-hiragana-face ((t (:foreground "#00ff00"))))
 '(nskk-indicator-katakana-face ((t (:foreground "#0000ff"))))
 '(nskk-indicator-converting-face ((t (:foreground "#ff0000")))))
```

### 3.3 カーソルとハイライト

```elisp
;; 変換中のアンダーライン
(setq nskk-use-underline t)
(setq nskk-underline-style 'wave) ; 'line, 'wave, 'dots

;; カーソル色の変更
(setq nskk-cursor-change-enabled t)
(setq nskk-cursor-hiragana-color "#00ff00")
(setq nskk-cursor-katakana-color "#0000ff")
(setq nskk-cursor-latin-color "#ffffff")

;; 変換範囲のハイライト
(setq nskk-highlight-converting-text t)
(setq nskk-highlight-face 'highlight)
```

### 3.4 フォントとサイズ

```elisp
;; 候補ウィンドウのフォント
(custom-set-faces
 '(nskk-candidate-window-face
   ((t (:family "Noto Sans CJK JP"
        :height 120
        :background "#f0f0f0"
        :foreground "#000000")))))

;; 注釈のフォント
(custom-set-faces
 '(nskk-annotation-face
   ((t (:family "Noto Sans CJK JP"
        :height 100
        :foreground "#666666"
        :slant italic)))))
```

## 第4章：辞書管理

### 4.1 辞書ファイルの設定

```elisp
;; 個人辞書
(setq nskk-jisyo-file "~/.nskk/jisyo")

;; システム辞書のリスト
(setq nskk-large-jisyo-list
      '("/usr/share/skk/SKK-JISYO.L"
        "~/dicts/SKK-JISYO.jinmei"
        "~/dicts/SKK-JISYO.geo"))

;; 辞書の優先順位
(setq nskk-dictionary-priority-list
      '("~/.nskk/jisyo"              ; 個人辞書（最優先）
        "~/.nskk/tech-jisyo"         ; 技術用語
        "/usr/share/skk/SKK-JISYO.L" ; システム辞書
        "~/dicts/SKK-JISYO.jinmei")) ; 人名辞書
```

### 4.2 辞書サーバー

```elisp
;; 辞書サーバーの使用
(setq nskk-use-server t)
(setq nskk-server-host "localhost")
(setq nskk-server-port 1178)

;; サーバー接続のタイムアウト
(setq nskk-server-timeout 3.0)

;; ローカル辞書優先
(setq nskk-search-local-first t)
```

### 4.3 辞書の自動保存

```elisp
;; 自動保存の有効化
(setq nskk-auto-save-jisyo t)

;; 自動保存の間隔（秒）
(setq nskk-auto-save-interval 300) ; 5分

;; Emacs終了時の保存確認
(setq nskk-save-jisyo-on-exit 'ask) ; t で無確認保存
```

### 4.4 専門辞書の追加

プログラミング用語辞書の作成：

```elisp
;; ~/.nskk/tech-jisyo

;; プログラミング用語
api /API/application programming interface/
ふれーむわーく /フレームワーク/Framework/
でばっぐ /デバッグ/debug/
りふぁくたりんぐ /リファクタリング/refactoring/

;; 読み込み
(add-to-list 'nskk-dictionary-priority-list
             "~/.nskk/tech-jisyo")
```

## 第5章：キーバインドのカスタマイズ

### 5.1 グローバルキー

```elisp
;; NSKK モードの切り替え
(global-set-key (kbd "C-x C-j") 'nskk-mode)
(global-set-key (kbd "C-x j") 'nskk-mode-toggle)

;; 辞書管理
(global-set-key (kbd "C-x J") 'nskk-jisyo-edit)

;; 設定メニュー
(global-set-key (kbd "C-c n m") 'nskk-transient-menu)
```

### 5.2 NSKK モード内のキー

```elisp
;; nskk-mode-map のカスタマイズ
(define-key nskk-mode-map (kbd "C-j") 'nskk-kakutei)
(define-key nskk-mode-map (kbd "q") 'nskk-toggle-kana)
(define-key nskk-mode-map (kbd "l") 'nskk-latin-mode)
(define-key nskk-mode-map (kbd "L") 'nskk-wide-latin-mode)
(define-key nskk-mode-map (kbd "C-q") 'nskk-set-henkan-point)

;; 候補選択
(define-key nskk-mode-map (kbd "SPC") 'nskk-start-henkan)
(define-key nskk-mode-map (kbd "x") 'nskk-previous-candidate)
(define-key nskk-mode-map (kbd "C-g") 'nskk-cancel-henkan)
```

### 5.3 Evil モードとの統合

```elisp
;; Evil使用時の設定
(with-eval-after-load 'evil
  ;; インサートモードでのみ有効化
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (when (bound-and-true-p nskk-mode)
                (nskk-mode 1))))

  ;; ノーマルモードで無効化
  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (bound-and-true-p nskk-mode)
                (nskk-latin-mode)))))
```

## 第6章：パフォーマンスチューニング

### 6.1 キャッシュ設定

```elisp
;; 辞書キャッシュのサイズ
(setq nskk-dictionary-cache-size 50000)

;; 変換結果のキャッシュ
(setq nskk-enable-conversion-cache t)
(setq nskk-conversion-cache-size 10000)

;; キャッシュの事前読み込み
(setq nskk-preload-cache t)

;; アイドル時のキャッシュ構築
(setq nskk-build-cache-when-idle t)
(setq nskk-idle-cache-build-delay 5.0) ; 秒
```

### 6.2 並列処理

```elisp
;; スレッドプールの設定
(setq nskk-enable-threading t)
(setq nskk-thread-pool-size 4) ; CPUコア数に応じて調整

;; 並列化する操作
(setq nskk-parallel-operations
      '(dictionary-search
        index-building
        ai-analysis))

;; 非同期辞書検索
(setq nskk-async-dictionary-search t)
```

### 6.3 メモリ管理

```elisp
;; メモリ使用量の制限
(setq nskk-memory-limit (* 20 1024 1024)) ; 20MB

;; 未使用辞書の自動アンロード
(setq nskk-auto-unload-dictionaries t)
(setq nskk-dictionary-idle-time 600) ; 10分

;; ガベージコレクション の調整
(setq gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-percentage 1.0)
```

### 6.4 起動時間の最適化

```elisp
;; 遅延ロード
(autoload 'nskk-mode "nskk" "NSKK mode" t)
(autoload 'nskk-transient-menu "nskk-transient" "NSKK menu" t)

;; 辞書の遅延読み込み
(setq nskk-lazy-load-dictionaries t)

;; バックグラウンド初期化
(setq nskk-async-initialization t)

;; 起動時のベンチマーク
(defun my-nskk-benchmark-startup ()
  "Benchmark NSKK startup time."
  (let ((start-time (current-time)))
    (require 'nskk)
    (message "NSKK loaded in %.3f seconds"
             (float-time (time-since start-time)))))

;; Emacs起動後に実行
(add-hook 'emacs-startup-hook #'my-nskk-benchmark-startup)
```

## 第7章：プロファイル管理

### 7.1 複数プロファイルの設定

```elisp
;; プロファイルの定義
(defvar my-nskk-profiles
  '((work
     :dictionaries ("~/.nskk/work-jisyo" "/usr/share/skk/SKK-JISYO.L")
     :ai-level 0.8
     :sync-enabled t)

    (personal
     :dictionaries ("~/.nskk/personal-jisyo" "/usr/share/skk/SKK-JISYO.L")
     :ai-level 0.6
     :sync-enabled nil)

    (programming
     :dictionaries ("~/.nskk/tech-jisyo" "~/.nskk/jisyo")
     :ai-level 0.9
     :ai-domain 'programming)))

;; プロファイル切り替え関数
(defun my-nskk-switch-profile (profile)
  "Switch to PROFILE."
  (interactive
   (list (intern (completing-read "Profile: "
                                  '("work" "personal" "programming")))))
  (let ((config (alist-get profile my-nskk-profiles)))
    (setq nskk-dictionary-priority-list (plist-get config :dictionaries))
    (setq nskk-ai-aggression-level (plist-get config :ai-level))
    (setq nskk-enable-sync (plist-get config :sync-enabled))
    (when-let ((domain (plist-get config :ai-domain)))
      (setq nskk-ai-domain domain))
    (nskk-reload-dictionaries)
    (message "Switched to %s profile" profile)))

;; キーバインド
(global-set-key (kbd "C-c n p") 'my-nskk-switch-profile)
```

### 7.2 コンテキスト別自動切り替え

```elisp
;; プロジェクトディレクトリに応じた切り替え
(defun my-nskk-auto-profile ()
  "Automatically switch profile based on context."
  (cond
   ;; 仕事用ディレクトリ
   ((string-match "/work/" default-directory)
    (my-nskk-switch-profile 'work))

   ;; プログラミングプロジェクト
   ((or (derived-mode-p 'prog-mode)
        (file-exists-p ".git"))
    (my-nskk-switch-profile 'programming))

   ;; その他
   (t
    (my-nskk-switch-profile 'personal))))

(add-hook 'nskk-mode-enable-hook #'my-nskk-auto-profile)
```

## 第8章：高度なカスタマイズ例

### 8.1 カスタム変換フィルタ

```elisp
;; 特定パターンの自動変換
(defun my-nskk-auto-convert (input)
  "Automatically convert certain patterns."
  (pcase input
    ;; 日付
    ("@today" (format-time-string "%Y-%m-%d"))
    ("@now" (format-time-string "%H:%M:%S"))

    ;; メールアドレス
    ((pred (string-prefix-p "@"))
     (concat (substring input 1) "@example.com"))

    ;; Git ハッシュ
    ("@git"
     (string-trim
      (shell-command-to-string "git rev-parse --short HEAD")))

    (_ nil)))

(add-hook 'nskk-before-conversion-hook #'my-nskk-auto-convert)
```

### 8.2 辞書の動的生成

```elisp
;; バッファ内の単語を辞書に自動追加
(defun my-nskk-add-buffer-words ()
  "Add frequently used words in buffer to dictionary."
  (interactive)
  (let ((words (make-hash-table :test 'equal)))
    ;; バッファ内の単語を収集
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cj+" nil t)
        (let ((word (match-string 0)))
          (puthash word (1+ (gethash word words 0)) words))))

    ;; 頻出単語を辞書に追加
    (maphash
     (lambda (word count)
       (when (> count 5) ; 5回以上出現
         (nskk-add-word-to-jisyo word)))
     words)))
```

### 8.3 カスタム統計収集

```elisp
;; 入力統計の収集
(defvar my-nskk-stats
  '((total-conversions . 0)
    (total-keystrokes . 0)
    (start-time . nil)))

(defun my-nskk-track-conversion ()
  "Track conversion statistics."
  (cl-incf (alist-get 'total-conversions my-nskk-stats))
  (unless (alist-get 'start-time my-nskk-stats)
    (setf (alist-get 'start-time my-nskk-stats) (current-time))))

(defun my-nskk-show-stats ()
  "Show input statistics."
  (interactive)
  (let* ((conversions (alist-get 'total-conversions my-nskk-stats))
         (start (alist-get 'start-time my-nskk-stats))
         (elapsed (if start (float-time (time-since start)) 0))
         (rate (if (> elapsed 0) (/ conversions elapsed 60.0) 0)))
    (message "Conversions: %d, Rate: %.1f/min" conversions rate)))

(add-hook 'nskk-after-conversion-hook #'my-nskk-track-conversion)
```

## 第9章：トラブルシューティング設定

### 9.1 デバッグモード

```elisp
;; デバッグログの有効化
(setq nskk-debug-mode t)
(setq nskk-debug-log-file "~/.nskk/debug.log")

;; 詳細ログレベル
(setq nskk-log-level 'debug) ; 'error, 'warn, 'info, 'debug, 'trace

;; ログの表示
M-x nskk-show-debug-log
```

### 9.2 パフォーマンスモニタリング

```elisp
;; リアルタイムパフォーマンスモニタ
(setq nskk-show-performance-metrics t)

;; モードラインにメトリクス表示
(defun my-nskk-show-metrics ()
  "Show performance metrics in mode-line."
  (format " [%.2fms|%.1f%%]"
          nskk-last-conversion-time
          nskk-cache-hit-rate))

(add-to-list 'mode-line-format '(:eval (my-nskk-show-metrics)))
```

### 9.3 設定の検証

```elisp
;; 設定の妥当性チェック
(defun my-nskk-validate-config ()
  "Validate NSKK configuration."
  (interactive)
  (let ((warnings nil))
    ;; 辞書ファイルの存在確認
    (dolist (dict nskk-dictionary-priority-list)
      (unless (file-exists-p dict)
        (push (format "Dictionary not found: %s" dict) warnings)))

    ;; メモリ設定の確認
    (when (< nskk-dictionary-cache-size 1000)
      (push "Cache size too small (< 1000)" warnings))

    ;; スレッド数の確認
    (when (> nskk-thread-pool-size (num-processors))
      (push "Thread pool larger than CPU cores" warnings))

    ;; 結果表示
    (if warnings
        (message "Configuration warnings:\n%s"
                 (mapconcat #'identity warnings "\n"))
      (message "Configuration is valid"))))

;; 起動時に自動検証
(add-hook 'emacs-startup-hook #'my-nskk-validate-config)
```

## 第10章：設定例集

### 10.1 ミニマム設定

```elisp
;; 最小限の設定
(require 'nskk)
(global-set-key (kbd "C-x C-j") 'nskk-mode)
(setq nskk-jisyo-file "~/.nskk-jisyo")
(setq nskk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
```

### 10.2 バランス設定

```elisp
;; バランスの取れた設定
(require 'nskk)

;; 基本設定
(setq nskk-jisyo-file "~/.nskk-jisyo")
(setq nskk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
(global-set-key (kbd "C-x C-j") 'nskk-mode)

;; 学習とAI
(setq nskk-enable-learning t)
(setq nskk-enable-ai-completion t)
(setq nskk-ai-aggression-level 0.7)

;; UI
(setq nskk-candidate-window-type 'popup)
(setq nskk-show-mode-line t)

;; パフォーマンス
(setq nskk-dictionary-cache-size 25000)
(setq nskk-enable-threading t)
```

### 10.3 最大パフォーマンス設定

```elisp
;; 高性能設定
(require 'nskk)

;; 基本
(setq nskk-jisyo-file "~/.nskk-jisyo")
(setq nskk-large-jisyo "/usr/share/skk/SKK-JISYO.L")

;; 最大キャッシュ
(setq nskk-dictionary-cache-size 100000)
(setq nskk-conversion-cache-size 50000)
(setq nskk-preload-cache t)

;; 最大並列化
(setq nskk-enable-threading t)
(setq nskk-thread-pool-size 8)
(setq nskk-parallel-operations
      '(dictionary-search index-building ai-analysis sync-upload))

;; AI最大活用
(setq nskk-enable-ai-completion t)
(setq nskk-ai-aggression-level 0.9)
(setq nskk-ai-context-window 200)

;; メモリ設定
(setq gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-percentage 1.0)
```

## まとめ

### 習得した設定技術

このガイドで、以下のカスタマイズ技術を習得しました：

1. ✅ **基本設定**: ファイル構成・入力方式・モード制御
2. ✅ **UI設定**: 表示・フォント・色のカスタマイズ
3. ✅ **辞書管理**: 複数辞書・専門辞書・サーバー連携
4. ✅ **キーバインド**: グローバル・モード内キーの設定
5. ✅ **パフォーマンス**: キャッシュ・並列化・メモリ管理
6. ✅ **プロファイル**: 用途別設定の切り替え
7. ✅ **高度カスタマイズ**: フィルタ・統計・デバッグ

### カスタマイズ継続のために

- 定期的な設定の見直し（月1回）
- パフォーマンス測定と最適化
- 新機能のトライアル
- コミュニティの設定例を参考に

**あなた専用の最適化されたNSKK環境を楽しんでください！** ⚙️
