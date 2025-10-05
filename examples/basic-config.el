;;; basic-config.el --- Basic NSKK configuration examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, examples
;; Version: 0.1.0

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKの基本設定例を提供します。
;;
;; 含まれる設定:
;; 1. 最小限の設定 - NSKKを動作させるための必要最小限の設定
;; 2. 標準的な設定 - 一般的な使用に適した設定
;; 3. 推奨設定 - パフォーマンスと使いやすさを両立した設定
;;
;; 使い方:
;; 1. このファイルを読む
;; 2. 自分の環境に合わせて設定をコピー
;; 3. init.elまたは.emacsに貼り付ける
;;
;; 注意:
;; - パスは環境に合わせて変更してください
;; - Emacs 30以上の機能を使用する設定も含まれています

;;; Code:

;;; ========================================
;;; 1. 最小限の設定（3行）
;;; ========================================

;; NSKKを動作させるための必要最小限の設定です。
;; とりあえず動かしてみたい場合はこれだけで十分です。

;; (add-to-list 'load-path "/path/to/nskk.el")
;; (require 'nskk)
;; (global-set-key (kbd "C-x C-j") 'nskk-mode)

;; 注意:
;; - /path/to/nskk.el は実際のNSKKのパスに置き換えてください
;; - 辞書ファイルは自動的に検索されます（~/dict/SKK-JISYO.Lなど）

;;; ========================================
;;; 2. 標準的な設定（10行）
;;; ========================================

;; 一般的な使用に適した設定です。
;; 基本的な日本語入力に必要な機能が有効になります。

;; ;; NSKKのロード
;; (add-to-list 'load-path "/path/to/nskk.el")
;; (require 'nskk)

;; ;; グローバルキーバインド
;; (global-set-key (kbd "C-x C-j") 'nskk-mode)

;; ;; 辞書設定
;; (setq nskk-dictionary-list
;;       '(("~/dict/personal.dic" . personal)  ; 個人辞書
;;         ("~/dict/SKK-JISYO.L" . system)))   ; システム辞書

;; ;; 基本設定
;; (setq nskk-auto-start-henkan t             ; 自動変換開始
;;       nskk-use-azik nil)                    ; AZIK無効（標準ローマ字）

;; 解説:
;; - personal.dic: 学習した変換を保存する個人辞書
;; - SKK-JISYO.L: 標準的なSKK辞書（別途ダウンロード必要）
;; - nskk-auto-start-henkan: 大文字入力で自動的に変換モードに入る

;;; ========================================
;;; 3. 推奨設定（20行）- パフォーマンス最適化版
;;; ========================================

;; パフォーマンスと使いやすさを両立した設定です。
;; 日常的に使用する場合はこの設定をおすすめします。

;; ;; NSKKのロードとコンパイル最適化
;; (add-to-list 'load-path "/path/to/nskk.el")
;; (require 'nskk)

;; ;; グローバルキーバインド
;; (global-set-key (kbd "C-x C-j") 'nskk-mode)
;; (global-set-key (kbd "C-x j") 'nskk-mode-toggle)  ; トグル用

;; ;; 辞書設定 - 階層化による高速検索
;; (setq nskk-dictionary-list
;;       '(("~/dict/personal.dic" . personal)      ; 個人辞書（最優先）
;;         ("~/dict/SKK-JISYO.L" . system)         ; システム辞書
;;         ("~/dict/SKK-JISYO.jinmei" . names)     ; 人名辞書
;;         ("~/dict/SKK-JISYO.geo" . geography)))  ; 地名辞書

;; ;; パフォーマンス設定
;; (setq nskk-dictionary-cache-size 10000      ; キャッシュサイズ: 10,000エントリ
;;       nskk-dictionary-preload t             ; 起動時プリロード
;;       nskk-use-trie-index t)                ; トライ木インデックス使用

;; ;; 入力設定
;; (setq nskk-auto-start-henkan t              ; 自動変換開始
;;       nskk-delete-implies-kakutei t         ; 削除キーで確定
;;       nskk-egg-like-newline t)              ; Enter で改行・確定

;; ;; 候補表示設定
;; (setq nskk-show-candidates-always-pop-to-buffer t  ; 候補バッファ常時表示
;;       nskk-candidate-display-count 7)              ; 表示候補数

;; ;; 学習機能設定
;; (setq nskk-learning-enabled t               ; 学習機能有効
;;       nskk-save-jisyo-instantly nil)        ; 終了時に保存

;; 解説:
;; - キャッシュサイズ: メモリ使用量とパフォーマンスのバランス
;; - トライ木インデックス: 前方一致検索の高速化
;; - 自動変換: 大文字入力で即座に変換モード
;; - 候補数: 一度に表示する変換候補の数

;;; ========================================
;;; 4. パッケージマネージャー対応設定
;;; ========================================

;; use-packageを使用する場合の設定例です。

;; (use-package nskk
;;   :load-path "/path/to/nskk.el"
;;   :bind (("C-x C-j" . nskk-mode)
;;          ("C-x j" . nskk-mode-toggle))
;;   :custom
;;   ;; 辞書設定
;;   (nskk-dictionary-list
;;    '(("~/dict/personal.dic" . personal)
;;      ("~/dict/SKK-JISYO.L" . system)))
;;   ;; パフォーマンス設定
;;   (nskk-dictionary-cache-size 10000)
;;   (nskk-dictionary-preload t)
;;   (nskk-use-trie-index t)
;;   ;; 入力設定
;;   (nskk-auto-start-henkan t)
;;   (nskk-delete-implies-kakutei t)
;;   (nskk-egg-like-newline t)
;;   ;; 学習設定
;;   (nskk-learning-enabled t)
;;   :config
;;   ;; 起動時初期化
;;   (nskk-setup-keybindings))

;;; ========================================
;;; 5. Emacs 30以上での最適化設定
;;; ========================================

;; Emacs 30以上でのみ使用できる高度な設定です。
;; ネイティブコンパイルとスレッド並列処理を活用します。

;; ;; Emacs 30以上でのバージョンチェック
;; (when (>= emacs-major-version 30)
;;   ;; ネイティブコンパイル設定
;;   (setopt native-comp-speed 3                    ; 最大速度
;;           native-comp-jit-compilation t          ; JITコンパイル
;;           native-comp-deferred-compilation t)    ; 遅延コンパイル

;;   ;; GC最適化
;;   (setopt gc-cons-threshold 134217728            ; 128MB
;;           gc-cons-percentage 1.0)                ; GC実行条件緩和

;;   ;; NSKKのロード
;;   (add-to-list 'load-path "/path/to/nskk.el")
;;   (require 'nskk)

;;   ;; Emacs 30以上対応機能の有効化
;;   (setopt nskk-enable-threading t                ; スレッド並列処理
;;           nskk-use-native-json t                 ; ネイティブJSON
;;           nskk-concurrent-dictionary-loading t)  ; 辞書並列読み込み

;;   ;; パフォーマンスモード設定
;;   (setopt nskk-performance-mode 'emacs31-turbo)

;;   ;; 総合最適化セットアップ（推奨）
;;   (nskk-emacs31-comprehensive-setup))

;;; ========================================
;;; 6. モード別自動有効化設定
;;; ========================================

;; 特定のモードでNSKKを自動的に有効にする設定です。

;; ;; テキスト系モードで自動有効化
;; (add-hook 'text-mode-hook 'nskk-mode)
;; (add-hook 'org-mode-hook 'nskk-mode)
;; (add-hook 'markdown-mode-hook 'nskk-mode)

;; ;; メール/チャット系で自動有効化
;; (add-hook 'message-mode-hook 'nskk-mode)  ; Gnus
;; (add-hook 'notmuch-message-mode-hook 'nskk-mode)

;; ;; 特定のモードでは無効化
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (when (memq major-mode '(python-mode emacs-lisp-mode))
;;               (nskk-mode -1))))

;;; ========================================
;;; 7. 初回セットアップ用設定
;;; ========================================

;; 初めてNSKKを使う場合の設定と辞書ダウンロード手順です。

;; ステップ1: NSKKのクローン
;; $ git clone https://github.com/takeokunn/nskk.el.git ~/nskk.el

;; ステップ2: 辞書のダウンロード
;; $ mkdir -p ~/dict
;; $ cd ~/dict
;; $ curl -O http://openlab.jp/skk/dic/SKK-JISYO.L.gz
;; $ gunzip SKK-JISYO.L.gz

;; ステップ3: init.elに以下を追加
;; (add-to-list 'load-path "~/nskk.el")
;; (require 'nskk)
;; (global-set-key (kbd "C-x C-j") 'nskk-mode)
;; (setq nskk-dictionary-list
;;       '(("~/dict/personal.dic" . personal)
;;         ("~/dict/SKK-JISYO.L" . system)))

;; ステップ4: Emacsを再起動

;;; ========================================
;;; 8. トラブルシューティング用設定
;;; ========================================

;; 問題が発生した場合のデバッグ設定です。

;; ;; デバッグモード有効化
;; (setq nskk-debug t)

;; ;; ログレベル設定
;; (setq nskk-log-level 'debug)  ; 'error, 'warning, 'info, 'debug

;; ;; パフォーマンス測定
;; (setq nskk-performance-monitoring t)

;; ;; エラー時の動作
;; (setq debug-on-error t)  ; エラー時にデバッガ起動

;; ;; 診断コマンド
;; ;; M-x nskk-diagnostic-report  ; 設定診断
;; ;; M-x nskk-benchmark-all      ; パフォーマンステスト

;;; ========================================
;;; 補足情報
;;; ========================================

;; 辞書ファイルの入手先:
;; - SKK-JISYO.L（標準辞書）:
;;   http://openlab.jp/skk/dic/SKK-JISYO.L.gz
;; - SKK-JISYO.jinmei（人名辞書）:
;;   http://openlab.jp/skk/dic/SKK-JISYO.jinmei.gz
;; - SKK-JISYO.geo（地名辞書）:
;;   http://openlab.jp/skk/dic/SKK-JISYO.geo.gz

;; よく使うコマンド:
;; - M-x nskk-mode             : NSKKの有効/無効切り替え
;; - M-x nskk-save-jisyo       : 個人辞書を保存
;; - M-x nskk-reload-jisyo     : 辞書を再読み込み
;; - M-x nskk-show-version     : バージョン情報表示
;; - M-x nskk-diagnostic-report: 設定診断

;; 設定のカスタマイズ:
;; M-x customize-group RET nskk RET

;; 詳細なドキュメント:
;; - チュートリアル: docs/tutorial/getting-started.md
;; - How-toガイド: docs/how-to/
;; - APIリファレンス: docs/reference/api-reference.md

(provide 'basic-config)

;;; basic-config.el ends here
