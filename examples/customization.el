;;; customization.el --- NSKK customization examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, customization
;; Version: 0.1.0

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKのカスタマイズ例を提供します。
;;
;; 含まれるカスタマイズ:
;; 1. キーバインドカスタマイズ - キー配置の変更
;; 2. UIカスタマイズ - 見た目の調整
;; 3. 辞書カスタマイズ - 辞書の追加・設定
;; 4. キャッシュ設定 - パフォーマンスチューニング
;; 5. フックカスタマイズ - 動作の拡張
;; 6. モードライン設定 - 状態表示の調整
;;
;; 使い方:
;; 1. 必要な設定を選んでコピー
;; 2. init.elに貼り付けて調整
;; 3. Emacsを再起動または設定を評価

;;; Code:

;;; ========================================
;;; 1. キーバインドカスタマイズ
;;; ========================================

;; 1-1. Sticky Shiftのキー変更
;; デフォルトの ";" から別のキーに変更する例

;; (setq nskk-keymap-sticky-key "@")  ; "@" キーをSticky Shiftに

;; 1-2. モード切り替えキーの変更

;; ;; ひらがなモードへの切り替えをC-jからC-oに変更
;; (define-key nskk-mode-map (kbd "C-o") 'nskk-mode-switch-to-hiragana)
;; (define-key nskk-mode-map (kbd "C-j") nil)  ; 元のバインドを削除

;; 1-3. 候補選択キーのカスタマイズ

;; ;; ホームポジションでの候補選択（Vim風）
;; (with-eval-after-load 'nskk-keymap
;;   (define-key nskk-kouho-mode-map (kbd "j") 'nskk-kouho-next)
;;   (define-key nskk-kouho-mode-map (kbd "k") 'nskk-kouho-previous)
;;   (define-key nskk-kouho-mode-map (kbd "h") 'nskk-kouho-prev-group)
;;   (define-key nskk-kouho-mode-map (kbd "l") 'nskk-kouho-next-group))

;; 1-4. プレフィックスキーの変更

;; (setq nskk-keymap-prefix "C-c j")  ; C-x j から C-c j に変更

;; 1-5. 変換開始キーの追加

;; ;; SPCに加えてTABでも変換開始
;; (define-key nskk-hiragana-mode-map (kbd "TAB") 'nskk-henkan-start-or-next)

;; 1-6. キャンセルキーの追加

;; ;; C-gに加えてESCでもキャンセル
;; (define-key nskk-henkan-mode-map (kbd "ESC") 'nskk-henkan-cancel)
;; (define-key nskk-kouho-mode-map (kbd "ESC") 'nskk-kouho-cancel)

;; 1-7. グローバルキーバインドの整理

;; (global-set-key (kbd "C-x C-j") 'nskk-mode)          ; NSKK切り替え
;; (global-set-key (kbd "C-x j") 'nskk-mode-toggle)     ; トグル
;; (global-set-key (kbd "C-x M-j") 'nskk-save-jisyo)    ; 辞書保存

;;; ========================================
;;; 2. UIカスタマイズ
;;; ========================================

;; 2-1. フェイス（色・フォント）のカスタマイズ

;; ;; 変換中の文字色
;; (custom-set-faces
;;  '(nskk-henkan-face ((t (:foreground "yellow" :background "blue"))))
;;  '(nskk-kouho-face ((t (:foreground "white" :background "darkgreen"))))
;;  '(nskk-okuri-face ((t (:foreground "cyan" :underline t)))))

;; 2-2. 候補ウィンドウのカスタマイズ

;; ;; 候補ウィンドウの位置とサイズ
;; (setq nskk-candidate-window-position 'bottom  ; 'top, 'bottom, 'left, 'right
;;       nskk-candidate-window-height 10         ; ウィンドウの高さ（行数）
;;       nskk-candidate-window-width 60)         ; ウィンドウの幅（文字数）

;; ;; 候補の表示形式
;; (setq nskk-candidate-display-format
;;       '(:index-format "[%d]"          ; インデックス表示
;;         :separator " | "              ; 候補の区切り文字
;;         :show-annotation t            ; 注釈を表示
;;         :annotation-format " <%s>"))  ; 注釈の形式

;; 2-3. モードラインのカスタマイズ

;; ;; モードライン表示のカスタマイズ
;; (setq nskk-modeline-format
;;       '(:eval
;;         (concat
;;          " ["
;;          (propertize (nskk-state-mode-indicator
;;                      (nskk-state-mode nskk-current-state))
;;                     'face 'nskk-mode-indicator-face)
;;          "]")))

;; ;; モードインジケーターの色設定
;; (custom-set-faces
;;  '(nskk-mode-indicator-face ((t (:foreground "green" :bold t)))))

;; 2-4. ポップアップ表示のカスタマイズ

;; ;; posframeを使った候補表示（要posframeパッケージ）
;; (when (require 'posframe nil t)
;;   (setq nskk-candidate-use-posframe t
;;         nskk-posframe-parameters
;;         '(:internal-border-width 2
;;           :internal-border-color "gray50"
;;           :background-color "black")))

;; 2-5. アニメーション設定

;; ;; モード切り替え時のアニメーション
;; (setq nskk-enable-mode-transition-animation t
;;       nskk-animation-duration 0.2)  ; 秒

;;; ========================================
;;; 3. 辞書カスタマイズ
;;; ========================================

;; 3-1. 複数辞書の統合

;; ;; 優先順位付き辞書リスト
;; (setq nskk-dictionary-list
;;       '(("~/dict/personal.dic" . personal)        ; 個人辞書（最優先）
;;         ("~/dict/tech.dic" . technical)           ; 技術用語辞書
;;         ("~/dict/SKK-JISYO.L" . system)           ; システム辞書
;;         ("~/dict/SKK-JISYO.jinmei" . names)       ; 人名辞書
;;         ("~/dict/SKK-JISYO.geo" . geography)      ; 地名辞書
;;         ("~/dict/SKK-JISYO.propernoun" . proper)  ; 固有名詞
;;         ("~/dict/SKK-JISYO.emoji" . emoji)))      ; 絵文字辞書

;; 3-2. 辞書の自動保存設定

;; (setq nskk-save-jisyo-instantly t           ; 即座に保存
;;       nskk-jisyo-save-count 50              ; 50回変換ごとに保存
;;       nskk-backup-jisyo t                   ; バックアップ作成
;;       nskk-backup-jisyo-directory "~/dict/backup/")

;; 3-3. 辞書サーバーの設定

;; ;; SKK辞書サーバーを使用
;; (setq nskk-use-dictionary-server t
;;       nskk-dictionary-server-host "localhost"
;;       nskk-dictionary-server-port 1178
;;       nskk-dictionary-server-timeout 5)  ; タイムアウト（秒）

;; 3-4. 動的辞書生成

;; ;; カスタム辞書エントリの追加
;; (defun nskk-add-custom-entry (key candidates)
;;   "カスタム辞書エントリを追加"
;;   (nskk-dict-add-entry nskk-personal-dictionary key candidates))

;; ;; 使用例: 略語展開
;; (nskk-add-custom-entry "addr" '("住所" "Address"))
;; (nskk-add-custom-entry "tel" '("電話番号" "Telephone"))

;; 3-5. 辞書のマージ

;; ;; 複数の個人辞書をマージ
;; (defun nskk-merge-personal-dictionaries (&rest dict-files)
;;   "複数の辞書をマージ"
;;   (let ((merged (make-hash-table :test 'equal)))
;;     (dolist (file dict-files)
;;       (when (file-exists-p file)
;;         (nskk--merge-into merged (nskk-dict-load file))))
;;     merged))

;;; ========================================
;;; 4. キャッシュ設定
;;; ========================================

;; 4-1. キャッシュサイズの調整

;; ;; 環境に応じたキャッシュサイズ
;; (setq nskk-dictionary-cache-size
;;       (cond
;;        ;; 8GB以上のメモリ
;;        ((> (nskk-available-memory) (* 8 1024 1024 1024))
;;         50000)
;;        ;; 4GB以上のメモリ
;;        ((> (nskk-available-memory) (* 4 1024 1024 1024))
;;         20000)
;;        ;; それ以外
;;        (t 10000)))

;; 4-2. キャッシュアルゴリズムの選択

;; (setq nskk-cache-type 'lru          ; 'lru または 'lfu
;;       nskk-cache-ttl 3600)          ; キャッシュ有効期限（秒）

;; 4-3. プリロード設定

;; ;; 起動時のプリロード
;; (setq nskk-dictionary-preload t              ; プリロード有効
;;       nskk-preload-common-words t            ; 頻出語のプリロード
;;       nskk-preload-word-list
;;       '("あいさつ" "こんにちは" "ありがとう" "よろしく"))

;; 4-4. キャッシュのクリア

;; ;; 定期的なキャッシュクリア（1時間ごと）
;; (run-with-timer 3600 3600 'nskk-cache-clear)

;; 4-5. キャッシュ統計の監視

;; ;; キャッシュヒット率のモニタリング
;; (add-hook 'nskk-after-conversion-hook
;;           (lambda ()
;;             (let ((stats (nskk-cache-stats nskk--dictionary-cache)))
;;               (when (< (plist-get stats :hit-rate) 0.8)
;;                 (message "Cache hit rate low: %.2f"
;;                         (plist-get stats :hit-rate))))))

;;; ========================================
;;; 5. フックカスタマイズ
;;; ========================================

;; 5-1. モード切り替え時の処理

;; ;; ひらがなモードに切り替わったときの処理
;; (add-hook 'nskk-mode-hook
;;           (lambda ()
;;             (when (eq (nskk-state-mode nskk-current-state) 'hiragana)
;;               (message "ひらがなモードに切り替わりました"))))

;; 5-2. 変換完了時の処理

;; ;; 変換完了時に音を鳴らす
;; (add-hook 'nskk-after-conversion-hook
;;           (lambda ()
;;             (when (and (fboundp 'play-sound-file)
;;                       (file-exists-p "~/sounds/conversion.wav"))
;;               (play-sound-file "~/sounds/conversion.wav"))))

;; 5-3. 辞書保存時の処理

;; ;; 辞書保存時にバックアップ作成
;; (add-hook 'nskk-before-save-jisyo-hook
;;           (lambda ()
;;             (let ((backup-file
;;                    (format "~/dict/backup/personal-%s.dic"
;;                           (format-time-string "%Y%m%d-%H%M%S"))))
;;               (copy-file nskk-personal-dictionary backup-file t))))

;; 5-4. エラー時の処理

;; ;; 変換エラー時のフォールバック
;; (add-hook 'nskk-conversion-error-hook
;;           (lambda (error-data)
;;             (message "変換エラー: %s" error-data)
;;             ;; ログファイルに記録
;;             (append-to-file
;;              (format "[%s] Conversion error: %s\n"
;;                     (format-time-string "%Y-%m-%d %H:%M:%S")
;;                     error-data)
;;              nil "~/nskk-errors.log")))

;; 5-5. バッファ切り替え時の処理

;; ;; バッファ切り替え時にモードを保存
;; (defvar nskk-buffer-mode-alist nil
;;   "バッファごとのNSKKモード保存")

;; (add-hook 'buffer-list-update-hook
;;           (lambda ()
;;             (when (and (boundp 'nskk-current-state)
;;                       nskk-current-state)
;;               (setf (alist-get (current-buffer) nskk-buffer-mode-alist)
;;                     (nskk-state-mode nskk-current-state)))))

;;; ========================================
;;; 6. モードライン設定
;;; ========================================

;; 6-1. シンプルなモードライン

;; (setq nskk-modeline-format
;;       '(:eval (nskk-state-mode-indicator
;;               (nskk-state-mode nskk-current-state))))

;; 6-2. 詳細なモードライン

;; (setq nskk-modeline-format
;;       '(:eval
;;         (concat
;;          "["
;;          (nskk-state-mode-indicator (nskk-state-mode nskk-current-state))
;;          (when (nskk-state-in-conversion-p nskk-current-state)
;;            (format " %d/%d"
;;                   (1+ (nskk-state-candidate-index nskk-current-state))
;;                   (length (nskk-state-candidates nskk-current-state))))
;;          "]")))

;; 6-3. 色付きモードライン

;; (defun nskk-modeline-with-color ()
;;   "色付きモードライン表示"
;;   (let* ((mode (nskk-state-mode nskk-current-state))
;;          (indicator (nskk-state-mode-indicator mode))
;;          (color (pcase mode
;;                  ('hiragana "green")
;;                  ('katakana "blue")
;;                  ('latin "gray")
;;                  ('conversion "yellow")
;;                  (_ "white"))))
;;     (propertize (format "[%s]" indicator)
;;                'face `(:foreground ,color :weight bold))))

;; (setq nskk-modeline-format '(:eval (nskk-modeline-with-color)))

;; 6-4. アイコン付きモードライン（要all-the-iconsパッケージ）

;; (when (require 'all-the-icons nil t)
;;   (defun nskk-modeline-with-icon ()
;;     "アイコン付きモードライン"
;;     (let ((mode (nskk-state-mode nskk-current-state)))
;;       (concat
;;        (pcase mode
;;          ('hiragana (all-the-icons-faicon "language" :v-adjust 0.0))
;;          ('katakana (all-the-icons-faicon "font" :v-adjust 0.0))
;;          ('latin (all-the-icons-faicon "keyboard-o" :v-adjust 0.0))
;;          (_ ""))
;;        " "
;;        (nskk-state-mode-indicator mode))))
;;   (setq nskk-modeline-format '(:eval (nskk-modeline-with-icon))))

;;; ========================================
;;; 7. 特殊用途向けカスタマイズ
;;; ========================================

;; 7-1. プログラミング用設定

;; ;; プログラミングモードでの特別設定
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (when nskk-mode
;;               ;; 変数名・関数名用の辞書を優先
;;               (setq-local nskk-dictionary-list
;;                          (cons '("~/dict/programming.dic" . programming)
;;                                nskk-dictionary-list))
;;               ;; スネークケース・キャメルケース対応
;;               (setq-local nskk-enable-case-conversion t))))

;; 7-2. Org-mode用設定

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (when nskk-mode
;;               ;; 見出しでの自動確定
;;               (setq-local nskk-auto-kakutei-in-heading t)
;;               ;; リンク内では無効
;;               (add-hook 'org-link-edit-hook
;;                        (lambda () (nskk-mode -1))
;;                        nil t))))

;; 7-3. メール用設定

;; (add-hook 'message-mode-hook
;;           (lambda ()
;;             ;; 署名の自動挿入
;;             (setq-local nskk-signature
;;                        "よろしくお願いいたします。\n\n氏名")
;;             ;; 挨拶文のテンプレート
;;             (setq-local nskk-greeting-templates
;;                        '(("お世話になっております。")
;;                          ("いつもお世話になっております。")))))

;; 7-4. Markdown用設定

;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             ;; コードブロック内では無効
;;             (add-hook 'markdown-code-block-hook
;;                      (lambda () (nskk-mode -1))
;;                      nil t)))

;;; ========================================
;;; 8. パフォーマンスチューニング
;;; ========================================

;; 8-1. メモリ使用量の最適化

;; ;; GC設定の調整
;; (setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
;;       gc-cons-percentage 0.6)

;; ;; NSKKのメモリプール設定
;; (setq nskk-use-memory-pool t
;;       nskk-memory-pool-size 1000)

;; 8-2. CPUコア数に応じた並列処理

;; (setq nskk-thread-pool-size
;;       (max 2 (/ (num-processors) 2)))  ; コア数の半分

;; 8-3. 検索アルゴリズムの選択

;; (setq nskk-search-algorithm 'trie     ; 'trie, 'hash, 'binary-search
;;       nskk-enable-fuzzy-search nil)   ; ファジー検索は負荷が高い

;; 8-4. 非同期処理の活用

;; (setq nskk-async-dictionary-loading t    ; 辞書の非同期読み込み
;;       nskk-async-learning-update t)      ; 学習データの非同期更新

;;; ========================================
;;; 9. 統合カスタマイズ例
;;; ========================================

;; 9-1. 完全カスタマイズ設定

;; (defun my-nskk-ultimate-setup ()
;;   "究極のNSKKカスタマイズ"
;;   (interactive)

;;   ;; キーバインド
;;   (setq nskk-keymap-sticky-key "@")
;;   (define-key nskk-kouho-mode-map (kbd "j") 'nskk-kouho-next)
;;   (define-key nskk-kouho-mode-map (kbd "k") 'nskk-kouho-previous)

;;   ;; UI
;;   (setq nskk-candidate-window-position 'bottom
;;         nskk-candidate-window-height 10)
;;   (custom-set-faces
;;    '(nskk-henkan-face ((t (:foreground "yellow" :background "blue")))))

;;   ;; 辞書
;;   (setq nskk-dictionary-list
;;         '(("~/dict/personal.dic" . personal)
;;           ("~/dict/tech.dic" . technical)
;;           ("~/dict/SKK-JISYO.L" . system)))

;;   ;; パフォーマンス
;;   (setq nskk-dictionary-cache-size 50000
;;         nskk-use-trie-index t
;;         nskk-dictionary-preload t)

;;   ;; フック
;;   (add-hook 'nskk-after-conversion-hook 'my-nskk-log-conversion)

;;   (message "My NSKK setup completed!"))

;; ;; 起動時に実行
;; (with-eval-after-load 'nskk
;;   (my-nskk-ultimate-setup))

;;; ========================================
;;; 補足情報
;;; ========================================

;; カスタマイズの確認:
;; - M-x describe-variable RET nskk-<変数名> RET
;; - M-x customize-group RET nskk RET

;; パフォーマンステスト:
;; - M-x nskk-benchmark-all
;; - M-x nskk-cache-stats

;; 設定のリセット:
;; - M-x nskk-reset-configuration

;; 詳細なドキュメント:
;; - How-toガイド: docs/how-to/advanced-customization.md
;; - APIリファレンス: docs/reference/api-reference.md

(provide 'customization)

;;; customization.el ends here
