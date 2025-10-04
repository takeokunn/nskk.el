;;; troubleshooting.el --- NSKK troubleshooting guide -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, troubleshooting
;; Version: 0.1.0

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKのトラブルシューティングガイドです。
;;
;; 含まれる内容:
;; 1. よくある問題と解決策
;; 2. デバッグ方法
;; 3. ログ出力設定
;; 4. パフォーマンス問題の診断
;; 5. 辞書関連の問題
;; 6. 設定の検証
;;
;; 使い方:
;; 1. 問題の症状を確認
;; 2. 該当する診断コードを実行
;; 3. 解決策を試す
;; 4. 必要に応じてログを確認

;;; Code:

;;; ========================================
;;; 1. よくある問題と解決策
;;; ========================================

;; 問題1: NSKKが起動しない

;; 診断コード:
;; (defun nskk-diagnose-startup ()
;;   "起動問題の診断"
;;   (interactive)
;;   (let ((issues nil))
;;     ;; NSKKがロードされているか確認
;;     (unless (featurep 'nskk)
;;       (push "NSKK is not loaded. Check load-path." issues))

;;     ;; 必須ファイルの存在確認
;;     (dolist (file '("nskk-state.el" "nskk-keymap.el" "nskk-converter.el"))
;;       (unless (locate-library file)
;;         (push (format "Required file not found: %s" file) issues)))

;;     ;; 結果表示
;;     (if issues
;;         (message "Startup issues found:\n%s"
;;                 (mapconcat 'identity issues "\n"))
;;       (message "No startup issues detected."))))

;; 解決策:
;; 1. load-pathを確認
;;    (add-to-list 'load-path "/path/to/nskk.el")
;; 2. requireの前にload-pathを設定
;;    (add-to-list 'load-path "/path/to/nskk.el")
;;    (require 'nskk)
;; 3. ファイルの完全性を確認
;;    $ git pull origin main

;; ----------------------------------------

;; 問題2: 辞書が読み込めない

;; 診断コード:
;; (defun nskk-diagnose-dictionary ()
;;   "辞書問題の診断"
;;   (interactive)
;;   (let ((issues nil))
;;     ;; 辞書ファイルの存在確認
;;     (dolist (dict nskk-dictionary-list)
;;       (let ((path (car dict)))
;;         (unless (file-exists-p path)
;;           (push (format "Dictionary not found: %s" path) issues))))

;;     ;; 辞書ファイルの読み込み権限確認
;;     (dolist (dict nskk-dictionary-list)
;;       (let ((path (car dict)))
;;         (when (file-exists-p path)
;;           (unless (file-readable-p path)
;;             (push (format "Dictionary not readable: %s" path) issues)))))

;;     ;; 辞書形式の確認
;;     (dolist (dict nskk-dictionary-list)
;;       (let ((path (car dict)))
;;         (when (file-exists-p path)
;;           (unless (nskk-dict-valid-format-p path)
;;             (push (format "Invalid dictionary format: %s" path) issues)))))

;;     ;; 結果表示
;;     (if issues
;;         (with-output-to-temp-buffer "*NSKK Dictionary Diagnosis*"
;;           (princ "Dictionary issues found:\n\n")
;;           (dolist (issue issues)
;;             (princ (format "- %s\n" issue))))
;;       (message "All dictionaries are valid."))))

;; 解決策:
;; 1. 辞書パスの確認
;;    (setq nskk-dictionary-list
;;          '(("~/dict/personal.dic" . personal)
;;            ("~/dict/SKK-JISYO.L" . system)))
;; 2. 辞書のダウンロード
;;    $ mkdir -p ~/dict
;;    $ curl -o ~/dict/SKK-JISYO.L.gz http://openlab.jp/skk/dic/SKK-JISYO.L.gz
;;    $ gunzip ~/dict/SKK-JISYO.L.gz
;; 3. 権限の確認
;;    $ chmod 644 ~/dict/SKK-JISYO.L
;; 4. 文字コードの確認（UTF-8であること）
;;    $ file ~/dict/SKK-JISYO.L

;; ----------------------------------------

;; 問題3: 日本語が入力できない

;; 診断コード:
;; (defun nskk-diagnose-input ()
;;   "入力問題の診断"
;;   (interactive)
;;   (let ((issues nil))
;;     ;; NSKKモードが有効か確認
;;     (unless (bound-and-true-p nskk-mode)
;;       (push "NSKK mode is not enabled. Try C-x C-j." issues))

;;     ;; 状態の確認
;;     (unless (bound-and-true-p nskk-current-state)
;;       (push "NSKK state is not initialized." issues))

;;     ;; キーマップの確認
;;     (unless (keymapp nskk-mode-map)
;;       (push "NSKK keymap is not properly set." issues))

;;     ;; ローマ字テーブルの確認
;;     (unless (bound-and-true-p nskk-romaji-table)
;;       (push "Romaji table is not loaded." issues))

;;     ;; 結果表示
;;     (if issues
;;         (message "Input issues found:\n%s"
;;                 (mapconcat 'identity issues "\n"))
;;       (message "Input system is working correctly."))))

;; 解決策:
;; 1. NSKKモードの有効化
;;    M-x nskk-mode
;;    または
;;    C-x C-j
;; 2. 状態の初期化
;;    (nskk-state-init)
;; 3. キーバインドの確認
;;    M-x describe-key C-x C-j
;; 4. Emacsの再起動

;; ----------------------------------------

;; 問題4: 変換候補が表示されない

;; 診断コード:
;; (defun nskk-diagnose-conversion ()
;;   "変換問題の診断"
;;   (interactive)
;;   (let ((issues nil)
;;         (test-key "あ"))

;;     ;; 辞書検索のテスト
;;     (unless (nskk-dict-search test-key)
;;       (push "Dictionary search is not working." issues))

;;     ;; 候補生成のテスト
;;     (let ((candidates (nskk-generate-candidates test-key)))
;;       (unless candidates
;;         (push "Candidate generation failed." issues)))

;;     ;; キャッシュの確認
;;     (when (and (boundp 'nskk--dictionary-cache)
;;               nskk--dictionary-cache)
;;       (let ((stats (nskk-cache-stats nskk--dictionary-cache)))
;;         (when (< (plist-get stats :size) 1)
;;           (push "Dictionary cache is empty." issues))))

;;     ;; 結果表示
;;     (if issues
;;         (message "Conversion issues found:\n%s"
;;                 (mapconcat 'identity issues "\n"))
;;       (message "Conversion system is working correctly."))))

;; 解決策:
;; 1. 辞書の再読み込み
;;    M-x nskk-reload-jisyo
;; 2. キャッシュのクリア
;;    (nskk-cache-clear nskk--dictionary-cache)
;; 3. 辞書インデックスの再構築
;;    (nskk-rebuild-dictionary-index)

;; ----------------------------------------

;; 問題5: パフォーマンスが遅い

;; 診断コード:
;; (defun nskk-diagnose-performance ()
;;   "パフォーマンス問題の診断"
;;   (interactive)
;;   (let ((results nil))

;;     ;; 応答時間の測定
;;     (let ((start (current-time))
;;           (end nil))
;;       (nskk-convert-romaji "ka")
;;       (setq end (current-time))
;;       (let ((elapsed (float-time (time-subtract end start))))
;;         (push (format "Romaji conversion time: %.3f ms" (* elapsed 1000))
;;               results)
;;         (when (> elapsed 0.001)
;;           (push "⚠ Conversion is too slow (>1ms)" results))))

;;     ;; 辞書検索時間の測定
;;     (let ((start (current-time))
;;           (end nil))
;;       (nskk-dict-search "か")
;;       (setq end (current-time))
;;       (let ((elapsed (float-time (time-subtract end start))))
;;         (push (format "Dictionary search time: %.3f ms" (* elapsed 1000))
;;               results)
;;         (when (> elapsed 0.050)
;;           (push "⚠ Dictionary search is slow (>50ms)" results))))

;;     ;; メモリ使用量の確認
;;     (when (fboundp 'memory-report)
;;       (let ((mem (memory-report)))
;;         (push (format "Memory usage: %s" mem) results)))

;;     ;; キャッシュヒット率の確認
;;     (when (and (boundp 'nskk--dictionary-cache)
;;               nskk--dictionary-cache)
;;       (let* ((stats (nskk-cache-stats nskk--dictionary-cache))
;;              (hit-rate (plist-get stats :hit-rate)))
;;         (push (format "Cache hit rate: %.1f%%" (* hit-rate 100)) results)
;;         (when (< hit-rate 0.7)
;;           (push "⚠ Cache hit rate is low (<70%)" results))))

;;     ;; 結果表示
;;     (with-output-to-temp-buffer "*NSKK Performance Diagnosis*"
;;       (princ "Performance Diagnosis Results:\n\n")
;;       (dolist (result (nreverse results))
;;         (princ (format "%s\n" result))))))

;; 解決策:
;; 1. キャッシュサイズの増加
;;    (setq nskk-dictionary-cache-size 50000)
;; 2. プリロードの有効化
;;    (setq nskk-dictionary-preload t)
;; 3. トライ木インデックスの使用
;;    (setq nskk-use-trie-index t)
;; 4. GC設定の調整
;;    (setq gc-cons-threshold (* 100 1024 1024))

;;; ========================================
;;; 2. デバッグ方法
;;; ========================================

;; 2-1. デバッグモードの有効化

;; (setq nskk-debug t                    ; デバッグモード有効
;;       debug-on-error t                ; エラー時にデバッガ起動
;;       nskk-log-level 'debug)          ; ログレベル: debug

;; 2-2. 詳細ログの有効化

;; (defun nskk-enable-verbose-logging ()
;;   "詳細ログを有効化"
;;   (interactive)
;;   (setq nskk-log-level 'debug
;;         nskk-log-file "~/nskk-debug.log"
;;         nskk-log-timestamp t
;;         nskk-log-caller t))

;; 2-3. 関数のトレース

;; ;; 特定の関数をトレース
;; (trace-function 'nskk-convert-romaji)
;; (trace-function 'nskk-dict-search)

;; ;; トレースの解除
;; (untrace-function 'nskk-convert-romaji)

;; 2-4. イベントログの確認

;; (defun nskk-show-event-log ()
;;   "イベントログを表示"
;;   (interactive)
;;   (when (boundp 'nskk-event-log)
;;     (with-output-to-temp-buffer "*NSKK Event Log*"
;;       (dolist (event nskk-event-log)
;;         (princ (format "[%s] %s: %s\n"
;;                       (format-time-string "%H:%M:%S" (plist-get event :time))
;;                       (plist-get event :type)
;;                       (plist-get event :data)))))))

;; 2-5. ステップ実行

;; ;; edebugを使用したステップ実行
;; ;; 1. 対象関数の定義部分で C-u C-M-x
;; ;; 2. 関数を実行
;; ;; 3. SPCでステップ実行、cで継続

;;; ========================================
;;; 3. ログ出力設定
;;; ========================================

;; 3-1. ログレベルの設定

;; (setq nskk-log-level 'info)  ; 'error, 'warning, 'info, 'debug

;; 3-2. ログファイルの設定

;; (setq nskk-log-file "~/nskk.log"        ; ログファイルパス
;;       nskk-log-max-size (* 10 1024 1024) ; 最大10MB
;;       nskk-log-rotation t)               ; ローテーション有効

;; 3-3. カスタムログ関数

;; (defun nskk-log (level format-string &rest args)
;;   "カスタムログ出力"
;;   (when (nskk-should-log-p level)
;;     (let ((message (apply 'format format-string args))
;;           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
;;       (append-to-file
;;        (format "[%s] [%s] %s\n" timestamp level message)
;;        nil nskk-log-file))))

;; ;; 使用例
;; (nskk-log 'info "Dictionary loaded: %s" dict-path)
;; (nskk-log 'error "Conversion failed: %s" error-message)

;; 3-4. ログビューアー

;; (defun nskk-view-log ()
;;   "ログファイルを表示"
;;   (interactive)
;;   (if (file-exists-p nskk-log-file)
;;       (find-file-read-only nskk-log-file)
;;     (message "Log file not found: %s" nskk-log-file)))

;; 3-5. ログのフィルタリング

;; (defun nskk-filter-log (level)
;;   "特定レベルのログのみ表示"
;;   (interactive
;;    (list (intern (completing-read "Log level: "
;;                                  '("error" "warning" "info" "debug")))))
;;   (when (file-exists-p nskk-log-file)
;;     (with-output-to-temp-buffer "*NSKK Filtered Log*"
;;       (with-temp-buffer
;;         (insert-file-contents nskk-log-file)
;;         (goto-char (point-min))
;;         (while (re-search-forward
;;                 (format "\\[%s\\]" (symbol-name level))
;;                 nil t)
;;           (princ (buffer-substring-no-properties
;;                  (line-beginning-position)
;;                  (line-end-position)))
;;           (princ "\n"))))))

;;; ========================================
;;; 4. パフォーマンス診断
;;; ========================================

;; 4-1. ベンチマークツール

;; (defun nskk-benchmark-conversion ()
;;   "変換処理のベンチマーク"
;;   (interactive)
;;   (let ((iterations 1000)
;;         (test-inputs '("ka" "ki" "ku" "ke" "ko"))
;;         (results nil))

;;     (dolist (input test-inputs)
;;       (let ((start (current-time)))
;;         (dotimes (_ iterations)
;;           (nskk-convert-romaji input))
;;         (let* ((end (current-time))
;;                (total (float-time (time-subtract end start)))
;;                (avg (/ total iterations)))
;;           (push (cons input (* avg 1000)) results))))

;;     (with-output-to-temp-buffer "*NSKK Benchmark Results*"
;;       (princ "Conversion Benchmark (1000 iterations):\n\n")
;;       (dolist (result (nreverse results))
;;         (princ (format "%s: %.3f ms\n" (car result) (cdr result)))))))

;; 4-2. プロファイリング

;; ;; ELPを使用したプロファイリング
;; (require 'elp)

;; (defun nskk-profile-start ()
;;   "プロファイリング開始"
;;   (interactive)
;;   (elp-instrument-package "nskk-"))

;; (defun nskk-profile-results ()
;;   "プロファイリング結果表示"
;;   (interactive)
;;   (elp-results))

;; (defun nskk-profile-reset ()
;;   "プロファイリングリセット"
;;   (interactive)
;;   (elp-reset-all)
;;   (elp-restore-all))

;; 4-3. メモリプロファイリング

;; (defun nskk-memory-profile ()
;;   "メモリ使用量のプロファイリング"
;;   (interactive)
;;   (garbage-collect)
;;   (let ((before (memory-use-counts)))
;;     ;; 何らかの処理
;;     (nskk-load-dictionary "~/dict/SKK-JISYO.L")
;;     (garbage-collect)
;;     (let* ((after (memory-use-counts))
;;            (diff (cl-mapcar '- after before)))
;;       (message "Memory usage: %s" diff))))

;; 4-4. ボトルネック検出

;; (defun nskk-detect-bottleneck ()
;;   "ボトルネック検出"
;;   (interactive)
;;   (let ((start (current-time))
;;         (checkpoints nil))

;;     ;; チェックポイント1: 辞書読み込み
;;     (nskk-load-dictionary "~/dict/SKK-JISYO.L")
;;     (push (cons "Dictionary loading"
;;                (float-time (time-since start)))
;;          checkpoints)

;;     ;; チェックポイント2: インデックス構築
;;     (nskk-build-index)
;;     (push (cons "Index building"
;;                (float-time (time-since start)))
;;          checkpoints)

;;     ;; チェックポイント3: キャッシュ初期化
;;     (nskk-init-cache)
;;     (push (cons "Cache initialization"
;;                (float-time (time-since start)))
;;          checkpoints)

;;     ;; 結果表示
;;     (with-output-to-temp-buffer "*NSKK Bottleneck Analysis*"
;;       (princ "Bottleneck Analysis:\n\n")
;;       (dolist (cp (nreverse checkpoints))
;;         (princ (format "%s: %.3f s\n" (car cp) (cdr cp)))))))

;;; ========================================
;;; 5. 設定の検証
;;; ========================================

;; 5-1. 総合診断

;; (defun nskk-comprehensive-check ()
;;   "総合診断"
;;   (interactive)
;;   (let ((report nil))

;;     ;; バージョン確認
;;     (push (format "NSKK Version: %s" nskk-version) report)
;;     (push (format "Emacs Version: %s" emacs-version) report)

;;     ;; 設定確認
;;     (push (format "Dictionary cache size: %d"
;;                  nskk-dictionary-cache-size)
;;          report)
;;     (push (format "Dictionary preload: %s"
;;                  (if nskk-dictionary-preload "enabled" "disabled"))
;;          report)

;;     ;; 辞書確認
;;     (push (format "Dictionaries: %d loaded"
;;                  (length nskk-dictionary-list))
;;          report)

;;     ;; パフォーマンス確認
;;     (let ((start (current-time)))
;;       (nskk-convert-romaji "ka")
;;       (push (format "Conversion time: %.3f ms"
;;                    (* (float-time (time-since start)) 1000))
;;            report))

;;     ;; 結果表示
;;     (with-output-to-temp-buffer "*NSKK Comprehensive Check*"
;;       (princ "NSKK Comprehensive Check:\n\n")
;;       (dolist (item (nreverse report))
;;         (princ (format "✓ %s\n" item))))))

;; 5-2. 設定の妥当性チェック

;; (defun nskk-validate-configuration ()
;;   "設定の妥当性をチェック"
;;   (interactive)
;;   (let ((warnings nil))

;;     ;; キャッシュサイズのチェック
;;     (when (> nskk-dictionary-cache-size 100000)
;;       (push "Cache size is very large (>100,000). May use too much memory."
;;            warnings))

;;     ;; 辞書数のチェック
;;     (when (> (length nskk-dictionary-list) 10)
;;       (push "Too many dictionaries (>10). May affect performance."
;;            warnings))

;;     ;; パスのチェック
;;     (dolist (dict nskk-dictionary-list)
;;       (let ((path (car dict)))
;;         (unless (file-name-absolute-p path)
;;           (push (format "Dictionary path is not absolute: %s" path)
;;                warnings))))

;;     ;; 結果表示
;;     (if warnings
;;         (with-output-to-temp-buffer "*NSKK Configuration Warnings*"
;;           (princ "Configuration Warnings:\n\n")
;;           (dolist (warning warnings)
;;             (princ (format "⚠ %s\n" warning))))
;;       (message "Configuration is valid."))))

;;; ========================================
;;; 6. 緊急対処法
;;; ========================================

;; 6-1. 完全リセット

;; (defun nskk-emergency-reset ()
;;   "緊急リセット - 全状態をクリア"
;;   (interactive)
;;   (when (yes-or-no-p "Really reset all NSKK states? ")
;;     ;; 状態のクリア
;;     (when (boundp 'nskk-current-state)
;;       (nskk-state-cleanup))

;;     ;; キャッシュのクリア
;;     (when (boundp 'nskk--dictionary-cache)
;;       (nskk-cache-clear nskk--dictionary-cache))

;;     ;; キーマップのリセット
;;     (nskk-reset-keybindings)

;;     ;; 再初期化
;;     (nskk-setup-keybindings)
;;     (nskk-state-init)

;;     (message "NSKK has been reset completely.")))

;; 6-2. セーフモード起動

;; (defun nskk-safe-mode ()
;;   "セーフモードで起動（最小限の機能のみ）"
;;   (interactive)
;;   ;; 最小限の設定
;;   (setq nskk-dictionary-cache-size 1000
;;         nskk-dictionary-preload nil
;;         nskk-use-trie-index nil
;;         nskk-learning-enabled nil)

;;   ;; 辞書を個人辞書のみに
;;   (setq nskk-dictionary-list
;;         '(("~/dict/personal.dic" . personal)))

;;   (message "NSKK is running in safe mode."))

;;; ========================================
;;; 補足情報
;;; ========================================

;; デバッグに役立つコマンド:
;; - M-x nskk-diagnose-startup        : 起動問題の診断
;; - M-x nskk-diagnose-dictionary     : 辞書問題の診断
;; - M-x nskk-diagnose-input          : 入力問題の診断
;; - M-x nskk-diagnose-conversion     : 変換問題の診断
;; - M-x nskk-diagnose-performance    : パフォーマンス診断
;; - M-x nskk-comprehensive-check     : 総合診断
;; - M-x nskk-view-log                : ログ表示
;; - M-x nskk-emergency-reset         : 緊急リセット

;; ログファイルの場所:
;; - デフォルト: ~/nskk.log
;; - カスタマイズ可能: (setq nskk-log-file "path/to/log")

;; サポート:
;; - GitHub Issues: https://github.com/takeokunn/nskk.el/issues
;; - ドキュメント: docs/

(provide 'troubleshooting)

;;; troubleshooting.el ends here
