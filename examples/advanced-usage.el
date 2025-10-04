;;; advanced-usage.el --- NSKK advanced usage examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, advanced
;; Version: 0.1.0

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKの高度な使用例を提供します。
;;
;; 含まれる内容:
;; 1. 複数辞書の統合と優先順位設定
;; 2. カスタム変換ルールの作成
;; 3. フックを活用した拡張機能
;; 4. パフォーマンスチューニング
;; 5. プロファイリングと最適化
;; 6. プラグインシステムの活用
;; 7. 外部ツールとの連携
;;
;; 使い方:
;; 1. 興味のあるセクションを参照
;; 2. コードをコピーしてカスタマイズ
;; 3. init.elに追加して試す

;;; Code:

;;; ========================================
;;; 1. 複数辞書の統合と優先順位設定
;;; ========================================

;; 1-1. 階層化辞書システム

;; (defvar my-nskk-dictionary-hierarchy
;;   '((personal    . 10)   ; 個人辞書（最優先）
;;     (technical   . 8)    ; 技術用語辞書
;;     (system      . 6)    ; システム辞書
;;     (names       . 5)    ; 人名辞書
;;     (geography   . 5)    ; 地名辞書
;;     (propernoun  . 4)    ; 固有名詞辞書
;;     (emoji       . 3))   ; 絵文字辞書
;;   "辞書の優先順位（数値が大きいほど優先）")

;; (defun my-nskk-setup-dictionary-hierarchy ()
;;   "階層化辞書システムのセットアップ"
;;   (setq nskk-dictionary-list
;;         (mapcar (lambda (entry)
;;                  (let* ((type (car entry))
;;                         (priority (cdr entry))
;;                         (path (my-nskk-get-dictionary-path type)))
;;                    (cons path (list :type type :priority priority))))
;;                 (sort my-nskk-dictionary-hierarchy
;;                       (lambda (a b) (> (cdr a) (cdr b)))))))

;; (defun my-nskk-get-dictionary-path (type)
;;   "辞書タイプに応じたパスを取得"
;;   (expand-file-name
;;    (pcase type
;;      ('personal "~/dict/personal.dic")
;;      ('technical "~/dict/technical.dic")
;;      ('system "~/dict/SKK-JISYO.L")
;;      ('names "~/dict/SKK-JISYO.jinmei")
;;      ('geography "~/dict/SKK-JISYO.geo")
;;      ('propernoun "~/dict/SKK-JISYO.propernoun")
;;      ('emoji "~/dict/SKK-JISYO.emoji")
;;      (_ (error "Unknown dictionary type: %s" type)))))

;; 1-2. 動的辞書マージ

;; (defun my-nskk-merge-dictionaries (dict-files output-file)
;;   "複数の辞書をマージ"
;;   (interactive
;;    (list (nskk-read-multiple-files "Dictionary files: ")
;;          (read-file-name "Output file: ")))
;;   (let ((merged-entries (make-hash-table :test 'equal)))
;;     ;; 各辞書を読み込んでマージ
;;     (dolist (file dict-files)
;;       (when (file-exists-p file)
;;         (with-temp-buffer
;;           (insert-file-contents file)
;;           (goto-char (point-min))
;;           (while (not (eobp))
;;             (when-let ((entry (nskk-parse-dictionary-line
;;                               (buffer-substring-no-properties
;;                                (line-beginning-position)
;;                                (line-end-position)))))
;;               (let* ((key (car entry))
;;                      (candidates (cdr entry))
;;                      (existing (gethash key merged-entries)))
;;                 ;; 既存の候補とマージ（重複削除）
;;                 (puthash key
;;                         (delete-dups (append existing candidates))
;;                         merged-entries)))
;;             (forward-line 1)))))
;;     ;; マージ結果を保存
;;     (with-temp-file output-file
;;       (maphash (lambda (key candidates)
;;                 (insert (format "%s /%s/\n"
;;                               key
;;                               (mapconcat 'identity candidates "/"))))
;;               merged-entries))
;;     (message "Merged %d entries into %s"
;;             (hash-table-count merged-entries)
;;             output-file)))

;; 1-3. コンテキスト別辞書切り替え

;; (defvar my-nskk-context-dictionaries
;;   '((prog-mode . ("~/dict/programming.dic" "~/dict/SKK-JISYO.L"))
;;     (text-mode . ("~/dict/literary.dic" "~/dict/SKK-JISYO.L"))
;;     (org-mode  . ("~/dict/org.dic" "~/dict/SKK-JISYO.L"))
;;     (mail-mode . ("~/dict/mail.dic" "~/dict/SKK-JISYO.L")))
;;   "モード別辞書設定")

;; (defun my-nskk-switch-dictionary-by-context ()
;;   "コンテキストに応じて辞書を切り替え"
;;   (when-let* ((mode major-mode)
;;              (dicts (alist-get mode my-nskk-context-dictionaries)))
;;     (setq-local nskk-dictionary-list
;;                (mapcar (lambda (path) (cons path 'context))
;;                       dicts))
;;     (nskk-reload-jisyo)))

;; ;; モード変更時に自動切り替え
;; (add-hook 'after-change-major-mode-hook
;;          'my-nskk-switch-dictionary-by-context)

;;; ========================================
;;; 2. カスタム変換ルール
;;; ========================================

;; 2-1. プログラミング用変換ルール

;; (defvar my-nskk-programming-rules
;;   '(("fn" . "function")
;;     ("cls" . "class")
;;     ("var" . "variable")
;;     ("const" . "constant")
;;     ("impl" . "implementation")
;;     ("ret" . "return")
;;     ("async" . "asynchronous")
;;     ("sync" . "synchronous"))
;;   "プログラミング用略語ルール")

;; (defun my-nskk-expand-programming-abbrev (abbrev)
;;   "プログラミング略語を展開"
;;   (or (alist-get abbrev my-nskk-programming-rules nil nil #'string=)
;;       abbrev))

;; (add-hook 'nskk-before-conversion-hook
;;          (lambda (input)
;;            (when (derived-mode-p 'prog-mode)
;;              (my-nskk-expand-programming-abbrev input))))

;; 2-2. 日付・時刻変換ルール

;; (defun my-nskk-convert-date-time (input)
;;   "日付・時刻の変換"
;;   (pcase input
;;     ;; 今日の日付
;;     ("today" (format-time-string "%Y年%m月%d日"))
;;     ("きょう" (format-time-string "%Y年%m月%d日"))

;;     ;; 現在時刻
;;     ("now" (format-time-string "%H時%M分"))
;;     ("いま" (format-time-string "%H時%M分"))

;;     ;; 曜日
;;     ("youbi" (format-time-string "%A"))
;;     ("ようび" (format-time-string "%A"))

;;     ;; 年
;;     ("year" (format-time-string "%Y年"))
;;     ("ねん" (format-time-string "%Y年"))

;;     ;; デフォルト
;;     (_ nil)))

;; (add-hook 'nskk-special-conversion-hook
;;          'my-nskk-convert-date-time)

;; 2-3. ケース変換ルール

;; (defun my-nskk-case-conversion (input case-type)
;;   "ケース変換"
;;   (pcase case-type
;;     ('snake-case
;;      (downcase (replace-regexp-in-string "[A-Z]" "_\\&" input)))
;;     ('kebab-case
;;      (downcase (replace-regexp-in-string "[A-Z]" "-\\&" input)))
;;     ('camel-case
;;      (let ((words (split-string input "[_-]")))
;;        (concat (downcase (car words))
;;               (mapconcat 'capitalize (cdr words) ""))))
;;     ('pascal-case
;;      (mapconcat 'capitalize (split-string input "[_-]") ""))
;;     (_ input)))

;; ;; 使用例
;; ;; (my-nskk-case-conversion "my_variable" 'camel-case)  ; => "myVariable"
;; ;; (my-nskk-case-conversion "MyClass" 'snake-case)      ; => "my_class"

;;; ========================================
;;; 3. フックを活用した拡張機能
;;; ========================================

;; 3-1. 変換履歴の記録

;; (defvar my-nskk-conversion-history nil
;;   "変換履歴")

;; (defvar my-nskk-max-history-size 1000
;;   "最大履歴サイズ")

;; (defun my-nskk-record-conversion (input output)
;;   "変換を履歴に記録"
;;   (push (list :timestamp (current-time)
;;              :input input
;;              :output output
;;              :mode major-mode)
;;        my-nskk-conversion-history)
;;   ;; 履歴サイズの制限
;;   (when (> (length my-nskk-conversion-history)
;;           my-nskk-max-history-size)
;;     (setq my-nskk-conversion-history
;;          (butlast my-nskk-conversion-history))))

;; (add-hook 'nskk-after-conversion-hook
;;          (lambda () (my-nskk-record-conversion input output)))

;; ;; 履歴の表示
;; (defun my-nskk-show-conversion-history ()
;;   "変換履歴を表示"
;;   (interactive)
;;   (with-output-to-temp-buffer "*NSKK Conversion History*"
;;     (princ "Recent conversions:\n\n")
;;     (dolist (entry (cl-subseq my-nskk-conversion-history
;;                              0 (min 50 (length my-nskk-conversion-history))))
;;       (princ (format "[%s] %s → %s (%s)\n"
;;                     (format-time-string "%H:%M:%S" (plist-get entry :timestamp))
;;                     (plist-get entry :input)
;;                     (plist-get entry :output)
;;                     (plist-get entry :mode))))))

;; 3-2. 学習データの分析

;; (defun my-nskk-analyze-learning-data ()
;;   "学習データを分析"
;;   (interactive)
;;   (let ((word-freq (make-hash-table :test 'equal))
;;         (total-conversions 0))

;;     ;; 履歴から頻度を集計
;;     (dolist (entry my-nskk-conversion-history)
;;       (let ((output (plist-get entry :output)))
;;         (puthash output
;;                 (1+ (gethash output word-freq 0))
;;                 word-freq)
;;         (cl-incf total-conversions)))

;;     ;; トップ10を表示
;;     (with-output-to-temp-buffer "*NSKK Learning Analysis*"
;;       (princ (format "Total conversions: %d\n\n" total-conversions))
;;       (princ "Top 10 most used words:\n\n")
;;       (let ((sorted (sort (hash-table-to-alist word-freq)
;;                          (lambda (a b) (> (cdr a) (cdr b))))))
;;         (dotimes (i (min 10 (length sorted)))
;;           (let ((entry (nth i sorted)))
;;             (princ (format "%2d. %s (%d times, %.1f%%)\n"
;;                          (1+ i)
;;                          (car entry)
;;                          (cdr entry)
;;                          (* 100.0 (/ (float (cdr entry)) total-conversions))))))))))

;; 3-3. 自動バックアップ

;; (defvar my-nskk-backup-interval 300
;;   "バックアップ間隔（秒）")

;; (defvar my-nskk-backup-directory "~/dict/backup/"
;;   "バックアップディレクトリ")

;; (defun my-nskk-auto-backup ()
;;   "個人辞書の自動バックアップ"
;;   (when (and (boundp 'nskk-personal-dictionary)
;;             (file-exists-p nskk-personal-dictionary))
;;     (let ((backup-file
;;            (expand-file-name
;;             (format "personal-%s.dic"
;;                    (format-time-string "%Y%m%d-%H%M%S"))
;;             my-nskk-backup-directory)))
;;       (unless (file-directory-p my-nskk-backup-directory)
;;         (make-directory my-nskk-backup-directory t))
;;       (copy-file nskk-personal-dictionary backup-file t)
;;       (message "Dictionary backed up to %s" backup-file))))

;; ;; 定期的なバックアップ
;; (run-with-timer my-nskk-backup-interval
;;                my-nskk-backup-interval
;;                'my-nskk-auto-backup)

;; 3-4. 変換精度の監視

;; (defvar my-nskk-accuracy-stats
;;   '(:first-candidate-hits 0
;;     :total-conversions 0
;;     :average-candidate-index 0.0)
;;   "変換精度統計")

;; (defun my-nskk-track-accuracy (candidate-index)
;;   "変換精度を追跡"
;;   (when (= candidate-index 0)
;;     (cl-incf (plist-get my-nskk-accuracy-stats :first-candidate-hits)))
;;   (cl-incf (plist-get my-nskk-accuracy-stats :total-conversions))

;;   ;; 平均インデックスの更新
;;   (let* ((total (plist-get my-nskk-accuracy-stats :total-conversions))
;;          (current-avg (plist-get my-nskk-accuracy-stats :average-candidate-index))
;;          (new-avg (+ (* current-avg (/ (float (1- total)) total))
;;                     (/ (float candidate-index) total))))
;;     (plist-put my-nskk-accuracy-stats :average-candidate-index new-avg)))

;; (add-hook 'nskk-after-candidate-selection-hook
;;          'my-nskk-track-accuracy)

;; ;; 精度レポート
;; (defun my-nskk-accuracy-report ()
;;   "変換精度レポート"
;;   (interactive)
;;   (let* ((hits (plist-get my-nskk-accuracy-stats :first-candidate-hits))
;;          (total (plist-get my-nskk-accuracy-stats :total-conversions))
;;          (avg-idx (plist-get my-nskk-accuracy-stats :average-candidate-index))
;;          (accuracy (if (> total 0) (/ (* 100.0 hits) total) 0.0)))
;;     (message "First candidate accuracy: %.1f%% (avg index: %.2f)"
;;             accuracy avg-idx)))

;;; ========================================
;;; 4. パフォーマンスチューニング
;;; ========================================

;; 4-1. 適応的キャッシュサイズ調整

;; (defvar my-nskk-cache-adjustment-interval 60
;;   "キャッシュサイズ調整間隔（秒）")

;; (defun my-nskk-adjust-cache-size ()
;;   "キャッシュサイズを動的に調整"
;;   (when (and (boundp 'nskk--dictionary-cache)
;;             nskk--dictionary-cache)
;;     (let* ((stats (nskk-cache-stats nskk--dictionary-cache))
;;            (hit-rate (plist-get stats :hit-rate))
;;            (current-size (plist-get stats :capacity)))

;;       (cond
;;        ;; ヒット率が低い → サイズを増やす
;;        ((< hit-rate 0.7)
;;         (let ((new-size (min (* current-size 1.5) 100000)))
;;           (setq nskk-dictionary-cache-size new-size)
;;           (message "Increased cache size to %d (hit rate: %.1f%%)"
;;                   new-size (* hit-rate 100))))

;;        ;; ヒット率が高い → サイズを減らす（メモリ節約）
;;        ((> hit-rate 0.95)
;;         (let ((new-size (max (* current-size 0.8) 1000)))
;;           (setq nskk-dictionary-cache-size new-size)
;;           (message "Decreased cache size to %d (hit rate: %.1f%%)"
;;                   new-size (* hit-rate 100))))))))

;; ;; 定期的な調整
;; (run-with-timer my-nskk-cache-adjustment-interval
;;                my-nskk-cache-adjustment-interval
;;                'my-nskk-adjust-cache-size)

;; 4-2. メモリプール最適化

;; (defvar my-nskk-object-pools
;;   '((candidates . nil)
;;     (strings . nil)
;;     (states . nil))
;;   "オブジェクトプール")

;; (defun my-nskk-get-pooled-candidate ()
;;   "プールから候補オブジェクトを取得"
;;   (or (pop (alist-get 'candidates my-nskk-object-pools))
;;       (make-nskk-candidate)))

;; (defun my-nskk-return-candidate (candidate)
;;   "候補オブジェクトをプールに返却"
;;   (nskk-candidate-reset candidate)
;;   (push candidate (alist-get 'candidates my-nskk-object-pools)))

;; 4-3. 遅延読み込み辞書

;; (defvar my-nskk-lazy-dictionaries
;;   '(("~/dict/SKK-JISYO.emoji" . emoji)
;;     ("~/dict/SKK-JISYO.propernoun" . propernoun))
;;   "遅延読み込み辞書")

;; (defun my-nskk-load-dictionary-on-demand (dict-type)
;;   "必要に応じて辞書を読み込む"
;;   (unless (my-nskk-dictionary-loaded-p dict-type)
;;     (when-let ((path (car (rassoc dict-type my-nskk-lazy-dictionaries))))
;;       (nskk-load-dictionary path)
;;       (message "Loaded dictionary: %s" path))))

;;; ========================================
;;; 5. プロファイリングと最適化
;;; ========================================

;; 5-1. 詳細プロファイリング

;; (defvar my-nskk-profiling-data (make-hash-table :test 'eq)
;;   "プロファイリングデータ")

;; (defmacro my-nskk-profile (name &rest body)
;;   "プロファイリング付き実行"
;;   `(let ((start (current-time)))
;;      (prog1 (progn ,@body)
;;        (let* ((end (current-time))
;;               (elapsed (float-time (time-subtract end start)))
;;               (data (gethash ',name my-nskk-profiling-data)))
;;          (if data
;;              (puthash ',name
;;                      (list :count (1+ (plist-get data :count))
;;                            :total-time (+ (plist-get data :total-time) elapsed)
;;                            :max-time (max (plist-get data :max-time) elapsed)
;;                            :min-time (min (plist-get data :min-time) elapsed))
;;                      my-nskk-profiling-data)
;;            (puthash ',name
;;                    (list :count 1
;;                          :total-time elapsed
;;                          :max-time elapsed
;;                          :min-time elapsed)
;;                    my-nskk-profiling-data))))))

;; ;; 使用例
;; ;; (my-nskk-profile conversion
;; ;;   (nskk-convert-romaji "ka"))

;; ;; プロファイリング結果の表示
;; (defun my-nskk-show-profile-results ()
;;   "プロファイリング結果を表示"
;;   (interactive)
;;   (with-output-to-temp-buffer "*NSKK Profile Results*"
;;     (princ "NSKK Profiling Results:\n\n")
;;     (princ (format "%-20s %8s %12s %12s %12s %12s\n"
;;                   "Function" "Calls" "Total(ms)" "Avg(ms)" "Min(ms)" "Max(ms)"))
;;     (princ (make-string 80 ?-))
;;     (princ "\n")
;;     (maphash (lambda (name data)
;;               (let ((count (plist-get data :count))
;;                     (total (* 1000 (plist-get data :total-time)))
;;                     (max-t (* 1000 (plist-get data :max-time)))
;;                     (min-t (* 1000 (plist-get data :min-time))))
;;                 (princ (format "%-20s %8d %12.3f %12.3f %12.3f %12.3f\n"
;;                               name count total (/ total count) min-t max-t))))
;;             my-nskk-profiling-data)))

;; 5-2. ホットスポット検出

;; (defun my-nskk-find-hotspots ()
;;   "ホットスポット（遅い箇所）を検出"
;;   (interactive)
;;   (let ((hotspots nil))
;;     (maphash (lambda (name data)
;;               (let ((avg-time (/ (plist-get data :total-time)
;;                                 (plist-get data :count))))
;;                 (when (> avg-time 0.001)  ; 1ms以上
;;                   (push (cons name avg-time) hotspots))))
;;             my-nskk-profiling-data)

;;     (setq hotspots (sort hotspots (lambda (a b) (> (cdr a) (cdr b)))))

;;     (with-output-to-temp-buffer "*NSKK Hotspots*"
;;       (princ "Performance Hotspots (>1ms average):\n\n")
;;       (dolist (spot hotspots)
;;         (princ (format "%-30s: %.3f ms\n"
;;                       (car spot)
;;                       (* 1000 (cdr spot))))))))

;;; ========================================
;;; 6. プラグインシステムの活用
;;; ========================================

;; 6-1. プラグインの作成

;; (defun my-nskk-create-plugin (name init-func cleanup-func)
;;   "NSKKプラグインを作成"
;;   (let ((plugin (list :name name
;;                      :init init-func
;;                      :cleanup cleanup-func
;;                      :enabled nil)))
;;     (push plugin my-nskk-plugins)
;;     plugin))

;; ;; プラグイン例: タイピング速度測定
;; (defvar my-nskk-typing-plugin
;;   (my-nskk-create-plugin
;;    'typing-speed
;;    ;; 初期化関数
;;    (lambda ()
;;      (setq my-nskk-typing-start-time (current-time))
;;      (setq my-nskk-typing-char-count 0))
;;    ;; クリーンアップ関数
;;    (lambda ()
;;      (message "Typing speed: %.1f chars/min"
;;              (my-nskk-calculate-typing-speed)))))

;; 6-2. プラグインマネージャー

;; (defun my-nskk-enable-plugin (plugin-name)
;;   "プラグインを有効化"
;;   (interactive
;;    (list (intern (completing-read "Plugin: "
;;                                  (mapcar (lambda (p) (plist-get p :name))
;;                                         my-nskk-plugins)))))
;;   (when-let ((plugin (cl-find plugin-name my-nskk-plugins
;;                              :key (lambda (p) (plist-get p :name)))))
;;     (funcall (plist-get plugin :init))
;;     (plist-put plugin :enabled t)
;;     (message "Plugin '%s' enabled" plugin-name)))

;; (defun my-nskk-disable-plugin (plugin-name)
;;   "プラグインを無効化"
;;   (interactive
;;    (list (intern (completing-read "Plugin: "
;;                                  (mapcar (lambda (p) (plist-get p :name))
;;                                         my-nskk-plugins)))))
;;   (when-let ((plugin (cl-find plugin-name my-nskk-plugins
;;                              :key (lambda (p) (plist-get p :name)))))
;;     (when (plist-get plugin :cleanup)
;;       (funcall (plist-get plugin :cleanup)))
;;     (plist-put plugin :enabled nil)
;;     (message "Plugin '%s' disabled" plugin-name)))

;;; ========================================
;;; 7. 外部ツールとの連携
;;; ========================================

;; 7-1. SKKサーバーとの連携

;; (defun my-nskk-setup-server ()
;;   "SKKサーバーとの連携を設定"
;;   (setq nskk-use-dictionary-server t
;;         nskk-dictionary-server-host "localhost"
;;         nskk-dictionary-server-port 1178
;;         nskk-dictionary-server-timeout 5)

;;   ;; サーバー接続のテスト
;;   (when (my-nskk-test-server-connection)
;;     (message "SKK server connected successfully")))

;; (defun my-nskk-test-server-connection ()
;;   "サーバー接続をテスト"
;;   (condition-case err
;;       (progn
;;         (nskk-server-request "あ")
;;         t)
;;     (error
;;      (message "Server connection failed: %s" err)
;;      nil)))

;; 7-2. クリップボード連携

;; (defun my-nskk-paste-and-convert ()
;;   "クリップボードの内容を変換"
;;   (interactive)
;;   (let ((text (current-kill 0)))
;;     (when (string-match-p "[a-z]+" text)
;;       (insert (nskk-convert-romaji text)))))

;; 7-3. 外部辞書APIとの連携

;; (defun my-nskk-lookup-external-api (word)
;;   "外部辞書APIで単語を検索"
;;   (let ((url (format "https://api.example.com/dict?word=%s"
;;                     (url-hexify-string word))))
;;     (with-current-buffer (url-retrieve-synchronously url)
;;       (goto-char (point-min))
;;       (re-search-forward "\n\n")
;;       (json-parse-buffer))))

;;; ========================================
;;; 補足情報
;;; ========================================

;; 参考リソース:
;; - NSKK API リファレンス: docs/reference/api-reference.md
;; - プラグイン開発ガイド: docs/how-to/plugin-development.md
;; - パフォーマンスガイド: docs/explanation/performance-optimization.md

;; サンプルプラグイン:
;; - examples/plugins/ ディレクトリ

;; 質問・バグ報告:
;; - GitHub Issues: https://github.com/takeokunn/nskk.el/issues

(provide 'advanced-usage)

;;; advanced-usage.el ends here
