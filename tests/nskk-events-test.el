;;; nskk-events-test.el --- Tests for nskk-events.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-events.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-events)

;;; テスト用ヘルパー

(defvar nskk-events-test--received-events nil
  "テスト用に受信したイベントのリスト。")

(defun nskk-events-test--reset ()
  "テスト環境をリセットする。"
  (setq nskk-events-test--received-events nil)
  (nskk-events-clear-listeners nil nil)   ; ローカルリスナークリア
  (nskk-events-clear-listeners nil t)     ; グローバルリスナークリア
  (nskk-events-clear-log))

(defun nskk-events-test--handler (event)
  "テスト用イベントハンドラー。
受信したEVENTを `nskk-events-test--received-events' に記録する。"
  (push event nskk-events-test--received-events))

(defmacro nskk-events-test-with-clean-env (&rest body)
  "クリーンなイベント環境で BODY を実行する。"
  `(unwind-protect
       (progn
         (nskk-events-test--reset)
         ,@body)
     (nskk-events-test--reset)))

;;; イベントタイプテスト

(ert-deftest nskk-events-test-event-types ()
  "定義されているイベントタイプを確認する。"
  (should (memq :state-changed nskk-events-types))
  (should (memq :mode-switched nskk-events-types))
  (should (memq :buffer-modified nskk-events-types))
  (should (memq :conversion-started nskk-events-types))
  (should (memq :error nskk-events-types)))

;;; リスナー登録・削除テスト

(ert-deftest nskk-events-test-add-listener ()
  "リスナー登録をテストする。"
  (nskk-events-test-with-clean-env
   (should (nskk-events-add-listener :state-changed #'nskk-events-test--handler))
   (should (nskk-events-has-listener-p :state-changed))))

(ert-deftest nskk-events-test-add-global-listener ()
  "グローバルリスナー登録をテストする。"
  (nskk-events-test-with-clean-env
   (should (nskk-events-add-listener :mode-switched #'nskk-events-test--handler t))
   (should (nskk-events-has-listener-p :mode-switched t))))

(ert-deftest nskk-events-test-add-duplicate-listener ()
  "重複リスナー登録をテストする（重複は無視されるべき）。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (let ((listeners (nskk-events-get-listeners :state-changed)))
     (should (= (length listeners) 1)))))

(ert-deftest nskk-events-test-add-invalid-event-type ()
  "無効なイベントタイプへのリスナー登録をテストする。"
  (nskk-events-test-with-clean-env
   (should-error
    (nskk-events-add-listener :invalid-type #'nskk-events-test--handler))))

(ert-deftest nskk-events-test-remove-listener ()
  "リスナー削除をテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (should (nskk-events-remove-listener :state-changed #'nskk-events-test--handler))
   (should-not (nskk-events-has-listener-p :state-changed))))

(ert-deftest nskk-events-test-remove-nonexistent-listener ()
  "存在しないリスナーの削除をテストする。"
  (nskk-events-test-with-clean-env
   (should-not
    (nskk-events-remove-listener :state-changed #'nskk-events-test--handler))))

(ert-deftest nskk-events-test-clear-listeners ()
  "リスナークリアをテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (nskk-events-add-listener :mode-switched #'nskk-events-test--handler)
   (nskk-events-clear-listeners :state-changed)
   (should-not (nskk-events-has-listener-p :state-changed))
   (should (nskk-events-has-listener-p :mode-switched))))

(ert-deftest nskk-events-test-clear-all-listeners ()
  "全リスナークリアをテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (nskk-events-add-listener :mode-switched #'nskk-events-test--handler)
   (nskk-events-clear-listeners)
   (should-not (nskk-events-has-listener-p :state-changed))
   (should-not (nskk-events-has-listener-p :mode-switched))))

;;; イベント発行テスト

(ert-deftest nskk-events-test-emit-event ()
  "イベント発行をテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (let ((count (nskk-events-emit :state-changed :from 'hiragana :to 'katakana)))
     (should (= count 1))
     (should (= (length nskk-events-test--received-events) 1))
     (let ((event (car nskk-events-test--received-events)))
       (should (eq (nskk-event-type event) :state-changed))
       (should (equal (nskk-event-data event)
                     '(:from hiragana :to katakana)))))))

(ert-deftest nskk-events-test-emit-to-multiple-listeners ()
  "複数リスナーへのイベント発行をテストする。"
  (nskk-events-test-with-clean-env
   (let ((count1 0)
         (count2 0))
     (nskk-events-add-listener :state-changed
                               (lambda (_event) (setq count1 (1+ count1))))
     (nskk-events-add-listener :state-changed
                               (lambda (_event) (setq count2 (1+ count2))))
     (nskk-events-emit :state-changed :test t)
     (should (= count1 1))
     (should (= count2 1)))))

(ert-deftest nskk-events-test-emit-global-and-local ()
  "グローバル・ローカルリスナー両方へのイベント発行をテストする。"
  (nskk-events-test-with-clean-env
   (let ((global-count 0)
         (local-count 0))
     (nskk-events-add-listener :mode-switched
                               (lambda (_event) (setq global-count (1+ global-count)))
                               t)  ; グローバル
     (nskk-events-add-listener :mode-switched
                               (lambda (_event) (setq local-count (1+ local-count)))
                               nil)  ; ローカル
     (nskk-events-emit :mode-switched :from 'hiragana :to 'katakana)
     (should (= global-count 1))
     (should (= local-count 1)))))

(ert-deftest nskk-events-test-emit-no-listeners ()
  "リスナーがない場合のイベント発行をテストする。"
  (nskk-events-test-with-clean-env
   (let ((count (nskk-events-emit :state-changed :test t)))
     (should (= count 0)))))

(ert-deftest nskk-events-test-emit-invalid-event-type ()
  "無効なイベントタイプの発行をテストする。"
  (nskk-events-test-with-clean-env
   (should-error (nskk-events-emit :invalid-type :test t))))

;;; エラーハンドリングテスト

(ert-deftest nskk-events-test-error-in-listener ()
  "リスナー内のエラー処理をテストする。"
  (nskk-events-test-with-clean-env
   (let ((error-count 0))
     (nskk-events-add-listener :state-changed
                               (lambda (_event) (error "Test error")))
     (nskk-events-add-listener :state-changed
                               (lambda (_event) (setq error-count (1+ error-count))))
     ;; エラーが発生しても他のリスナーは実行される
     (nskk-events-emit :state-changed :test t)
     (should (= error-count 1)))))

(ert-deftest nskk-events-test-custom-error-handler ()
  "カスタムエラーハンドラーをテストする。"
  (nskk-events-test-with-clean-env
   (let ((error-handled nil))
     (setq nskk-events-error-handler
           (lambda (_event-type _error-info)
             (setq error-handled t)))
     (unwind-protect
         (progn
           (nskk-events-add-listener :state-changed
                                     (lambda (_event) (error "Test error")))
           (nskk-events-emit :state-changed :test t)
           (should error-handled))
       (setq nskk-events-error-handler nil)))))

;;; イベントデータ構造テスト

(ert-deftest nskk-events-test-event-structure ()
  "イベントデータ構造をテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :buffer-modified #'nskk-events-test--handler)
   (nskk-events-emit :buffer-modified :operation 'insert :text "あ")
   (let ((event (car nskk-events-test--received-events)))
     (should (nskk-event-p event))
     (should (eq (nskk-event-type event) :buffer-modified))
     (should (bufferp (nskk-event-buffer event)))
     (should (listp (nskk-event-timestamp event)))
     (should (plist-get (nskk-event-data event) :operation))
     (should (equal (plist-get (nskk-event-data event) :text) "あ")))))

;;; ロギングテスト

(ert-deftest nskk-events-test-logging ()
  "イベントロギングをテストする。"
  (nskk-events-test-with-clean-env
   (let ((nskk-events-enable-logging t))
     (nskk-events-emit :state-changed :test t)
     (should (> (length nskk-events--log-entries) 0))
     (let ((entry (car nskk-events--log-entries)))
       (should (eq (plist-get entry :event-type) :state-changed))
       (should (eq (plist-get entry :level) 'info))))))

(ert-deftest nskk-events-test-log-max-entries ()
  "ログエントリ上限をテストする。"
  (nskk-events-test-with-clean-env
   (let ((nskk-events-enable-logging t)
         (nskk-events-max-log-entries 5))
     (dotimes (i 10)
       (nskk-events-emit :state-changed :index i))
     (should (<= (length nskk-events--log-entries)
                nskk-events-max-log-entries)))))

(ert-deftest nskk-events-test-clear-log ()
  "ログクリアをテストする。"
  (nskk-events-test-with-clean-env
   (let ((nskk-events-enable-logging t))
     (nskk-events-emit :state-changed :test t)
     (should (> (length nskk-events--log-entries) 0))
     (nskk-events-clear-log)
     (should (= (length nskk-events--log-entries) 0)))))

;;; 統計情報テスト

(ert-deftest nskk-events-test-stats ()
  "統計情報取得をテストする。"
  (nskk-events-test-with-clean-env
   (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
   (nskk-events-add-listener :mode-switched #'nskk-events-test--handler t)
   (let ((stats (nskk-events-stats)))
     (should (= (plist-get stats :local-listeners-count) 1))
     (should (= (plist-get stats :global-listeners-count) 1))
     (should (numberp (plist-get stats :event-types-count))))))

(ert-deftest nskk-events-test-stats-with-logging ()
  "ロギング有効時の統計情報をテストする。"
  (nskk-events-test-with-clean-env
   (let ((nskk-events-enable-logging t))
     (nskk-events-emit :state-changed :test t)
     (nskk-events-emit :mode-switched :test t)
     (let ((stats (nskk-events-stats)))
       (should (= (plist-get stats :log-entries-count) 2))))))

;;; リスナー取得テスト

(ert-deftest nskk-events-test-get-listeners ()
  "リスナー取得をテストする。"
  (nskk-events-test-with-clean-env
   (let ((handler1 (lambda (_event) nil))
         (handler2 (lambda (_event) nil)))
     (nskk-events-add-listener :state-changed handler1)
     (nskk-events-add-listener :state-changed handler2 t)  ; グローバル
     (let ((local-only (nskk-events-get-listeners :state-changed nil))
           (with-global (nskk-events-get-listeners :state-changed t)))
       (should (= (length local-only) 1))
       (should (= (length with-global) 2))))))

;;; 複合シナリオテスト

(ert-deftest nskk-events-test-complex-scenario ()
  "複雑なイベント処理シナリオをテストする。"
  (nskk-events-test-with-clean-env
   (let ((state-changes 0)
         (mode-switches 0)
         (nskk-events-enable-logging t))

     ;; リスナー登録
     (nskk-events-add-listener :state-changed
                               (lambda (_event) (setq state-changes (1+ state-changes))))
     (nskk-events-add-listener :mode-switched
                               (lambda (_event) (setq mode-switches (1+ mode-switches)))
                               t)  ; グローバル

     ;; イベント発行
     (nskk-events-emit :state-changed :from 'idle :to 'active)
     (nskk-events-emit :mode-switched :from 'hiragana :to 'katakana)
     (nskk-events-emit :state-changed :from 'active :to 'idle)

     ;; 検証
     (should (= state-changes 2))
     (should (= mode-switches 1))
     (should (= (length nskk-events--log-entries) 3))

     ;; 統計確認
     (let ((stats (nskk-events-stats)))
       (should (= (plist-get stats :local-listeners-count) 1))
       (should (= (plist-get stats :global-listeners-count) 1))
       (should (= (plist-get stats :log-entries-count) 3))))))

(ert-deftest nskk-events-test-cleanup ()
  "クリーンアップ処理をテストする。"
  (nskk-events-test-with-clean-env
   (let ((nskk-events-enable-logging t))
     (nskk-events-add-listener :state-changed #'nskk-events-test--handler)
     (nskk-events-emit :state-changed :test t)

     ;; クリーンアップ前
     (should (nskk-events-has-listener-p :state-changed))
     (should (> (length nskk-events--log-entries) 0))

     ;; クリーンアップ
     (nskk-events-cleanup)

     ;; クリーンアップ後
     (should-not (nskk-events-has-listener-p :state-changed))
     (should (= (length nskk-events--log-entries) 0)))))

(ert-deftest nskk-events-test-buffer-context ()
  "イベントのバッファコンテキスト情報をテストする。"
  (nskk-events-test-with-clean-env
   (let ((buffer1 (generate-new-buffer " *test1*"))
         (buffer2 (generate-new-buffer " *test2*"))
         (received-buffers nil))
     (unwind-protect
         (progn
           ;; グローバルリスナーを登録（全バッファのイベントを受信）
           (nskk-events-add-listener :state-changed
                                     (lambda (event)
                                       (push (nskk-event-buffer event)
                                             received-buffers))
                                     t)  ; グローバル

           ;; バッファ1でイベント発行
           (with-current-buffer buffer1
             (nskk-events-emit :state-changed :test 1))

           ;; バッファ2でイベント発行
           (with-current-buffer buffer2
             (nskk-events-emit :state-changed :test 2))

           ;; 検証：各イベントが正しいバッファコンテキストを持つ
           (should (= (length received-buffers) 2))
           (should (memq buffer1 received-buffers))
           (should (memq buffer2 received-buffers)))
       (kill-buffer buffer1)
       (kill-buffer buffer2)))))

(provide 'nskk-events-test)

;;; nskk-events-test.el ends here
