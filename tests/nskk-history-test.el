;;; nskk-history-test.el --- Tests for nskk-history -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for history management system

;;; Code:

(require 'ert)
(require 'nskk-history)
(require 'nskk-test-framework)

;;; 基本機能テスト

(nskk-deftest nskk-history-record-test
  "履歴記録の基本テスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 履歴記録
  (nskk-record-history '(:midashi "かんじ"
                        :candidate "漢字"
                        :conversion-time 0.001
                        :candidate-index 0
                        :total-candidates 3))

  (should (= (length nskk-history-entries) 1))

  (let ((entry (car nskk-history-entries)))
    (should (equal (nskk-history-entry-midashi entry) "かんじ"))
    (should (equal (nskk-history-entry-candidate entry) "漢字"))
    (should (= (nskk-history-entry-candidate-index entry) 0))
    (should (= (nskk-history-entry-total-candidates entry) 3))))

(nskk-deftest nskk-history-multiple-entries-test
  "複数履歴記録のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 複数履歴を記録
  (dotimes (i 5)
    (nskk-record-history `(:midashi ,(format "test%d" i)
                          :candidate ,(format "候補%d" i))))

  (should (= (length nskk-history-entries) 5))

  ;; 最新のものが先頭にあることを確認
  (should (equal (nskk-history-entry-midashi (car nskk-history-entries)) "test4")))

(nskk-deftest nskk-history-max-entries-test
  "履歴数制限のテスト"
  :tags '(:unit :slow)

  (let ((nskk-history-max-entries 10))
    (setq nskk-history-entries nil)

    ;; 制限を超える履歴を記録
    (dotimes (i 15)
      (nskk-record-history `(:midashi ,(format "test%d" i)
                            :candidate "候補")))

    ;; 制限内に収まっていることを確認
    (should (<= (length nskk-history-entries) nskk-history-max-entries))))

;;; 匿名化テスト

(nskk-deftest nskk-history-anonymize-test
  "匿名化機能のテスト"
  :tags '(:unit)

  (let ((nskk-history-anonymize t))
    (setq nskk-history-entries nil)

    ;; 電話番号を含む履歴
    (nskk-record-history '(:midashi "でんわ"
                          :candidate "090-1234-5678"))

    (let ((entry (car nskk-history-entries)))
      ;; 匿名化されていることを確認
      (should (nskk-history-entry-anonymized entry))
      (should (string-match-p "\\*+" (nskk-history-entry-candidate entry))))))

(nskk-deftest nskk-history-no-anonymize-test
  "匿名化無効時のテスト"
  :tags '(:unit)

  (let ((nskk-history-anonymize nil))
    (setq nskk-history-entries nil)

    (nskk-record-history '(:midashi "test"
                          :candidate "090-1234-5678"))

    (let ((entry (car nskk-history-entries)))
      ;; 匿名化されていないことを確認
      (should-not (nskk-history-entry-anonymized entry))
      (should (equal (nskk-history-entry-candidate entry) "090-1234-5678")))))

;;; 検索機能テスト

(nskk-deftest nskk-history-search-test
  "履歴検索のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; テストデータ作成
  (nskk-record-history '(:midashi "かんじ" :candidate "漢字"))
  (nskk-record-history '(:midashi "かんじ" :candidate "幹事"))
  (nskk-record-history '(:midashi "test" :candidate "テスト"))

  (let ((results (nskk-history-search "かんじ")))
    (should (= (length results) 2))
    (should (cl-every (lambda (e) (equal (nskk-history-entry-midashi e) "かんじ"))
                     results))))

(nskk-deftest nskk-history-recent-test
  "最近の履歴取得のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; テストデータ作成
  (dotimes (i 10)
    (nskk-record-history `(:midashi ,(format "test%d" i)
                          :candidate "候補")))

  (let ((recent (nskk-history-recent 3)))
    (should (= (length recent) 3))
    ;; 最新のものから順に取得されることを確認
    (should (equal (nskk-history-entry-midashi (nth 0 recent)) "test9"))
    (should (equal (nskk-history-entry-midashi (nth 1 recent)) "test8"))
    (should (equal (nskk-history-entry-midashi (nth 2 recent)) "test7"))))

;;; 統計情報テスト

(nskk-deftest nskk-history-statistics-test
  "統計情報のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; テストデータ作成
  (dotimes (i 5)
    (nskk-record-history `(:midashi "test"
                          :candidate ,(format "候補%d" i)
                          :conversion-time 0.001
                          :candidate-index 0
                          :total-candidates 3
                          :okuri-ari ,(= (mod i 2) 0))))

  (let ((stats (nskk-history-statistics)))
    (should (= (plist-get stats :total-entries) 5))
    (should (> (plist-get stats :unique-candidates) 0))
    (should (>= (plist-get stats :first-choice-rate) 0.0))))

(nskk-deftest nskk-history-most-used-words-test
  "最頻出単語のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; テストデータ作成
  (dotimes (_ 5)
    (nskk-record-history '(:midashi "test" :candidate "候補A")))

  (dotimes (_ 2)
    (nskk-record-history '(:midashi "test" :candidate "候補B")))

  (let ((most-used (nskk-history-most-used-words 5)))
    (should (> (length most-used) 0))
    ;; 最も使われた単語が先頭にあることを確認
    (should (equal (car (car most-used)) "候補A"))))

;;; クリーンアップテスト

(nskk-deftest nskk-history-cleanup-test
  "古い履歴の削除テスト"
  :tags '(:unit)

  (let ((nskk-history-retention-days 0)  ; 即座に削除
        (nskk-history-enabled t))
    (setq nskk-history-entries nil)

    ;; 古いエントリを作成（直接pushではなくnskk-record-historyを使う）
    ;; 保持期間0日なので、1日以上前のエントリは削除される
    (let ((old-entry (nskk-history-entry--create
                      :midashi "old"
                      :candidate "古い"
                      :timestamp (time-subtract (current-time) (days-to-time 2)))))
      (push old-entry nskk-history-entries))

    ;; 新しいエントリを作成
    (nskk-record-history '(:midashi "new" :candidate "新しい"))

    ;; 初期状態では2エントリ存在
    (should (= (length nskk-history-entries) 2))

    ;; クリーンアップ実行
    (nskk-history--cleanup-old-entries)

    ;; 古いエントリが削除され、新しいエントリだけが残る
    (should (= (length nskk-history-entries) 1))
    (should (equal (nskk-history-entry-midashi (car nskk-history-entries)) "new"))))

;;; エラー処理テスト

(nskk-deftest nskk-history-invalid-entry-test
  "不正なエントリのエラー処理テスト"
  :tags '(:unit)

  ;; plistでない場合
  (should-error (nskk-record-history "invalid"))

  ;; 必須フィールドがない場合
  (should-error (nskk-record-history '(:midashi "test"))))

;;; 追加テスト: カバレッジ向上

(nskk-deftest nskk-history-candidate-index-test
  "candidate-indexパラメータのテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; candidate-indexを明示的に指定
  (nskk-record-history '(:midashi "かんじ"
                        :candidate "漢字"
                        :candidate-index 2
                        :total-candidates 5))

  (let ((entry (car nskk-history-entries)))
    (should (= (nskk-history-entry-candidate-index entry) 2))
    (should (= (nskk-history-entry-total-candidates entry) 5))))

(nskk-deftest nskk-history-personal-info-detection-test
  "個人情報検出のエッジケーステスト"
  :tags '(:unit)

  ;; 個人情報を含まない通常のテキスト
  (should-not (nskk-history--contains-personal-info-p "普通のテキスト"))

  ;; 電話番号を含む
  (should (nskk-history--contains-personal-info-p "090-1234-5678"))

  ;; メールアドレスを含む
  (should (nskk-history--contains-personal-info-p "test@example.com"))

  ;; 日付を含む
  (should (nskk-history--contains-personal-info-p "2024/01/15")))

(nskk-deftest nskk-history-statistics-anonymized-count-test
  "匿名化カウントの統計テスト"
  :tags '(:unit)

  (let ((nskk-history-anonymize t))
    (setq nskk-history-entries nil)

    ;; 匿名化されるエントリ
    (nskk-record-history '(:midashi "でんわ" :candidate "090-1234-5678"))
    ;; 通常のエントリ
    (nskk-record-history '(:midashi "かんじ" :candidate "漢字"))

    (let ((stats (nskk-history-statistics)))
      (should (= (plist-get stats :anonymized-count) 1)))))

(nskk-deftest nskk-history-statistics-avg-candidate-index-test
  "平均候補インデックスの統計テスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 異なるcandidate-indexのエントリを追加
  (nskk-record-history '(:midashi "test1" :candidate "候補" :candidate-index 0))
  (nskk-record-history '(:midashi "test2" :candidate "候補" :candidate-index 2))
  (nskk-record-history '(:midashi "test3" :candidate "候補" :candidate-index 4))

  (let ((stats (nskk-history-statistics)))
    ;; 平均は (0+2+4)/3 = 2.0
    (should (= (plist-get stats :avg-candidate-index) 2.0))))

(nskk-deftest nskk-history-by-date-end-boundary-test
  "日付フィルタリングの終了境界テスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 現在時刻の前後にエントリを作成
  (let ((now (current-time))
        (past (time-subtract (current-time) (days-to-time 5)))
        (future (time-add (current-time) (days-to-time 5))))

    ;; 過去のエントリ
    (let ((old-entry (nskk-history-entry--create
                      :midashi "old"
                      :candidate "古い"
                      :timestamp past)))
      (push old-entry nskk-history-entries))

    ;; 現在のエントリ
    (nskk-record-history '(:midashi "current" :candidate "現在"))

    ;; 過去から現在までのフィルタ（end-dateの境界確認）
    (let ((results (nskk-history-by-date past now)))
      ;; pastより後でnowより前のエントリが取得されることを確認
      (should (> (length results) 0)))))

(nskk-deftest nskk-history-conversion-time-analysis-values-test
  "変換時間分析の各値のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 異なる変換時間のエントリを追加
  (nskk-record-history '(:midashi "test1" :candidate "候補" :conversion-time 0.001))
  (nskk-record-history '(:midashi "test2" :candidate "候補" :conversion-time 0.005))
  (nskk-record-history '(:midashi "test3" :candidate "候補" :conversion-time 0.003))

  (let ((analysis (nskk-history-conversion-time-analysis)))
    (should (= (plist-get analysis :count) 3))
    (should (= (plist-get analysis :min) 0.001))
    (should (= (plist-get analysis :max) 0.005))
    (should (= (plist-get analysis :avg) 0.003))
    (should (= (plist-get analysis :median) 0.003))))

(nskk-deftest nskk-history-conversion-time-analysis-empty-test
  "変換時間が記録されていない場合の分析テスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; 変換時間なしのエントリ
  (nskk-record-history '(:midashi "test" :candidate "候補"))

  (let ((analysis (nskk-history-conversion-time-analysis)))
    (should (= (plist-get analysis :count) 0))
    (should (= (plist-get analysis :min) 0.0))
    (should (= (plist-get analysis :max) 0.0))
    (should (= (plist-get analysis :avg) 0.0))
    (should (= (plist-get analysis :median) 0.0))))

(nskk-deftest nskk-history-export-anonymized-flag-test
  "エクスポート時の匿名化フラグテスト"
  :tags '(:unit)

  (let ((nskk-history-anonymize t)
        (temp-file (make-temp-file "nskk-history-export-test")))
    (setq nskk-history-entries nil)

    ;; 匿名化されるエントリ
    (nskk-record-history '(:midashi "でんわ" :candidate "090-1234-5678"))

    ;; 通常のエントリ
    (nskk-record-history '(:midashi "かんじ" :candidate "漢字"))

    (nskk-history-export temp-file)

    ;; エクスポートされたファイルを確認
    (with-temp-buffer
      (insert-file-contents temp-file)
      (let ((content (buffer-string)))
        ;; 匿名化フラグ"true"が含まれることを確認
        (should (string-match-p "true" content))
        ;; 匿名化フラグ"false"が含まれることを確認
        (should (string-match-p "false" content))))

    (delete-file temp-file)))

(nskk-deftest nskk-history-statistics-print-test
  "統計情報表示のテスト"
  :tags '(:unit)

  (setq nskk-history-entries nil)

  ;; テストデータ作成
  (nskk-record-history '(:midashi "test1" :candidate "候補1" :candidate-index 0))
  (nskk-record-history '(:midashi "test2" :candidate "候補2" :candidate-index 2))

  ;; message関数の出力をキャプチャ
  (let ((message-output nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-output (apply #'format format-string args)))))
      (nskk-history-print-statistics)
      ;; avg-candidate-indexが出力に含まれることを確認
      (should (string-match-p "Average Candidate Index" message-output)))))

(provide 'nskk-history-test)

;;; nskk-history-test.el ends here
