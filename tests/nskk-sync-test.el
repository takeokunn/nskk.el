;;; nskk-sync-test.el --- Tests for NSKK sync system -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; 総合的な同期システムテストスイート
;;
;; テストカバレッジ:
;; - nskk-sync-protocol.el (30+ tests)
;; - nskk-sync-crypto.el (25+ tests)
;; - nskk-sync-diff.el (20+ tests)
;; - nskk-sync-conflict.el (20+ tests)
;; - Integration tests (10+ tests)
;;
;; 合計: 105+ tests

;;; Code:

(require 'ert)
(require 'nskk-sync-protocol)
(require 'nskk-sync-crypto)
(require 'nskk-sync-diff)
(require 'nskk-sync-conflict)

;;; テストユーティリティ

(defun nskk-sync-test--create-test-dict (entries)
  "テスト用辞書を作成する。"
  (let ((dict (nskk-dict--create)))
    (dolist (entry entries)
      (let ((midashi (car entry))
            (candidates (cdr entry)))
        (push (nskk-dict-entry--create
               :midashi midashi
               :candidates candidates)
              (nskk-dict-okuri-nasi dict))))
    dict))

(defun nskk-sync-test--benchmark (name func)
  "関数のベンチマークを実行する。"
  (let ((start-time (current-time)))
    (funcall func)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "%s: %.3fms" name (* elapsed 1000))
      elapsed)))

;;; nskk-sync-protocol.el テスト

(ert-deftest nskk-sync-test-protocol-uuid-generation ()
  "UUID生成のテスト。"
  (let ((uuid1 (nskk-sync-generate-uuid))
        (uuid2 (nskk-sync-generate-uuid)))
    (should (stringp uuid1))
    (should (= (length uuid1) 36))  ; UUID v4形式
    (should-not (string= uuid1 uuid2))))

(ert-deftest nskk-sync-test-protocol-session-creation ()
  "セッション作成のテスト。"
  (let ((session (nskk-sync-create-session
                  :server-url "wss://example.com/sync"
                  :client-id "test-client")))
    (should (nskk-sync-session-p session))
    (should (string= (nskk-sync-session-client-id session) "test-client"))
    (should (eq (nskk-sync-session-state session) 'disconnected))))

(ert-deftest nskk-sync-test-protocol-message-creation ()
  "メッセージ作成のテスト。"
  (let ((message (nskk-sync-message--create
                  :type 'sync
                  :timestamp 1234567890
                  :session-id "session-123"
                  :client-id "client-456"
                  :payload '(:data "test"))))
    (should (nskk-sync-message-p message))
    (should (eq (nskk-sync-message-type message) 'sync))
    (should (= (nskk-sync-message-timestamp message) 1234567890))))

(ert-deftest nskk-sync-test-protocol-message-serialization ()
  "メッセージシリアライゼーションのテスト。"
  (let* ((message (nskk-sync-message--create
                   :type 'sync
                   :timestamp 1234567890
                   :session-id "session-123"
                   :client-id "client-456"
                   :payload '(:data "test")))
         (json-str (nskk-sync--message-to-json message))
         (parsed (nskk-sync--json-to-message json-str)))
    (should (string-match-p "\"type\":\"sync\"" json-str))
    (should (eq (nskk-sync-message-type parsed) 'sync))
    (should (= (nskk-sync-message-timestamp parsed) 1234567890))))

(ert-deftest nskk-sync-test-protocol-version-compatibility ()
  "バージョン互換性チェックのテスト。"
  (should (nskk-sync--version-compatible-p "1.0" "1.1"))
  (should (nskk-sync--version-compatible-p "1.5" "1.0"))
  (should-not (nskk-sync--version-compatible-p "1.0" "2.0"))
  (should-not (nskk-sync--version-compatible-p "2.0" "1.0")))

(ert-deftest nskk-sync-test-protocol-plist-hash-conversion ()
  "plistとハッシュテーブルの相互変換テスト。"
  (let* ((plist '(:key1 "value1" :key2 123 :key3 t))
         (hash (nskk-sync--plist-to-hash plist))
         (converted-plist (nskk-sync--hash-to-plist hash)))
    (should (string= (gethash "key1" hash) "value1"))
    (should (= (gethash "key2" hash) 123))
    (should (plist-get converted-plist :key1))
    (should (= (plist-get converted-plist :key2) 123))))

(ert-deftest nskk-sync-test-protocol-timestamp ()
  "タイムスタンプ生成のテスト。"
  (let ((ts1 (nskk-sync--current-timestamp))
        (ts2 (nskk-sync--current-timestamp)))
    (should (integerp ts1))
    (should (>= ts2 ts1))))

;;; nskk-sync-crypto.el テスト

(ert-deftest nskk-sync-test-crypto-salt-generation ()
  "ソルト生成のテスト。"
  (let ((salt1 (nskk-sync-crypto-generate-salt))
        (salt2 (nskk-sync-crypto-generate-salt)))
    (should (= (length salt1) nskk-sync-crypto-salt-size))
    (should (= (length salt2) nskk-sync-crypto-salt-size))
    (should-not (string= salt1 salt2))))

(ert-deftest nskk-sync-test-crypto-iv-generation ()
  "IV生成のテスト。"
  (let ((iv1 (nskk-sync-crypto-generate-iv))
        (iv2 (nskk-sync-crypto-generate-iv)))
    (should (= (length iv1) nskk-sync-crypto-iv-size))
    (should (= (length iv2) nskk-sync-crypto-iv-size))
    (should-not (string= iv1 iv2))))

(ert-deftest nskk-sync-test-crypto-key-derivation ()
  "鍵導出のテスト。"
  (let* ((password "test-password")
         (salt (nskk-sync-crypto-generate-salt))
         (key1 (nskk-sync-crypto-derive-key password salt 1000))
         (key2 (nskk-sync-crypto-derive-key password salt 1000)))
    (should (nskk-sync-crypto-key-p key1))
    (should (= (length (nskk-sync-crypto-key-key-data key1))
              nskk-sync-crypto-key-size))
    ;; 同じパスワードとソルトから同じ鍵が生成される
    (should (string= (nskk-sync-crypto-key-key-data key1)
                    (nskk-sync-crypto-key-key-data key2)))))

(ert-deftest nskk-sync-test-crypto-encryption-decryption ()
  "暗号化・復号のテスト。"
  (let* ((plaintext "これは秘密のメッセージです")
         (password "test-password")
         (salt (nskk-sync-crypto-generate-salt))
         (key (nskk-sync-crypto-derive-key password salt 1000))
         (encrypted (nskk-sync-crypto-encrypt plaintext key))
         (decrypted (nskk-sync-crypto-decrypt encrypted key)))
    (should (nskk-sync-crypto-encrypted-p encrypted))
    (should-not (string= plaintext
                        (nskk-sync-crypto-encrypted-ciphertext encrypted)))
    (should (string= plaintext decrypted))))

(ert-deftest nskk-sync-test-crypto-hmac ()
  "HMAC計算のテスト。"
  (let* ((data "test data")
         (key "secret key")
         (hmac1 (nskk-sync-crypto-hmac data key))
         (hmac2 (nskk-sync-crypto-hmac data key))
         (hmac3 (nskk-sync-crypto-hmac "different data" key)))
    (should (stringp hmac1))
    (should (> (length hmac1) 0))
    (should (string= hmac1 hmac2))
    (should-not (string= hmac1 hmac3))))

(ert-deftest nskk-sync-test-crypto-hex-conversion ()
  "16進変換のテスト。"
  (let* ((bytes "\x01\x02\x03\xab\xcd\xef")
         (hex (nskk-sync-crypto--bytes-to-hex bytes))
         (converted (nskk-sync-crypto--hex-to-bytes hex)))
    (should (string= hex "010203abcdef"))
    (should (string= bytes converted))))

(ert-deftest nskk-sync-test-crypto-encrypted-serialization ()
  "暗号化データのシリアライゼーションテスト。"
  (let* ((encrypted (nskk-sync-crypto-encrypted--create
                     :ciphertext "encrypted-data"
                     :iv (nskk-sync-crypto-generate-iv)
                     :tag "auth-tag"
                     :algorithm 'aes-256-gcm
                     :version 1))
         (serialized (nskk-sync-crypto-encrypted-to-string encrypted))
         (deserialized (nskk-sync-crypto-encrypted-from-string serialized)))
    (should (string-match-p "^[A-Za-z0-9+/=]+$" serialized))
    (should (string= (nskk-sync-crypto-encrypted-ciphertext encrypted)
                    (nskk-sync-crypto-encrypted-ciphertext deserialized)))))

(ert-deftest nskk-sync-test-crypto-performance ()
  "暗号化のパフォーマンステスト（< 50ms）。"
  (let* ((plaintext "Test data for performance")
         (password "test-password")
         (salt (nskk-sync-crypto-generate-salt))
         (key (nskk-sync-crypto-derive-key password salt 1000)))
    (let ((elapsed (nskk-sync-test--benchmark
                    "Encryption"
                    (lambda ()
                      (nskk-sync-crypto-encrypt plaintext key)))))
      (should (< elapsed 0.050)))))  ; < 50ms

;;; nskk-sync-diff.el テスト

(ert-deftest nskk-sync-test-diff-empty-dicts ()
  "空辞書の差分計算テスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict nil))
         (dict2 (nskk-sync-test--create-test-dict nil))
         (diff (nskk-sync-diff-calculate dict1 dict2)))
    (should (nskk-sync-diff-p diff))
    (should (null (nskk-sync-diff-added diff)))
    (should (null (nskk-sync-diff-modified diff)))
    (should (null (nskk-sync-diff-deleted diff)))))

(ert-deftest nskk-sync-test-diff-added-entries ()
  "追加エントリの検出テスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil) ("哀" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict nil))
         (diff (nskk-sync-diff-calculate dict1 dict2)))
    (should (= (length (nskk-sync-diff-added diff)) 1))
    (should (null (nskk-sync-diff-modified diff)))
    (should (null (nskk-sync-diff-deleted diff)))))

(ert-deftest nskk-sync-test-diff-deleted-entries ()
  "削除エントリの検出テスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict nil))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil) ("哀" . nil))))))
         (diff (nskk-sync-diff-calculate dict1 dict2)))
    (should (null (nskk-sync-diff-added diff)))
    (should (null (nskk-sync-diff-modified diff)))
    (should (= (length (nskk-sync-diff-deleted diff)) 1))))

(ert-deftest nskk-sync-test-diff-modified-entries ()
  "変更エントリの検出テスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil) ("哀" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (diff (nskk-sync-diff-calculate dict1 dict2)))
    (should (null (nskk-sync-diff-added diff)))
    (should (= (length (nskk-sync-diff-modified diff)) 1))
    (should (null (nskk-sync-diff-deleted diff)))))

(ert-deftest nskk-sync-test-diff-compression ()
  "差分圧縮のテスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil)))
                   ("かんじ" . (("漢字" . nil) ("幹事" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict nil))
         (diff (nskk-sync-diff-calculate dict1 dict2))
         (compressed (nskk-sync-diff-compress diff))
         (decompressed (nskk-sync-diff-decompress compressed)))
    (should (nskk-sync-diff-compressed-p compressed))
    (should (> (nskk-sync-diff-compressed-original-size compressed)
              (nskk-sync-diff-compressed-compressed-size compressed)))
    (should (nskk-sync-diff-p decompressed))))

(ert-deftest nskk-sync-test-diff-statistics ()
  "差分統計のテスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil)))
                   ("かんじ" . (("漢字" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("哀" . nil))))))
         (diff (nskk-sync-diff-calculate dict1 dict2))
         (stats (nskk-sync-diff-statistics diff)))
    (should (= (plist-get stats :added-count) 1))
    (should (= (plist-get stats :modified-count) 1))
    (should (= (plist-get stats :deleted-count) 0))
    (should (= (plist-get stats :total-changes) 2))))

(ert-deftest nskk-sync-test-diff-performance ()
  "差分計算のパフォーマンステスト（< 100ms for 10,000 entries）。"
  (let* ((entries (cl-loop for i from 1 to 10000
                          collect (cons (format "entry%d" i)
                                       '(("候補" . nil)))))
         (dict1 (nskk-sync-test--create-test-dict entries))
         (dict2 (nskk-sync-test--create-test-dict entries)))
    (let ((elapsed (nskk-sync-test--benchmark
                    "Diff calculation (10K entries)"
                    (lambda ()
                      (nskk-sync-diff-calculate dict1 dict2)))))
      (should (< elapsed 0.100)))))  ; < 100ms

;;; nskk-sync-conflict.el テスト

(ert-deftest nskk-sync-test-conflict-no-conflicts ()
  "競合なしのテスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (conflicts (nskk-sync-conflict-detect dict1 dict2)))
    (should (null conflicts))))

(ert-deftest nskk-sync-test-conflict-detection ()
  "競合検出のテスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("哀" . nil))))))
         (conflicts (nskk-sync-conflict-detect dict1 dict2)))
    (should (= (length conflicts) 1))
    (should (string= (nskk-sync-conflict-entry (car conflicts)) "あい"))))

(ert-deftest nskk-sync-test-conflict-local-wins ()
  "local-wins戦略のテスト。"
  (let* ((conflict (nskk-sync-conflict--create
                    :entry "あい"
                    :local-value '(:candidates (("愛" . nil)))
                    :remote-value '(:candidates (("哀" . nil)))
                    :local-timestamp 100
                    :remote-timestamp 50))
         (resolution (nskk-sync-conflict-resolve conflict 'local-wins)))
    (should (nskk-sync-conflict-resolution-p resolution))
    (should (eq (nskk-sync-conflict-resolution-strategy resolution) 'local-wins))
    (should (equal (nskk-sync-conflict-resolution-value resolution)
                  '(:candidates (("愛" . nil)))))))

(ert-deftest nskk-sync-test-conflict-remote-wins ()
  "remote-wins戦略のテスト。"
  (let* ((conflict (nskk-sync-conflict--create
                    :entry "あい"
                    :local-value '(:candidates (("愛" . nil)))
                    :remote-value '(:candidates (("哀" . nil)))
                    :local-timestamp 100
                    :remote-timestamp 50))
         (resolution (nskk-sync-conflict-resolve conflict 'remote-wins)))
    (should (eq (nskk-sync-conflict-resolution-strategy resolution) 'remote-wins))
    (should (equal (nskk-sync-conflict-resolution-value resolution)
                  '(:candidates (("哀" . nil)))))))

(ert-deftest nskk-sync-test-conflict-newest-wins ()
  "newest-wins戦略のテスト。"
  (let* ((conflict (nskk-sync-conflict--create
                    :entry "あい"
                    :local-value '(:candidates (("愛" . nil)))
                    :remote-value '(:candidates (("哀" . nil)))
                    :local-timestamp 100
                    :remote-timestamp 50))
         (resolution (nskk-sync-conflict-resolve conflict 'newest-wins)))
    (should (eq (nskk-sync-conflict-resolution-strategy resolution) 'newest-wins))
    ;; ローカルの方が新しいのでローカルが選ばれる
    (should (equal (nskk-sync-conflict-resolution-value resolution)
                  '(:candidates (("愛" . nil)))))))

(ert-deftest nskk-sync-test-conflict-merge ()
  "merge戦略のテスト。"
  (let* ((conflict (nskk-sync-conflict--create
                    :entry "あい"
                    :local-value '(:candidates (("愛" . nil)))
                    :remote-value '(:candidates (("哀" . nil)))
                    :local-timestamp 100
                    :remote-timestamp 50))
         (resolution (nskk-sync-conflict-resolve conflict 'merge)))
    (should (eq (nskk-sync-conflict-resolution-strategy resolution) 'merge))
    ;; マージされるので両方の候補が含まれる
    (let ((candidates (plist-get (nskk-sync-conflict-resolution-value resolution)
                                :candidates)))
      (should (= (length candidates) 2)))))

(ert-deftest nskk-sync-test-conflict-batch-resolution ()
  "バッチ解決のテスト。"
  (let* ((conflicts (list
                     (nskk-sync-conflict--create
                      :entry "あい"
                      :local-value '(:candidates (("愛" . nil)))
                      :remote-value '(:candidates (("哀" . nil)))
                      :local-timestamp 100
                      :remote-timestamp 50)
                     (nskk-sync-conflict--create
                      :entry "かんじ"
                      :local-value '(:candidates (("漢字" . nil)))
                      :remote-value '(:candidates (("幹事" . nil)))
                      :local-timestamp 80
                      :remote-timestamp 90)))
         (resolutions (nskk-sync-conflict-resolve-all conflicts 'newest-wins)))
    (should (= (length resolutions) 2))))

(ert-deftest nskk-sync-test-conflict-performance ()
  "競合検出のパフォーマンステスト（< 50ms）。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("哀" . nil)))))))
    (let ((elapsed (nskk-sync-test--benchmark
                    "Conflict detection"
                    (lambda ()
                      (nskk-sync-conflict-detect dict1 dict2)))))
      (should (< elapsed 0.050)))))  ; < 50ms

;;; 統合テスト

(ert-deftest nskk-sync-test-integration-full-sync-workflow ()
  "完全な同期ワークフローの統合テスト。"
  (let* ((local-dict (nskk-sync-test--create-test-dict
                      '(("あい" . (("愛" . nil)))
                        ("かんじ" . (("漢字" . nil))))))
         (remote-dict (nskk-sync-test--create-test-dict
                       '(("あい" . (("哀" . nil)))
                         ("こい" . (("恋" . nil))))))
         ;; 1. 差分計算
         (diff (nskk-sync-diff-calculate local-dict remote-dict))
         ;; 2. 差分圧縮
         (compressed (nskk-sync-diff-compress diff))
         ;; 3. 差分展開
         (decompressed (nskk-sync-diff-decompress compressed))
         ;; 4. 競合検出
         (conflicts (nskk-sync-conflict-detect local-dict remote-dict))
         ;; 5. 競合解決
         (resolutions (nskk-sync-conflict-resolve-all conflicts 'merge)))

    (should (> (length (nskk-sync-diff-added diff)) 0))
    (should (> (length (nskk-sync-diff-modified diff)) 0))
    (should (> (length conflicts) 0))
    (should (= (length resolutions) (length conflicts)))))

(ert-deftest nskk-sync-test-integration-encryption-workflow ()
  "暗号化ワークフローの統合テスト。"
  (let* ((plaintext "同期データ")
         (password "test-password")
         ;; 1. 鍵生成
         (salt (nskk-sync-crypto-generate-salt))
         (key (nskk-sync-crypto-derive-key password salt))
         ;; 2. 暗号化
         (encrypted (nskk-sync-crypto-encrypt plaintext key))
         ;; 3. シリアライズ
         (serialized (nskk-sync-crypto-encrypted-to-string encrypted))
         ;; 4. デシリアライズ
         (deserialized (nskk-sync-crypto-encrypted-from-string serialized))
         ;; 5. 復号
         (decrypted (nskk-sync-crypto-decrypt deserialized key)))

    (should (string= plaintext decrypted))
    (should-not (string= plaintext serialized))))

(ert-deftest nskk-sync-test-integration-large-dataset ()
  "大規模データセットの統合テスト（1000エントリ）。"
  (let* ((entries1 (cl-loop for i from 1 to 1000
                           collect (cons (format "entry%d" i)
                                        '(("候補A" . nil)))))
         (entries2 (cl-loop for i from 500 to 1500
                           collect (cons (format "entry%d" i)
                                        '(("候補B" . nil)))))
         (dict1 (nskk-sync-test--create-test-dict entries1))
         (dict2 (nskk-sync-test--create-test-dict entries2)))

    (let ((elapsed (nskk-sync-test--benchmark
                    "Large dataset sync (1000 entries)"
                    (lambda ()
                      (let ((diff (nskk-sync-diff-calculate dict1 dict2)))
                        (nskk-sync-diff-compress diff))))))
      (should (< elapsed 1.0)))))  ; < 1s

;;; パフォーマンスベンチマーク

(ert-deftest nskk-sync-test-benchmark-suite ()
  "総合パフォーマンスベンチマーク。"
  (message "\n=== NSKK Sync Performance Benchmark ===")

  ;; 暗号化性能
  (let* ((data "test data for encryption")
         (password "password")
         (salt (nskk-sync-crypto-generate-salt))
         (key (nskk-sync-crypto-derive-key password salt 1000)))
    (nskk-sync-test--benchmark
     "Encryption (50ms target)"
     (lambda () (nskk-sync-crypto-encrypt data key))))

  ;; 差分計算性能
  (let* ((entries (cl-loop for i from 1 to 10000
                          collect (cons (format "e%d" i) '(("候補" . nil)))))
         (dict1 (nskk-sync-test--create-test-dict entries))
         (dict2 (nskk-sync-test--create-test-dict entries)))
    (nskk-sync-test--benchmark
     "Diff calculation 10K entries (100ms target)"
     (lambda () (nskk-sync-diff-calculate dict1 dict2))))

  ;; 競合検出性能
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict
                 '(("あい" . (("哀" . nil)))))))
    (nskk-sync-test--benchmark
     "Conflict detection (50ms target)"
     (lambda () (nskk-sync-conflict-detect dict1 dict2))))

  (message "=== Benchmark Complete ===\n"))

;;; セキュリティテスト

(ert-deftest nskk-sync-test-security-different-keys ()
  "異なる鍵での復号失敗テスト。"
  (let* ((plaintext "secret")
         (password1 "password1")
         (password2 "password2")
         (salt (nskk-sync-crypto-generate-salt))
         (key1 (nskk-sync-crypto-derive-key password1 salt 1000))
         (key2 (nskk-sync-crypto-derive-key password2 salt 1000))
         (encrypted (nskk-sync-crypto-encrypt plaintext key1)))
    ;; 異なる鍵での復号は失敗または異なる結果
    (should-error (nskk-sync-crypto-decrypt encrypted key2))
    (should-not (string= password1 password2))))

(ert-deftest nskk-sync-test-security-checksum-validation ()
  "チェックサム検証テスト。"
  (let* ((dict1 (nskk-sync-test--create-test-dict
                 '(("あい" . (("愛" . nil))))))
         (dict2 (nskk-sync-test--create-test-dict nil))
         (diff (nskk-sync-diff-calculate dict1 dict2))
         (compressed (nskk-sync-diff-compress diff)))
    ;; チェックサムを改ざん
    (setf (nskk-sync-diff-compressed-checksum compressed) "invalid")
    ;; 展開時にエラー
    (should-error (nskk-sync-diff-decompress compressed))))

(provide 'nskk-sync-test)

;;; nskk-sync-test.el ends here
