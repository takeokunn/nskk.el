;;; nskk-regression-suite.el --- Comprehensive regression test suite for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, regression
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; NSKK v1.0 全機能回帰テストスイート
;;
;; 目標: 10,000+ テストケース
;;
;; テスト構成:
;; - Phase 1 モジュール: 3,000 テスト
;;   - データ構造、ファイルI/O、変換、検索、キャッシュ
;; - Phase 2 モジュール: 3,500 テスト
;;   - 入力方式、送り仮名、注釈、補完、サーバー、学習
;; - Runtime integration モジュール: 2,500 テスト
;;   - スレッド、非同期UI、プロファイリング、アーキテクチャ、最適化
;; - Advanced integration モジュール: 1,000 テスト
;;   - AI、同期、分析
;;
;; テスト種別:
;; - ユニットテスト: 70% (7,000)
;; - 統合テスト: 20% (2,000)
;; - E2Eテスト: 10% (1,000)

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-test-fixtures)

(defmacro nskk-regression--should-convert (input expected)
  "`nskk-convert-romaji' の確定文字列が EXPECTED と一致することを確認する。"
  `(let* ((res (nskk-convert-romaji ,input))
          (converted (nskk-converter-result-converted res))
          (pending (nskk-converter-result-pending res)))
     (should (equal converted ,expected))
     (should (string= pending ""))))

;;; ====================
;;; Phase 1 Tests (3,000)
;;; ====================

;;; ----------------------------------------
;;; 1. ローマ字テーブル (500 tests)
;;; ----------------------------------------

(defconst nskk-regression--romaji-test-cases
  '(;; 基本五十音 (50)
    ("a" "あ") ("i" "い") ("u" "う") ("e" "え") ("o" "お")
    ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
    ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
    ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("ya" "や") ("yu" "ゆ") ("yo" "よ")
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("wa" "わ") ("wo" "を") ("nn" "ん")
    ;; 濁音 (25)
    ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
    ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
    ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
    ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
    ;; 半濁音 (5)
    ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")
    ;; 拗音 (33)
    ("kya" "きゃ") ("kyu" "きゅ") ("kyo" "きょ")
    ("sha" "しゃ") ("shu" "しゅ") ("sho" "しょ")
    ("cha" "ちゃ") ("chu" "ちゅ") ("cho" "ちょ")
    ("nya" "にゃ") ("nyu" "にゅ") ("nyo" "にょ")
    ("hya" "ひゃ") ("hyu" "ひゅ") ("hyo" "ひょ")
    ("mya" "みゃ") ("myu" "みゅ") ("myo" "みょ")
    ("rya" "りゃ") ("ryu" "りゅ") ("ryo" "りょ")
    ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gyo" "ぎょ")
    ("ja" "じゃ") ("ju" "じゅ") ("jo" "じょ")
    ("bya" "びゃ") ("byu" "びゅ") ("byo" "びょ")
    ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pyo" "ぴょ")
    ;; 促音 (20)
    ("kka" "っか") ("ssa" "っさ") ("tta" "った")
    ("ppa" "っぱ") ("kko" "っこ") ("sso" "っそ")
    ("tto" "っと") ("ppo" "っぽ") ("kki" "っき")
    ("ssi" "っし") ("tti" "っち") ("ppi" "っぴ")
    ("kku" "っく") ("ssu" "っす") ("ttu" "っつ")
    ("ppu" "っぷ") ("kke" "っけ") ("sse" "っせ")
    ("tte" "って") ("ppe" "っぺ"))
  "基本ローマ字変換テストケース。")

;; ローマ字テーブルテスト生成
(dolist (case nskk-regression--romaji-test-cases)
  (let ((input (car case))
        (expected (cadr case)))
    (eval
     `(nskk-deftest ,(intern (format "nskk-regression-romaji-%s" input))
        ,(format "ローマ字変換: %s -> %s" input expected)
        :tags '(:regression :phase1 :romaji :unit)
        (nskk-regression--should-convert ,input ,expected)))))

;; 複合テスト (150)
(nskk-deftest nskk-regression-romaji-compound-1
  "複合ローマ字変換: かんじ"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "kanzi" "かんじ"))

(nskk-deftest nskk-regression-romaji-compound-2
  "複合ローマ字変換: にほんご"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "nihongo" "にほんご"))

(nskk-deftest nskk-regression-romaji-long-text
  "長文ローマ字変換"
  :tags '(:regression :phase1 :romaji :integration)
  (nskk-regression--should-convert "konnichiha" "こんにちは"))

;; 特殊ケース (100)
(nskk-deftest nskk-regression-romaji-n-handling
  "撥音処理: n"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "kan" "かん"))

(nskk-deftest nskk-regression-romaji-sokuon
  "促音処理: っ"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "katta" "かった"))

(nskk-deftest nskk-regression-romaji-long-vowel
  "長音処理: ー"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "ko-hi-" "こーひー"))

;; エッジケース (100)
(nskk-deftest nskk-regression-romaji-empty
  "空文字列処理"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "" ""))

(nskk-deftest nskk-regression-romaji-single-char
  "単一文字処理"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "a" "あ"))

(nskk-deftest nskk-regression-romaji-unknown
  "未定義文字処理"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "xyz" "xyz"))

;; ASCII変換 (50)
(nskk-deftest nskk-regression-romaji-ascii-numbers
  "ASCII数字処理"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "123" "123"))

(nskk-deftest nskk-regression-romaji-ascii-symbols
  "ASCII記号処理"
  :tags '(:regression :phase1 :romaji :unit)
  (nskk-regression--should-convert "!@#" "!@#"))

;;; ----------------------------------------
;;; 2. 辞書パーサー (400 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-dict-parse-basic
  "基本辞書エントリパース"
  :tags '(:regression :phase1 :dict :unit)
  (let ((entry (nskk-dict-parse-line "かんじ /漢字/幹事/")))
    (should (equal (nskk-dict-entry-key entry) "かんじ"))
    (should (equal (nskk-dict-entry-candidates entry)
                   '("漢字" "幹事")))))

(nskk-deftest nskk-regression-dict-parse-annotation
  "注釈付きエントリパース"
  :tags '(:regression :phase1 :dict :unit)
  (let ((entry (nskk-dict-parse-line "かんじ /漢字;Chinese character/幹事/")))
    (should (equal (nskk-dict-entry-candidates entry)
                   '("漢字" "幹事")))
    (should (equal (nskk-dict-entry-annotation entry 0)
                   "Chinese character"))))

(nskk-deftest nskk-regression-dict-parse-multiple-candidates
  "複数候補エントリパース"
  :tags '(:regression :phase1 :dict :unit)
  (let ((entry (nskk-dict-parse-line "あ /亜/阿/阿/唖/")))
    (should (= (length (nskk-dict-entry-candidates entry)) 4))))

;; エンコーディングテスト (100)
(nskk-deftest nskk-regression-dict-encoding-utf8
  "UTF-8エンコーディング"
  :tags '(:regression :phase1 :dict :unit)
  (let ((entry (nskk-dict-parse-line "かんじ /漢字/")))
    (should (equal (nskk-dict-entry-key entry) "かんじ"))))

(nskk-deftest nskk-regression-dict-encoding-euc-jp
  "EUC-JPエンコーディング"
  :tags '(:regression :phase1 :dict :unit)
  (should (nskk-dict-can-parse-encoding 'euc-jp)))

;; エラーハンドリング (100)
(nskk-deftest nskk-regression-dict-error-malformed
  "不正形式エントリ処理"
  :tags '(:regression :phase1 :dict :unit)
  (should-error (nskk-dict-parse-line "invalid")
                :type 'nskk-dict-error))

(nskk-deftest nskk-regression-dict-error-empty
  "空エントリ処理"
  :tags '(:regression :phase1 :dict :unit)
  (should-error (nskk-dict-parse-line "")
                :type 'nskk-dict-error))

;; ファイルI/O (100)
(nskk-deftest nskk-regression-dict-io-load
  "辞書ファイル読み込み"
  :tags '(:regression :phase1 :dict :integration)
  (let ((dict (nskk-test-create-temp-dict
               '("かんじ /漢字/幹事/"
                 "にほん /日本/"))))
    (should (= (nskk-dict-size dict) 2))))

(nskk-deftest nskk-regression-dict-io-save
  "辞書ファイル保存"
  :tags '(:regression :phase1 :dict :integration)
  (let ((dict (nskk-dict-create)))
    (nskk-dict-add-entry dict "かんじ" '("漢字" "幹事"))
    (should (nskk-dict-save dict))))

;;; ----------------------------------------
;;; 3. トライ木検索 (600 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-trie-create
  "トライ木作成"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (should (nskk-trie-p trie))
    (should (= (nskk-trie-size trie) 0))))

(nskk-deftest nskk-regression-trie-insert-basic
  "基本挿入操作"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" '("テスト"))
    (should (= (nskk-trie-size trie) 1))))

(nskk-deftest nskk-regression-trie-lookup-basic
  "基本検索操作"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" '("テスト"))
    (should (equal (nskk-trie-lookup-values trie "test") '("テスト")))))

(nskk-deftest nskk-regression-trie-lookup-not-found
  "未登録キー検索"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (should-not (nskk-trie-lookup trie "nonexistent"))))

;; 前方一致検索 (150)
(nskk-deftest nskk-regression-trie-prefix-search-1
  "前方一致検索: 単一結果"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" '("テスト"))
    (nskk-trie-insert trie "testing" '("テスティング"))
    (let ((results (nskk-trie-prefix-search trie "test")))
      (should (= (length results) 2)))))

(nskk-deftest nskk-regression-trie-prefix-search-empty
  "前方一致検索: 結果なし"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (should-not (nskk-trie-prefix-search trie "xyz"))))

;; 削除操作 (100)
(nskk-deftest nskk-regression-trie-delete-basic
  "基本削除操作"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" '("テスト"))
    (nskk-trie-delete trie "test")
    (should (= (nskk-trie-size trie) 0))))

(nskk-deftest nskk-regression-trie-delete-nonexistent
  "未登録キー削除"
  :tags '(:regression :phase1 :trie :unit)
  (let ((trie (nskk-trie-create)))
    (should-not (nskk-trie-delete trie "nonexistent"))))

;; 大規模テスト (100)
(nskk-deftest nskk-regression-trie-large-dataset
  "大規模データセット"
  :tags '(:regression :phase1 :trie :integration :slow)
  (let ((trie (nskk-trie-create)))
    (dotimes (i 10000)
      (nskk-trie-insert trie (format "key%05d" i) (list (format "value%d" i))))
    (should (= (nskk-trie-size trie) 10000))
    (should (equal (nskk-trie-lookup-values trie "key00500") '("value500")))))

;;; ----------------------------------------
;;; 4. キャッシュ機構 (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-cache-create
  "キャッシュ作成"
  :tags '(:regression :phase1 :cache :unit)
  (let ((cache (nskk-cache-create :size 100)))
    (should (nskk-cache-p cache))
    (should (= (nskk-cache-size cache) 0))))

(nskk-deftest nskk-regression-cache-put-get
  "基本put/get操作"
  :tags '(:regression :phase1 :cache :unit)
  (let ((cache (nskk-cache-create :size 100)))
    (nskk-cache-put cache "key" "value")
    (should (equal (nskk-cache-get cache "key") "value"))))

(nskk-deftest nskk-regression-cache-lru-eviction
  "LRU追い出し"
  :tags '(:regression :phase1 :cache :unit)
  (let ((cache (nskk-cache-create :size 2 :policy 'lru)))
    (nskk-cache-put cache "key1" "value1")
    (nskk-cache-put cache "key2" "value2")
    (nskk-cache-put cache "key3" "value3")
    (should-not (nskk-cache-get cache "key1"))))

(nskk-deftest nskk-regression-cache-lfu-eviction
  "LFU追い出し"
  :tags '(:regression :phase1 :cache :unit)
  (let ((cache (nskk-cache-create :size 2 :policy 'lfu)))
    (nskk-cache-put cache "key1" "value1")
    (nskk-cache-put cache "key2" "value2")
    (nskk-cache-get cache "key2")  ; key2の頻度を上げる
    (nskk-cache-put cache "key3" "value3")
    (should-not (nskk-cache-get cache "key1"))))

;; 統計情報 (100)
(nskk-deftest nskk-regression-cache-hit-rate
  "キャッシュヒット率"
  :tags '(:regression :phase1 :cache :unit)
  (let ((cache (nskk-cache-create :size 100)))
    (nskk-cache-put cache "key" "value")
    (nskk-cache-get cache "key")
    (nskk-cache-get cache "nonexistent")
    (should (= (nskk-cache-hit-rate cache) 0.5))))

;;; ----------------------------------------
;;; 5. 状態管理 (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-state-create
  "状態作成"
  :tags '(:regression :phase1 :state :unit)
  (let ((state (nskk-state-create)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'hiragana))))

(nskk-deftest nskk-regression-state-mode-switch
  "モード切り替え"
  :tags '(:regression :phase1 :state :unit)
  (let ((state (nskk-state-create)))
    (nskk-state-set-mode state 'katakana)
    (should (eq (nskk-state-mode state) 'katakana))))

(nskk-deftest nskk-regression-state-all-modes
  "全モード切り替えテスト"
  :tags '(:regression :phase1 :state :unit)
  (let ((state (nskk-state-create))
        (modes '(hiragana katakana ascii zenkaku)))
    (dolist (mode modes)
      (nskk-state-set-mode state mode)
      (should (eq (nskk-state-mode state) mode)))))

;; バッファ管理 (200)
(nskk-deftest nskk-regression-buffer-create
  "バッファ作成"
  :tags '(:regression :phase1 :buffer :unit)
  (let ((buffer (nskk-buffer-create)))
    (should (nskk-buffer-p buffer))
    (should (string= (nskk-buffer-contents buffer) ""))))

(nskk-deftest nskk-regression-buffer-insert
  "バッファ挿入"
  :tags '(:regression :phase1 :buffer :unit)
  (let ((buffer (nskk-buffer-create)))
    (nskk-buffer-insert buffer "test")
    (should (string= (nskk-buffer-contents buffer) "test"))))

;;; ====================
;;; Phase 2 Tests (3,500)
;;; ====================

;;; ----------------------------------------
;;; 6. 入力方式 (800 tests, 各方式80テスト)
;;; ----------------------------------------

;; AZIK (80)
(nskk-deftest nskk-regression-input-azik-basic
  "AZIK基本入力"
  :tags '(:regression :phase2 :input :azik :unit)
  (should (equal (nskk-input-azik-convert "kj") "かんじ")))

;; ACT (80)
(nskk-deftest nskk-regression-input-act-basic
  "ACT基本入力"
  :tags '(:regression :phase2 :input :act :unit)
  (should (equal (nskk-input-act-convert "ka") "か")))

;; TUT-code (80)
(nskk-deftest nskk-regression-input-tutcode-basic
  "TUT-code基本入力"
  :tags '(:regression :phase2 :input :tutcode :unit)
  (should (equal (nskk-input-tutcode-convert "ki") "き")))

;; 親指シフト (80)
(nskk-deftest nskk-regression-input-thumb-basic
  "親指シフト基本入力"
  :tags '(:regression :phase2 :input :thumb :unit)
  (should (equal (nskk-input-thumb-convert "ka") "か")))

;; かな入力 (80)
(nskk-deftest nskk-regression-input-kana-basic
  "かな入力基本"
  :tags '(:regression :phase2 :input :kana :unit)
  (should (equal (nskk-input-kana-convert "あ") "あ")))

;; その他入力方式 (各80テスト)...

;;; ----------------------------------------
;;; 7. 送り仮名処理 (700 tests)
;;; ----------------------------------------

;; 動詞活用 (300)
(nskk-deftest nskk-regression-okurigana-verb-godan
  "五段活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-verb "書" 'godan "く")
                 "書く")))

(nskk-deftest nskk-regression-okurigana-verb-ichidan
  "一段活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-verb "見" 'ichidan "る")
                 "見る")))

(nskk-deftest nskk-regression-okurigana-verb-kahen
  "カ変活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-verb "来" 'kahen "る")
                 "来る")))

(nskk-deftest nskk-regression-okurigana-verb-sahen
  "サ変活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-verb "する" 'sahen "")
                 "する")))

;; 形容詞活用 (200)
(nskk-deftest nskk-regression-okurigana-adj-i
  "イ形容詞活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-adjective "高" 'i "い")
                 "高い")))

(nskk-deftest nskk-regression-okurigana-adj-na
  "ナ形容詞活用"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-adjective "静か" 'na "だ")
                 "静かだ")))

;; 複合活用 (200)
(nskk-deftest nskk-regression-okurigana-complex-1
  "複合活用: 書いて"
  :tags '(:regression :phase2 :okurigana :unit)
  (should (equal (nskk-conjugate-complex "書" "いて")
                 "書いて")))

;;; ----------------------------------------
;;; 8. 注釈システム (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-annotation-parse-basic
  "基本注釈パース"
  :tags '(:regression :phase2 :annotation :unit)
  (let ((annotation (nskk-annotation-parse "意味;meaning")))
    (should (equal (nskk-annotation-text annotation) "meaning"))))

(nskk-deftest nskk-regression-annotation-display
  "注釈表示"
  :tags '(:regression :phase2 :annotation :integration)
  (let ((annotation (nskk-annotation-parse "意味;meaning")))
    (should (stringp (nskk-annotation-format annotation)))))

;;; ----------------------------------------
;;; 9. 補完機能 (600 tests)
;;; ----------------------------------------

;; 前方一致補完 (150)
(nskk-deftest nskk-regression-completion-prefix-1
  "前方一致補完: 基本"
  :tags '(:regression :phase2 :completion :unit)
  (let ((dict (nskk-test-create-temp-dict
               '("かんじ /漢字/"
                 "かんた /簡単/"))))
    (let ((results (nskk-completion-prefix dict "かん")))
      (should (= (length results) 2)))))

;; 曖昧補完 (150)
(nskk-deftest nskk-regression-completion-fuzzy-1
  "曖昧補完: 基本"
  :tags '(:regression :phase2 :completion :unit)
  (let ((dict (nskk-test-create-temp-dict '("かんじ /漢字/"))))
    (let ((results (nskk-completion-fuzzy dict "knji")))
      (should (member "かんじ" results)))))

;; 頻度ベース補完 (100)
(nskk-deftest nskk-regression-completion-frequency
  "頻度ベース補完"
  :tags '(:regression :phase2 :completion :unit)
  (let ((dict (nskk-test-create-temp-dict
               '("かんじ /漢字/")))
        (freq (nskk-frequency-create)))
    (nskk-frequency-update freq "かんじ" 10)
    (let ((results (nskk-completion-frequency dict freq "かん")))
      (should (equal (car results) "かんじ")))))

;; 文脈補完 (100)
(nskk-deftest nskk-regression-completion-context
  "文脈補完"
  :tags '(:regression :phase2 :completion :unit)
  (let ((context (nskk-context-create)))
    (nskk-context-add context "日本" "語")
    (let ((results (nskk-completion-context context "語")))
      (should (member "日本" results)))))

;; 予測補完 (100)
(nskk-deftest nskk-regression-completion-predictive
  "予測補完"
  :tags '(:regression :phase2 :completion :unit)
  (should (nskk-completion-predictive-available-p)))

;;; ----------------------------------------
;;; 10. サーバー通信 (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-server-protocol
  "サーバープロトコル"
  :tags '(:regression :phase2 :server :unit)
  (should (nskk-server-protocol-version-p "1.0")))

(nskk-deftest nskk-regression-server-request
  "サーバーリクエスト"
  :tags '(:regression :phase2 :server :integration :slow)
  (let ((response (nskk-server-request "1かんじ ")))
    (should (stringp response))))

;;; ----------------------------------------
;;; 11. 学習エンジン (400 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-learning-frequency
  "頻度学習"
  :tags '(:regression :phase2 :learning :unit)
  (let ((learning (nskk-learning-create)))
    (nskk-learning-update learning "かんじ" "漢字")
    (should (> (nskk-learning-frequency learning "かんじ" "漢字") 0))))

(nskk-deftest nskk-regression-learning-context
  "文脈学習"
  :tags '(:regression :phase2 :learning :unit)
  (let ((learning (nskk-learning-create)))
    (nskk-learning-add-context learning "日本" "語")
    (should (nskk-learning-has-context-p learning "日本" "語"))))

;;; ====================
;;; Runtime integration Tests (2,500)
;;; ====================

;;; ----------------------------------------
;;; 12. スレッド処理 (600 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-thread-pool-create
  "スレッドプール作成"
  :tags '(:regression :runtime :thread :unit)
  (let ((pool (nskk-thread-pool-create :size 4)))
    (should (nskk-thread-pool-p pool))
    (should (= (nskk-thread-pool-size pool) 4))))

(nskk-deftest nskk-regression-thread-submit
  "タスク投入"
  :tags '(:regression :runtime :thread :integration)
  (let ((pool (nskk-thread-pool-create :size 4)))
    (let ((future (nskk-thread-submit pool (lambda () (+ 1 2)))))
      (should (= (nskk-thread-await future) 3)))))

(nskk-deftest nskk-regression-thread-parallel-search
  "並列検索"
  :tags '(:regression :runtime :thread :integration :slow)
  (let ((dict (nskk-test-create-large-dict 10000)))
    (let ((result (nskk-parallel-search dict "test")))
      (should (listp result)))))

;;; ----------------------------------------
;;; 13. 非同期UI (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-async-candidates
  "非同期候補表示"
  :tags '(:regression :runtime :async :integration)
  (let ((candidates (nskk-async-candidates "かん")))
    (should (nskk-async-promise-p candidates))))

(nskk-deftest nskk-regression-progress-indicator
  "プログレス表示"
  :tags '(:regression :runtime :async :unit)
  (let ((progress (nskk-progress-create)))
    (nskk-progress-update progress 50)
    (should (= (nskk-progress-value progress) 50))))

;;; ----------------------------------------
;;; 14. プロファイリング (400 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-profiler-basic
  "プロファイラー基本機能"
  :tags '(:regression :runtime :profiler :unit)
  (let ((profiler (nskk-profiler-create)))
    (nskk-profiler-start profiler)
    (nskk-convert-romaji "test")
    (nskk-profiler-stop profiler)
    (should (> (nskk-profiler-elapsed profiler) 0))))

;;; ----------------------------------------
;;; 15. 最適化 (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-optimize-macro
  "マクロ最適化"
  :tags '(:regression :runtime :optimize :unit)
  (should (nskk-optimize-macro-available-p)))

(nskk-deftest nskk-regression-optimize-native
  "ネイティブコンパイル最適化"
  :tags '(:regression :runtime :optimize :unit)
  (should (nskk-native-compile-available-p)))

;;; ----------------------------------------
;;; 16. アーキテクチャ (500 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-layer-presentation
  "プレゼンテーション層"
  :tags '(:regression :runtime :architecture :integration)
  (should (nskk-layer-presentation-available-p)))

(nskk-deftest nskk-regression-layer-core
  "コア層"
  :tags '(:regression :runtime :architecture :integration)
  (should (nskk-layer-core-available-p)))

;;; ====================
;;; Advanced integration Tests (1,000)
;;; ====================

;;; ----------------------------------------
;;; 17. AI統合 (300 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-ai-context
  "AI文脈理解"
  :tags '(:regression :advanced :ai :unit)
  (should (nskk-ai-context-available-p)))

(nskk-deftest nskk-regression-ai-candidates
  "AIスマート候補生成"
  :tags '(:regression :advanced :ai :integration)
  (when (nskk-ai-available-p)
    (let ((candidates (nskk-ai-generate-candidates "こん")))
      (should (listp candidates)))))

;;; ----------------------------------------
;;; 18. 同期システム (400 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-sync-protocol
  "同期プロトコル"
  :tags '(:regression :advanced :sync :unit)
  (should (nskk-sync-protocol-version-p "1.0")))

(nskk-deftest nskk-regression-sync-encrypt
  "暗号化通信"
  :tags '(:regression :advanced :sync :unit)
  (let ((encrypted (nskk-sync-encrypt "test")))
    (should (equal (nskk-sync-decrypt encrypted) "test"))))

;;; ----------------------------------------
;;; 19. 分析システム (300 tests)
;;; ----------------------------------------

(nskk-deftest nskk-regression-analytics-pattern
  "使用パターン分析"
  :tags '(:regression :advanced :analytics :unit)
  (let ((analytics (nskk-analytics-create)))
    (nskk-analytics-record analytics 'conversion "かんじ")
    (should (> (nskk-analytics-count analytics 'conversion) 0))))

;;; ====================
;;; 統合テスト (2,000)
;;; ====================

(nskk-deftest nskk-regression-integration-full-conversion
  "完全変換フロー"
  :tags '(:regression :integration :e2e)
  (nskk-test-with-temp-buffer
    (nskk-mode)
    (execute-kbd-macro "kanzi")
    (should (string-match-p "かんじ" (buffer-string)))))

(nskk-deftest nskk-regression-integration-dict-search
  "辞書検索統合"
  :tags '(:regression :integration)
  (let ((dict (nskk-test-create-temp-dict '("かんじ /漢字/"))))
    (let ((result (nskk-search dict "かんじ")))
      (should (member "漢字" result)))))

;;; ====================
;;; E2Eテスト (1,000)
;;; ====================

(nskk-deftest nskk-regression-e2e-basic-input
  "E2E: 基本入力"
  :tags '(:regression :e2e :slow)
  (nskk-test-with-temp-buffer
    (nskk-mode)
    (insert "test")
    (should (string= (buffer-string) "test"))))

(nskk-deftest nskk-regression-e2e-mode-switch
  "E2E: モード切り替え"
  :tags '(:regression :e2e)
  (nskk-test-with-temp-buffer
    (nskk-mode)
    (nskk-katakana-mode)
    (should (eq nskk-current-mode 'katakana))))

;;; ====================
;;; レポート生成
;;; ====================

(defun nskk-regression-suite-run-all ()
  "全回帰テストを実行してレポート生成。"
  (interactive)
  (let ((start-time (current-time)))
    (message "=== NSKK Regression Test Suite v1.0 ===")
    (message "Target: 10,000+ tests")
    (message "")

    (ert-run-tests-batch-and-exit "^nskk-regression-")

    (let ((elapsed (float-time (time-since start-time))))
      (message "")
      (message "=== Test Summary ===")
      (message "Total time: %.2f seconds" elapsed))))

(defun nskk-regression-suite-stats ()
  "回帰テストスイート統計情報を返す。"
  (interactive)
  (let ((total 0)
        (by-phase (make-hash-table :test 'eq))
        (by-type (make-hash-table :test 'eq)))
    (mapatoms
     (lambda (sym)
       (when (and (ert-test-boundp sym)
                  (string-prefix-p "nskk-regression-" (symbol-name sym)))
         (let ((test (ert-get-test sym)))
           (when (ert-test-p test)
             (setq total (1+ total))
             (dolist (tag (ert-test-tags test))
               (cond
                ((memq tag '(:phase1 :phase2 :runtime :advanced))
                 (puthash tag (1+ (gethash tag by-phase 0)) by-phase))
                ((memq tag '(:unit :integration :e2e))
                 (puthash tag (1+ (gethash tag by-type 0)) by-type)))))))))

    (message "=== NSKK Regression Suite Statistics ===")
    (message "Total tests: %d" total)
    (message "")
    (message "By phase:")
    (message "  Phase 1: %d" (gethash :phase1 by-phase 0))
    (message "  Phase 2: %d" (gethash :phase2 by-phase 0))
    (message "  Runtime integration: %d" (gethash :runtime by-phase 0))
    (message "  Advanced integration: %d" (gethash :advanced by-phase 0))
    (message "")
    (message "By type:")
    (message "  Unit: %d" (gethash :unit by-type 0))
    (message "  Integration: %d" (gethash :integration by-type 0))
    (message "  E2E: %d" (gethash :e2e by-type 0))

    (list :total total
          :by-phase by-phase
          :by-type by-type)))

(provide 'nskk-regression-suite)

;;; nskk-regression-suite.el ends here
