;;; nskk-input-azik-test.el --- Tests for AZIK input method -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, azik, test

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; AZIK入力方式のテストスイート。
;;
;; テスト対象:
;; - 基本的なAZIK拡張入力
;; - 標準ローマ字へのフォールバック
;; - 候補取得機能
;; - ハッシュテーブル初期化
;; - 統計情報取得
;; - パフォーマンス要件

;;; Code:

(require 'ert)
(require 'nskk-input-azik)

;;; 基本的なAZIK拡張入力テスト

(ert-deftest nskk-input-azik-test-youon-k ()
  "AZIK拡張: きゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "kj") "きゃ"))
  (should (equal (nskk-input-azik-lookup "kl") "きゅ"))
  (should (equal (nskk-input-azik-lookup "ko") "きょ")))

(ert-deftest nskk-input-azik-test-youon-s ()
  "AZIK拡張: しゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "sj") "しゃ"))
  (should (equal (nskk-input-azik-lookup "sl") "しゅ"))
  (should (equal (nskk-input-azik-lookup "so") "しょ")))

(ert-deftest nskk-input-azik-test-youon-t ()
  "AZIK拡張: ちゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "tj") "ちゃ"))
  (should (equal (nskk-input-azik-lookup "tl") "ちゅ"))
  (should (equal (nskk-input-azik-lookup "to") "ちょ")))

(ert-deftest nskk-input-azik-test-youon-n ()
  "AZIK拡張: にゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "nj") "にゃ"))
  (should (equal (nskk-input-azik-lookup "nl") "にゅ"))
  (should (equal (nskk-input-azik-lookup "no") "にょ")))

(ert-deftest nskk-input-azik-test-youon-h ()
  "AZIK拡張: ひゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "hj") "ひゃ"))
  (should (equal (nskk-input-azik-lookup "hl") "ひゅ"))
  (should (equal (nskk-input-azik-lookup "ho") "ひょ")))

(ert-deftest nskk-input-azik-test-youon-m ()
  "AZIK拡張: みゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "mj") "みゃ"))
  (should (equal (nskk-input-azik-lookup "ml") "みゅ"))
  (should (equal (nskk-input-azik-lookup "mo") "みょ")))

(ert-deftest nskk-input-azik-test-youon-r ()
  "AZIK拡張: りゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "rj") "りゃ"))
  (should (equal (nskk-input-azik-lookup "rl") "りゅ"))
  (should (equal (nskk-input-azik-lookup "ro") "りょ")))

(ert-deftest nskk-input-azik-test-youon-g ()
  "AZIK拡張: ぎゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "gj") "ぎゃ"))
  (should (equal (nskk-input-azik-lookup "gl") "ぎゅ"))
  (should (equal (nskk-input-azik-lookup "go") "ぎょ")))

(ert-deftest nskk-input-azik-test-youon-z ()
  "AZIK拡張: じゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "zj") "じゃ"))
  (should (equal (nskk-input-azik-lookup "zl") "じゅ"))
  (should (equal (nskk-input-azik-lookup "zo") "じょ")))

(ert-deftest nskk-input-azik-test-youon-b ()
  "AZIK拡張: びゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "bj") "びゃ"))
  (should (equal (nskk-input-azik-lookup "bl") "びゅ"))
  (should (equal (nskk-input-azik-lookup "bo") "びょ")))

(ert-deftest nskk-input-azik-test-youon-p ()
  "AZIK拡張: ぴゃ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "pj") "ぴゃ"))
  (should (equal (nskk-input-azik-lookup "pl") "ぴゅ"))
  (should (equal (nskk-input-azik-lookup "po") "ぴょ")))

;;; 二重母音・特殊入力テスト

(ert-deftest nskk-input-azik-test-special-n ()
  "AZIK拡張: 「ん」の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "q") "ん")))

(ert-deftest nskk-input-azik-test-special-d ()
  "AZIK拡張: だ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "dh") "で"))
  (should (equal (nskk-input-azik-lookup "dk") "だ"))
  (should (equal (nskk-input-azik-lookup "dl") "ど")))

(ert-deftest nskk-input-azik-test-fa-gyou ()
  "AZIK拡張: ファ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "fj") "ふぁ"))
  (should (equal (nskk-input-azik-lookup "fk") "ふぃ"))
  (should (equal (nskk-input-azik-lookup "fl") "ふぇ"))
  (should (equal (nskk-input-azik-lookup "fo") "ふぉ")))

(ert-deftest nskk-input-azik-test-vu-gyou ()
  "AZIK拡張: ヴァ行の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "vj") "ゔぁ"))
  (should (equal (nskk-input-azik-lookup "vk") "ゔぃ"))
  (should (equal (nskk-input-azik-lookup "vl") "ゔぇ"))
  (should (equal (nskk-input-azik-lookup "vo") "ゔぉ")))

(ert-deftest nskk-input-azik-test-special-chars ()
  "AZIK拡張: 特殊文字の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup "wh") "う"))
  (should (equal (nskk-input-azik-lookup "xj") "ゃ"))
  (should (equal (nskk-input-azik-lookup "xk") "ゅ"))
  (should (equal (nskk-input-azik-lookup "xl") "ょ")))

(ert-deftest nskk-input-azik-test-sokuon-kigou ()
  "AZIK拡張: 促音・記号の短縮入力テスト。"
  (should (equal (nskk-input-azik-lookup ";") "っ"))
  (should (equal (nskk-input-azik-lookup ":") "ー"))
  (should (equal (nskk-input-azik-lookup "@") "、"))
  (should (equal (nskk-input-azik-lookup "[") "「"))
  (should (equal (nskk-input-azik-lookup "]") "」")))

;;; 標準ローマ字フォールバックテスト

(ert-deftest nskk-input-azik-test-fallback-basic ()
  "標準ローマ字へのフォールバック: 基本五十音テスト。"
  (should (equal (nskk-input-azik-lookup "a") "あ"))
  (should (equal (nskk-input-azik-lookup "ka") "か"))
  (should (equal (nskk-input-azik-lookup "sa") "さ"))
  (should (equal (nskk-input-azik-lookup "ta") "た"))
  (should (equal (nskk-input-azik-lookup "na") "な")))

(ert-deftest nskk-input-azik-test-fallback-youon ()
  "標準ローマ字へのフォールバック: 拗音テスト。"
  (should (equal (nskk-input-azik-lookup "kya") "きゃ"))
  (should (equal (nskk-input-azik-lookup "sha") "しゃ"))
  (should (equal (nskk-input-azik-lookup "cha") "ちゃ"))
  (should (equal (nskk-input-azik-lookup "nya") "にゃ")))

(ert-deftest nskk-input-azik-test-fallback-voiced ()
  "標準ローマ字へのフォールバック: 濁音テスト。"
  (should (equal (nskk-input-azik-lookup "ga") "が"))
  (should (equal (nskk-input-azik-lookup "za") "ざ"))
  (should (equal (nskk-input-azik-lookup "da") "だ"))
  (should (equal (nskk-input-azik-lookup "ba") "ば"))
  (should (equal (nskk-input-azik-lookup "pa") "ぱ")))

(ert-deftest nskk-input-azik-test-not-found ()
  "存在しないローマ字入力のテスト。"
  (should (null (nskk-input-azik-lookup "xyz")))
  (should (null (nskk-input-azik-lookup "qqq")))
  (should (null (nskk-input-azik-lookup "zzz"))))

;;; 候補取得機能テスト

(ert-deftest nskk-input-azik-test-candidates-k ()
  "候補取得: 「k」で始まる候補テスト。"
  (let ((candidates (nskk-input-azik-get-candidates "k")))
    (should (member "ka" candidates))
    (should (member "ki" candidates))
    (should (member "ku" candidates))
    (should (member "kj" candidates))  ; AZIK拡張
    (should (member "kl" candidates))  ; AZIK拡張
    (should (member "ko" candidates))  ; AZIK拡張 (きょ)
    (should (member "kya" candidates))))

(ert-deftest nskk-input-azik-test-candidates-s ()
  "候補取得: 「s」で始まる候補テスト。"
  (let ((candidates (nskk-input-azik-get-candidates "s")))
    (should (member "sa" candidates))
    (should (member "si" candidates))
    (should (member "sj" candidates))  ; AZIK拡張
    (should (member "sl" candidates))  ; AZIK拡張
    (should (member "so" candidates))  ; AZIK拡張 (しょ)
    (should (member "sha" candidates))))

(ert-deftest nskk-input-azik-test-candidates-exact ()
  "候補取得: 完全一致候補テスト。"
  (let ((candidates (nskk-input-azik-get-candidates "kj")))
    (should (member "kj" candidates))
    (should (equal (length candidates) 1))))

(ert-deftest nskk-input-azik-test-candidates-special ()
  "候補取得: 特殊文字候補テスト。"
  (let ((candidates (nskk-input-azik-get-candidates ";")))
    (should (member ";" candidates)))
  (let ((candidates (nskk-input-azik-get-candidates "[")))
    (should (member "[" candidates))))

(ert-deftest nskk-input-azik-test-candidates-empty ()
  "候補取得: 存在しないプレフィックステスト。"
  (let ((candidates (nskk-input-azik-get-candidates "xyz")))
    (should (null candidates))))

;;; ハッシュテーブル初期化テスト

(ert-deftest nskk-input-azik-test-hash-init ()
  "ハッシュテーブル初期化テスト。"
  (setq nskk-input-azik-hash-table nil)
  (nskk-input-azik-init-hash-table)
  (should (hash-table-p nskk-input-azik-hash-table))
  (should (> (hash-table-count nskk-input-azik-hash-table) 0)))

(ert-deftest nskk-input-azik-test-hash-lookup ()
  "ハッシュテーブルからの検索テスト。"
  (nskk-input-azik-init-hash-table)
  (should (equal (gethash "kj" nskk-input-azik-hash-table) "きゃ"))
  (should (equal (gethash "ka" nskk-input-azik-hash-table) "か"))
  (should (null (gethash "xyz" nskk-input-azik-hash-table))))

;;; 統計情報テスト

(ert-deftest nskk-input-azik-test-stats ()
  "統計情報取得テスト。"
  (let ((stats (nskk-input-azik-stats)))
    (should (plist-get stats :total))
    (should (plist-get stats :azik-extension))
    (should (plist-get stats :base-romaji))
    (should (> (plist-get stats :total) 0))
    (should (> (plist-get stats :azik-extension) 0))
    (should (> (plist-get stats :base-romaji) 0))
    ;; 総数は拡張エントリ + 標準ローマ字エントリの合計
    (should (= (plist-get stats :total)
               (+ (plist-get stats :azik-extension)
                  (plist-get stats :base-romaji))))))

(ert-deftest nskk-input-azik-test-stats-extension-count ()
  "AZIK拡張テーブルのエントリ数検証。"
  (let ((stats (nskk-input-azik-stats)))
    ;; nskk-input-azik-extension-table の実際のエントリ数と一致すること
    (should (= (plist-get stats :azik-extension)
               (length nskk-input-azik-extension-table)))))

;;; 登録関数テスト

(ert-deftest nskk-input-azik-test-register ()
  "AZIK入力方式登録テスト。"
  (setq nskk-input-azik-hash-table nil)
  (nskk-input-azik-register)
  (should (hash-table-p nskk-input-azik-hash-table))
  (should (> (hash-table-count nskk-input-azik-hash-table) 0)))

;;; パフォーマンステスト

(ert-deftest nskk-input-azik-test-performance-lookup ()
  "ルックアップ処理のパフォーマンステスト (<0.5ms)。"
  (nskk-input-azik-init-hash-table)
  (let ((start-time (current-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (nskk-input-azik-lookup "kj")
      (nskk-input-azik-lookup "ka")
      (nskk-input-azik-lookup "sha"))
    (let* ((end-time (current-time))
           (elapsed (float-time (time-subtract end-time start-time)))
           (avg-time-ms (* (/ elapsed iterations) 1000 (/ 1.0 3))))
      ;; 1回あたりの平均処理時間が0.5ms未満であること
      (should (< avg-time-ms 0.5))
      (message "Average lookup time: %.4f ms" avg-time-ms))))

(ert-deftest nskk-input-azik-test-performance-init ()
  "ハッシュテーブル初期化のパフォーマンステスト (<10ms)。"
  (let ((start-time (current-time)))
    (nskk-input-azik-init-hash-table)
    (let* ((end-time (current-time))
           (elapsed-ms (* (float-time (time-subtract end-time start-time)) 1000)))
      ;; 初期化時間が10ms未満であること
      (should (< elapsed-ms 10.0))
      (message "Hash table initialization time: %.4f ms" elapsed-ms))))

(ert-deftest nskk-input-azik-test-memory-usage ()
  "メモリ使用量テスト (拡張テーブル用に100KB以内)。"
  (nskk-input-azik-init-hash-table)
  (let* ((hash-size (hash-table-count nskk-input-azik-hash-table))
         ;; 概算: (key文字列 + value文字列 + オーバーヘッド) * エントリ数
         ;; Emacs内部では1文字あたり約4バイトと仮定
         (estimated-bytes (* hash-size 100)))  ; エントリあたり平均100バイトと仮定
    ;; 100KB = 102400バイト以内であること
    (should (< estimated-bytes 102400))
    (message "Estimated memory usage: %d bytes (%.2f KB)"
             estimated-bytes (/ estimated-bytes 1024.0))))

;;; 高速化インライン関数テスト

(ert-deftest nskk-input-azik-test-lookup-fast ()
  "高速ルックアップ関数のテスト。"
  (nskk-input-azik-init-hash-table)
  (should (equal (nskk-input-azik-lookup-fast "kj") "きゃ"))
  (should (equal (nskk-input-azik-lookup-fast "ka") "か"))
  (should (null (nskk-input-azik-lookup-fast "xyz"))))

;;; エッジケーステスト

(ert-deftest nskk-input-azik-test-edge-empty ()
  "空文字列入力のテスト。"
  (should (null (nskk-input-azik-lookup ""))))

(ert-deftest nskk-input-azik-test-edge-long ()
  "長い文字列入力のテスト。"
  (should (null (nskk-input-azik-lookup "abcdefghijklmnop"))))

(ert-deftest nskk-input-azik-test-edge-case-sensitive ()
  "大文字小文字区別のテスト。"
  ;; AZIK/ローマ字テーブルは小文字のみを想定
  (should (null (nskk-input-azik-lookup "KJ")))
  (should (null (nskk-input-azik-lookup "Ka"))))

;;; 統合テスト

(ert-deftest nskk-input-azik-test-integration-sentence ()
  "文章入力の統合テスト。"
  ;; 「きょう」を AZIK で入力 -> "ko" + "u" (標準)
  (should (equal (nskk-input-azik-lookup "ko") "きょ"))
  (should (equal (nskk-input-azik-lookup "u") "う"))
  ;; 「しゅくだい」を AZIK で入力 -> "sl" + "ku" + "da" + "i"
  (should (equal (nskk-input-azik-lookup "sl") "しゅ"))
  (should (equal (nskk-input-azik-lookup "ku") "く"))
  (should (equal (nskk-input-azik-lookup "da") "だ"))
  (should (equal (nskk-input-azik-lookup "i") "い")))

(provide 'nskk-input-azik-test)

;;; nskk-input-azik-test.el ends here
