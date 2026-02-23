# NSKK サーバー通信設計仕様書

## 概要

NSKK Track L（辞書サーバー）の設計仕様書です。SKK辞書サーバー（skkserv）プロトコルに対応した非同期通信機能を提供します。

## 設計モジュール

### サーバープロトコル (`nskk-server-protocol.el`)

skkservプロトコルのリクエスト生成・レスポンス解析を担当するモジュールです。

**主要機能:**
- リクエスト生成
  - 辞書検索 (`'1'`コマンド) -- 送り仮名あり/なしの区別はクライアント側で処理する
  - 補完要求 (`'4'`コマンド) -- 前方一致の補完要求
  - サーバー情報要求 (`'0'`, `'2'`, `'3'`コマンド)
- レスポンスパース
  - 検索成功: `1/候補1/候補2/` 形式
  - 検索失敗（NOT FOUND）: レスポンス `4`
  - 候補リスト抽出
  - 注釈（アノテーション）対応
- エンコーディング処理（EUC-JP/UTF-8）

**注意**: skkservプロトコルでは送り仮名あり/なしの区別はプロトコル上のコマンドとして分離されていません。コマンド`1`が辞書検索（送り仮名あり・なし両方）を担当し、送り仮名の処理はクライアント（ddskk等）側の責務です。

**API設計例:**

```elisp
;; リクエスト生成（送り仮名なし）
(nskk-server-protocol-make-request "かんじ")
;; => "1かんじ "

;; 補完要求
(nskk-server-protocol-make-completion-request "かん")
;; => "4かん "

;; レスポンスパース（検索成功）
(nskk-server-protocol-parse-response "1/漢字/幹事/")
;; => nskk-server-response構造体
;;    :status 'found
;;    :candidates (("漢字" . nil) ("幹事" . nil))

;; レスポンスパース（NOT FOUND）
(nskk-server-protocol-parse-response "4")
;; => nskk-server-response構造体
;;    :status 'not-found
```

**データ構造:**

- `nskk-server-request`: リクエスト表現
  - `command`: コマンド文字
  - `key`: 検索キー
  - `encoding`: エンコーディング

- `nskk-server-response`: レスポンス表現
  - `status`: ステータス (`'found`/`'not-found`/`'info`)
  - `candidates`: 候補リスト
  - `info`: サーバー情報文字列

### 非同期通信 (`nskk-server-async.el`)

`make-network-process`を使用した非同期通信モジュールです。

**主要機能:**
- 非同期リクエスト処理
- コールバックベースAPI
- タイムアウト処理（デフォルト3秒）
- コネクションプーリング
- 並列リクエスト対応（最大5接続）
- 自動再接続

**API設計例:**

```elisp
;; 非同期検索
(nskk-server-async-search
  "localhost" 1178 "かんじ"
  ;; 成功コールバック
  (lambda (response)
    (message "Found: %S"
      (nskk-server-response-candidates response)))
  ;; エラーコールバック
  (lambda (error)
    (message "Error: %s" error)))

;; 接続管理
(nskk-server-async-connection-count)  ;; => 現在の接続数
(nskk-server-async-close-all-connections)  ;; 全接続クローズ
```

**データ構造:**

- `nskk-server-connection`: サーバー接続
  - `host`/`port`: 接続先
  - `process`: ネットワークプロセス
  - `state`: 接続状態 (`'connecting`/`'connected`/`'closed`)
  - `pending`: 保留中のリクエストキュー
  - `buffer`: 受信バッファ

- `nskk-server-request-context`: リクエストコンテキスト
  - `request`: リクエスト文字列
  - `callback`: 成功コールバック
  - `error-callback`: エラーコールバック
  - `timeout`: タイムアウト時刻
  - `timer`: タイムアウトタイマー

**処理フロー:**

```
1. リクエスト生成
   ↓
2. コネクション取得（プールまたは新規作成）
   ↓
3. リクエストキューに追加
   ↓
4. タイムアウトタイマー設定
   ↓
5. 送信（接続済みの場合即座に、未接続の場合は接続完了後）
   ↓
6. レスポンス受信
   ↓
7. コールバック呼び出し
```

### サーバーエラー処理 (`nskk-server-error.el`)

堅牢なエラー処理とフォールバック機構のモジュールです。

**主要機能:**
- エラー検出と分類
  - 接続エラー (`'connection-error`)
  - タイムアウト (`'timeout`)
  - プロトコルエラー (`'protocol-error`)
  - サーバーエラー (`'server-error`)
- リトライロジック（指数バックオフ）
  - デフォルト最大3回
  - 初回遅延0.5秒、バックオフ係数2.0
- サーキットブレーカーパターン
  - エラー閾値：デフォルト5回
  - オープン時間：デフォルト60秒
  - 状態遷移：`closed` → `open` → `half-open` → `closed`
- ローカル辞書へのフォールバック
- エラーログ記録（最大100エントリ）
- エラー統計収集

**API設計例:**

```elisp
;; リトライ付き検索
(nskk-server-error-with-retry
  (lambda (success error)
    (nskk-server-async-search
      "localhost" 1178 "かんじ"
      success error))
  (lambda (response) (message "Success"))
  (lambda (error) (message "Failed: %s" error)))

;; フォールバック付き検索
(nskk-server-error-with-fallback
  "localhost" 1178 "かんじ"
  success-callback error-callback
  local-dict-index)  ;; ローカル辞書

;; エラーログ取得
(nskk-server-error-get-log 'timeout)  ;; タイムアウトエラーのみ

;; 統計取得
(nskk-server-error-get-statistics "localhost" 1178)
;; => (:total-requests 100 :total-errors 5 :error-rate 0.05 ...)
```

**データ構造:**

- `nskk-server-error-log-entry`: エラーログエントリ
  - `timestamp`: タイムスタンプ
  - `error-type`: エラータイプ
  - `message`: エラーメッセージ
  - `host`/`port`: 接続先
  - `request`: リクエスト内容

- `nskk-server-circuit-breaker`: サーキットブレーカー状態
  - `state`: 状態 (`'closed`/`'open`/`'half-open`)
  - `error-count`: エラーカウント
  - `last-error`: 最後のエラー時刻
  - `open-time`: オープンした時刻

- `nskk-server-error-statistics`: エラー統計
  - `total-requests`: 総リクエスト数
  - `total-errors`: 総エラー数
  - `error-by-type`: タイプ別エラー数
  - `last-error-time`: 最後のエラー時刻

**サーキットブレーカー動作:**

```
[Closed] → エラー蓄積（閾値5回）
    ↓
[Open] → リクエスト拒否（60秒間）
    ↓
[Half-Open] → 試験的にリクエスト許可
    ↓ 成功        ↓ 失敗
[Closed]      [Open]
```

## 対応プロトコル

### skkservプロトコル仕様

skkservプロトコルには公式なバージョン番号は存在しません。以下はNSKKプロジェクト内での便宜的な分類です。

**基本プロトコル:**
- コマンド: `'0'`, `'1'`, `'2'`, `'3'`, `'4'`
- エンコーディング: EUC-JP（従来サーバー）またはUTF-8（一部の新しいサーバー実装）
- レスポンス形式: `1/候補1/候補2/`（検索成功）または `4`（NOT FOUND）

### コマンド一覧

| コマンド | 機能 | リクエスト形式 | レスポンス形式 |
|---------|------|---------------|---------------|
| `'0'` | サーバー切断 | `0` | (なし) |
| `'1'` | 辞書検索 | `1<key> ` | `1/<候補>/...`（成功）or `4`（NOT FOUND） |
| `'2'` | プロトコルバージョン要求 | `2` | `<version-text>` |
| `'3'` | ホスト名要求 | `3` | `<hostname>` |
| `'4'` | 補完要求（前方一致） | `4<key> ` | `1/<候補>/...`（成功）or `4`（NOT FOUND） |

**補足:**
- コマンド`1`は送り仮名あり・なしの両方に使用されます。送り仮名の判定・処理はクライアント側の責務です。
- レスポンスの`4`は「NOT FOUND」を意味します（コマンド`4`の補完要求とは別物）。
- コマンド`4`（補完要求）はサーバーによっては未対応の場合があります。

## 使用例

### 基本的な使用法

```elisp
(require 'nskk-server-protocol)
(require 'nskk-server-async)
(require 'nskk-server-error)

;; 1. 基本的な非同期検索
(nskk-server-async-search
  "localhost" 1178 "かんじ"
  (lambda (response)
    (if (eq (nskk-server-response-status response) 'found)
        (message "Found: %S" (nskk-server-response-candidates response))
      (message "Not found")))
  (lambda (error)
    (message "Error: %s" error)))

;; 2. リトライ付き検索
(nskk-server-error-with-retry
  (lambda (success error)
    (nskk-server-async-search
      "localhost" 1178 "かんじ"
      success error))
  (lambda (response) (message "Success: %S" response))
  (lambda (error) (message "Failed after retries: %s" error))
  3)  ;; 最大3回リトライ

;; 3. フォールバック付き検索（ローカル辞書へフォールバック）
(let ((local-dict (nskk-load-dictionary "~/.skk/jisyo")))
  (nskk-server-error-with-fallback
    "localhost" 1178 "かんじ"
    (lambda (response)
      (message "Result: %S" (nskk-server-response-candidates response)))
    (lambda (error)
      (message "Total failure: %s" error))
    local-dict))

;; 4. エラーログ確認
(let ((errors (nskk-server-error-get-log 'timeout)))
  (message "Timeout errors: %d" (length errors)))

;; 5. 統計情報取得
(let ((stats (nskk-server-error-get-statistics "localhost" 1178)))
  (message "Error rate: %.2f%%"
    (* 100 (plist-get stats :error-rate))))

;; 6. 接続管理
(message "Active connections: %d" (nskk-server-async-connection-count))
(nskk-server-async-close-all-connections)
```

### カスタマイズ

```elisp
;; タイムアウト設定
(setopt nskk-server-async-timeout 5.0)  ;; 5秒

;; リトライ設定
(setopt nskk-server-error-max-retries 5)  ;; 最大5回
(setopt nskk-server-error-retry-delay 1.0)  ;; 初回1秒
(setopt nskk-server-error-retry-backoff 2.0)  ;; バックオフ係数2.0

;; サーキットブレーカー設定
(setopt nskk-server-error-circuit-breaker-threshold 10)  ;; 閾値10回
(setopt nskk-server-error-circuit-breaker-timeout 120.0)  ;; タイムアウト2分

;; フォールバック無効化
(setopt nskk-server-error-fallback-enabled nil)

;; デバッグログ有効化
(setopt nskk-server-protocol-verbose t)
(setopt nskk-server-async-verbose t)
(setopt nskk-server-error-verbose t)
```

## パフォーマンス目標

| 項目 | 目標 |
|------|------|
| リクエスト応答時間 | < 100ms（ローカルサーバー） |
| 並列リクエスト | 最大5接続の同時処理 |
| タイムアウト | デフォルト3秒 |
| リトライ | 指数バックオフ |

### 最適化方針

- **コネクションプーリング:** 接続の再利用により接続オーバーヘッドを削減
- **非同期処理:** `make-network-process`による完全非同期
- **タイムアウト管理:** タイマーによる確実なタイムアウト処理
- **エラーハンドリング:** サーキットブレーカーによる無駄なリクエスト削減

## テスト計画

### テスト対象

- **プロトコルテスト:**
  - リクエスト生成（辞書検索・補完要求）
  - レスポンスパース（成功/失敗/注釈付き）
  - エンコーディング処理

- **非同期通信テスト:**
  - コネクションプールキー生成
  - 接続有効性判定
  - 接続管理

- **エラー処理テスト:**
  - エラーログ記録/取得/クリア
  - サーキットブレーカー動作
  - エラー分類
  - 統計収集

- **統合テスト:**
  - プロトコル：リクエスト生成→レスポンスパース
  - エラー処理：ログ記録と取得
  - サーキットブレーカー：エラー蓄積→オープン→クローズ

## 計画ファイル構成

```
nskk.el/
├── nskk-server-protocol.el  - プロトコル処理
├── nskk-server-async.el     - 非同期通信
├── nskk-server-error.el     - エラー処理
└── tests/
    └── nskk-server-test.el  - テストスイート
```

## 今後の拡張可能性

### Phase 2以降で検討

1. **パフォーマンス最適化:**
   - リクエストバッチング
   - レスポンスキャッシング

2. **高度なエラー処理:**
   - アダプティブタイムアウト
   - 詳細なエラー分析

3. **モニタリング:**
   - メトリクス収集
   - パフォーマンス分析

## 参考資料

### skkservプロトコル

- [SKK OpenLab - skkserv protocol](http://openlab.ring.gr.jp/skk/skk/doc/skk.html#skkserv)
- [ddskk - skk-server.el](https://github.com/skk-dev/ddskk/blob/master/skk-server.el)

### Emacs ネットワークプログラミング

- [Emacs Lisp Reference Manual - Network Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Network.html)
- [make-network-process documentation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Network-Processes.html)
