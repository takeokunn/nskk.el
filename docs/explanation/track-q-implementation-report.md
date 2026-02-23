# Track Q: 7層アーキテクチャ設計仕様書

## 設計概要

Track Q（ランタイム統合: Task 3.11-3.18）として、NSKKの7層アーキテクチャの設計仕様を定義します。

**設計タスク**: 8タスク
**計画ファイル数**: 8ファイル

## モジュール設計

### 1. nskk-layer-presentation.el (Task 3.11)

**責務**: Presentation Layer - UI統合、イベントハンドリング

**主要機能**:
- キーマップ統合
- 候補ウィンドウ制御（インライン/ボトム/トップ表示）
- ミニバッファUI
- モードライン表示
- イベントハンドリング

**公開API**:
- `nskk-presentation-initialize`
- `nskk-presentation-handle-key-event`
- `nskk-presentation-show-candidates`
- `nskk-presentation-hide-candidates`
- `nskk-presentation-update-mode-line`
- `nskk-presentation-health-check`

### 2. nskk-layer-extension.el (Task 3.12)

**責務**: Extension Layer - フック、イベントバス、レイヤー間通信

**主要機能**:
- フックシステム（豊富な拡張ポイント）
- イベントバス（publish-subscribe）
- レイヤー間メッセージディスパッチャー
- 拡張ポイントレジストリ
- イベント履歴管理

**公開API**:
- `nskk-extension-add-hook` / `nskk-extension-remove-hook`
- `nskk-extension-subscribe` / `nskk-extension-unsubscribe`
- `nskk-extension-emit-event`
- `nskk-extension-send-message`
- `nskk-extension-register-extension-point`
- `nskk-extension-get-event-history`

**標準拡張ポイント**:
- `:before-conversion` / `:after-conversion`
- `:before-candidate-selection` / `:after-candidate-selection`
- `:before-mode-change` / `:after-mode-change`
- `:before-dictionary-lookup` / `:after-dictionary-lookup`
- `:before-learning` / `:after-learning`

### 3. nskk-layer-application.el (Task 3.13)

**責務**: Application Layer - ビジネスロジック、変換制御

**主要機能**:
- 入力処理（ひらがな/カタカナ/英数/全角英数）
- 変換プロセス制御
- 候補管理（選択、確定）
- モード管理
- 学習機能統合
- セッション管理

**公開API**:
- `nskk-application-process-input`
- `nskk-application-start-conversion`
- `nskk-application-select-candidate`
- `nskk-application-commit-candidate`
- `nskk-application-switch-mode`
- `nskk-application-current-mode`
- `nskk-application-toggle-kana`

**データ構造**:
- `nskk-application-conversion-state` - 変換状態

### 4. nskk-layer-core.el (Task 3.14)

**責務**: Core Engine Layer - 変換エンジン、辞書エンジン

**主要機能**:
- ローマ字→かな変換エンジン（キャッシュ付き）
- かな⇔カタカナ変換
- 半角⇔全角変換
- 辞書検索インターフェース
- 特殊文字処理（促音・撥音）
- パフォーマンス測定

**公開API**:
- `nskk-core-convert-romaji`
- `nskk-core-hiragana-to-katakana`
- `nskk-core-katakana-to-hiragana`
- `nskk-core-hankaku-to-zenkaku`
- `nskk-core-zenkaku-to-hankaku`
- `nskk-core-search-dictionary`
- `nskk-core-optimize-table`
- `nskk-core-clear-cache`

### 5. nskk-layer-data.el (Task 3.15)

**責務**: Data Access Layer - 永続化、辞書アクセス

**主要機能**:
- 辞書データの読み書き
- 学習データの永続化
- キャッシュ管理
- 自動保存（タイマー）
- トランザクション管理
- エクスポート/インポート

**公開API**:
- `nskk-data-load-dictionary`
- `nskk-data-search`
- `nskk-data-learn`
- `nskk-data-save-learning-data`
- `nskk-data-export-learning-data`
- `nskk-data-import-learning-data`
- `nskk-data-clear-cache`

**カスタマイズ変数**:
- `nskk-data-dictionary-paths` - 辞書ファイルパスリスト
- `nskk-data-learning-file` - 学習データファイルパス
- `nskk-data-auto-save` - 自動保存有効化
- `nskk-data-auto-save-interval` - 自動保存間隔

### 6. nskk-layer-infrastructure.el (Task 3.16)

**責務**: Infrastructure Layer - リソース管理、非同期処理

**主要機能**:
- タスク実行（Emacsの協調的スレッドを利用）
- ファイルI/O（同期・非同期）
- タイマー管理
- リソース監視
- 統計情報収集

**注意**: Emacs Lispはガベージコレクションによるメモリ管理を行うため、手動でのメモリ確保・解放は不可能であり設計対象外です。また、Emacsのスレッドは協調的（cooperative）であり、真の並列処理ではありません。CPUバウンドな処理はスレッドでは高速化できない点に留意する必要があります。

**公開API**:
- `nskk-infrastructure-submit-task`
- `nskk-infrastructure-run-async`
- `nskk-infrastructure-read-file` / `nskk-infrastructure-write-file`
- `nskk-infrastructure-read-file-async` / `nskk-infrastructure-write-file-async`
- `nskk-infrastructure-schedule-timer`
- `nskk-infrastructure-get-statistics`

**カスタマイズ変数**:
- `nskk-infrastructure-thread-pool-size` - スレッドプールサイズ
- `nskk-infrastructure-enable-monitoring` - リソース監視有効化

### 7. nskk-layer-qa.el (Task 3.17)

**責務**: QA Layer - テスト、ベンチマーク統合、品質保証

**主要機能**:
- テストランナー（全テスト/ユニット/統合）
- ベンチマークハーネス
- カバレッジ測定
- パフォーマンス検証
- 品質メトリクス収集
- 品質レポート生成

**公開API**:
- `nskk-qa-run-all-tests`
- `nskk-qa-run-unit-tests`
- `nskk-qa-run-integration-tests`
- `nskk-qa-run-benchmarks`
- `nskk-qa-measure-coverage`
- `nskk-qa-verify-performance`
- `nskk-qa-generate-report`
- `nskk-qa-get-metrics`

**カスタマイズ変数**:
- `nskk-qa-benchmark-iterations` - ベンチマーク繰り返し回数
- `nskk-qa-coverage-target` - 目標カバレッジ（%）
- `nskk-qa-enable-continuous-testing` - 継続的テスト有効化

### 8. nskk-architecture.el (Task 3.18)

**責務**: レイヤー統合 - レイヤー間通信、イベントフロー

**主要機能**:
- 7層の初期化・シャットダウン管理
- レイヤー間通信ルーティング
- イベントフロー制御
- 通信トレース
- アーキテクチャヘルスチェック
- アーキテクチャ図表示
- 統計情報収集

**公開API**:
- `nskk-architecture-initialize`
- `nskk-architecture-shutdown`
- `nskk-architecture-health-check`
- `nskk-architecture-show-diagram`
- `nskk-architecture-get-communication-log`
- `nskk-architecture-get-statistics`
- `nskk-architecture-enable-debug` / `nskk-architecture-disable-debug`

**ルーティングハンドラー**:
- Presentation → Application
- Application → Core
- Application → Data
- Core → Data
- Data → Infrastructure

## アーキテクチャ設計方針

### 1. 明確な責務分離

各レイヤーは単一の明確な責務を持つ:

- **Presentation**: UIのみ
- **Extension**: レイヤー間通信のみ
- **Application**: ビジネスロジックのみ
- **Core**: 変換エンジンのみ
- **Data**: データアクセスのみ
- **Infrastructure**: リソース管理のみ
- **QA**: 品質保証のみ

### 2. イベント駆動アーキテクチャ

すべてのレイヤー間通信はExtension Layerのイベントバスを経由し、疎結合を実現。

### 3. 依存性逆転

上位レイヤーは下位レイヤーの抽象に依存し、具象には依存しない。

### 4. テスタビリティ

各レイヤーが独立してテスト可能。QA Layerで品質を継続的に検証。

## 層間通信フロー

### 典型的な入力処理フロー

```
ユーザーキー入力 (e.g., "ka")
  ↓
[Presentation Layer]
  - nskk-presentation-handle-key-event
  ↓ Extension Layer (メッセージング)
[Application Layer]
  - nskk-application-process-input
  - モード判定（ひらがな/カタカナ/etc.）
  ↓ Extension Layer (メッセージング)
[Core Layer]
  - nskk-core-convert-romaji
  - キャッシュチェック
  - 変換実行: "ka" → "か"
  ↓
[Application Layer]
  - 変換結果受信
  ↓ Extension Layer (イベント発行)
[Presentation Layer]
  - UI更新
  - 確定文字表示
```

### 辞書検索から候補表示までのフロー

```
[Application Layer]
  - nskk-application-start-conversion
  - 変換状態作成
  ↓ Extension Layer (メッセージング)
[Data Layer]
  - nskk-data-search
  - キャッシュチェック
  ↓ (キャッシュミス時)
  - 辞書検索実行
  ↓ Infrastructure Layer (ファイルI/O)
[Infrastructure Layer]
  - 辞書ファイル読み込み
  ↓
[Data Layer]
  - 学習データでソート
  - キャッシュ保存
  ↓ Extension Layer (メッセージング)
[Application Layer]
  - 候補リスト受信
  - 候補状態更新
  ↓ Extension Layer (イベント発行: :conversion-started)
[Presentation Layer]
  - nskk-presentation-show-candidates
  - 候補ウィンドウ表示
```

## パフォーマンス設計

### キャッシング戦略

1. **Core Layer**: 変換結果キャッシュ（10,000エントリ）
2. **Data Layer**: 検索結果キャッシュ
3. **Infrastructure Layer**: ファイルI/Oキャッシュ

### 非同期処理

Infrastructure Layerでスレッドプールを提供（Emacsの協調的スレッド使用）:
- 辞書ロード
- 学習データ保存
- バックグラウンド処理

**注意**: Emacsのスレッドは協調的であるため、CPUバウンドな処理の並列化には向きません。I/O待ちの多い処理（辞書ロード、ネットワーク通信等）に適しています。

### メモリ管理

- Emacsのガベージコレクタに委ねる
- 大きなデータ構造のキャッシュにはサイズ上限を設定
- リソース監視による使用状況の可視化

## 品質保証

### テストカバレッジ目標

- 目標: 95%以上
- 測定: `nskk-qa-measure-coverage`

### パフォーマンス目標

| 処理 | 目標 |
|------|------|
| ローマ字変換 | < 0.1ms |
| 辞書検索 | < 10ms |
| 候補表示 | < 50ms |
| 学習処理 | < 5ms |

### ベンチマーク

- 自動ベンチマーク実行: `nskk-qa-run-benchmarks`
- パフォーマンス検証: `nskk-qa-verify-performance`

## デバッグ機能

### 全レイヤーデバッグ

```elisp
(nskk-architecture-enable-debug)
```

### 通信トレース

```elisp
(setopt nskk-architecture-enable-tracing t)
(nskk-architecture-get-communication-log)
```

### レイヤー別ヘルスチェック

```elisp
(nskk-presentation-health-check)
(nskk-application-health-check)
(nskk-core-health-check)
(nskk-data-health-check)
(nskk-infrastructure-health-check)
(nskk-qa-health-check)
(nskk-architecture-health-check)
```

## 拡張性

### プラグイン開発例

```elisp
;; 変換前処理プラグイン
(nskk-extension-add-hook :before-conversion
  (lambda (input)
    (message "Converting: %s" input)
    input))

;; 候補選択後処理プラグイン
(nskk-extension-add-hook :after-candidate-selection
  (lambda (candidate)
    (message "Selected: %s" candidate)
    nil))

;; 辞書検索後処理（候補フィルタリング）
(nskk-extension-add-hook :after-dictionary-lookup
  (lambda (query results)
    (seq-filter (lambda (r) (not (string-match-p "old" r)))
                results)))
```

### イベント購読例

```elisp
(nskk-extension-subscribe :mode-changed
  (lambda (&rest data)
    (let ((from (plist-get data :from))
          (to (plist-get data :to)))
      (message "Mode changed: %s -> %s" from to))))
```

## 次のステップ

### ランタイム統合の残りタスク

1. **Track N: Threading** - スレッドプール、辞書検索の非同期化
2. **Track O: Async UI** - 非同期候補表示
3. **Track P: Profiling** - プロファイラー、ボトルネック検出
4. **Track R: Plugin System** - プラグインAPI、動的ロード
5. **Track S: ミニバッファUI** - 設定メニュー、拡張管理UI
6. **Track T: Optimization** - マクロ最適化、Native Compile

### 統合作業

1. 既存モジュール（Track A-F）との統合
2. レイヤー間通信の実装完成
3. テストケース追加
4. ドキュメント整備
