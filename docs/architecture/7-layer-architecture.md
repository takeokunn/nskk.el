# NSKK 7層アーキテクチャ

## 概要

NSKKは、保守性・拡張性・テスタビリティを最大化するために、7層アーキテクチャを採用しています。各レイヤーは明確な責務を持ち、下位レイヤーのみに依存する階層構造を形成しています。

## アーキテクチャ図

```
┌─────────────────────────────────────────────────────────┐
│                Layer 1: Presentation                    │
│              UI統合、イベントハンドリング                  │
│  nskk-layer-presentation.el                             │
├─────────────────────────────────────────────────────────┤
│                Layer 2: Extension                       │
│          フック、イベントバス、レイヤー間通信              │
│  nskk-layer-extension.el                                │
├─────────────────────────────────────────────────────────┤
│                Layer 3: Application                     │
│           ビジネスロジック、変換制御                       │
│  nskk-layer-application.el                              │
├─────────────────────────────────────────────────────────┤
│                Layer 4: Core Engine                     │
│           変換エンジン、辞書エンジン                       │
│  nskk-layer-core.el                                     │
├─────────────────────────────────────────────────────────┤
│                Layer 5: Data Access                     │
│              永続化、同期、辞書アクセス                    │
│  nskk-layer-data.el                                     │
├─────────────────────────────────────────────────────────┤
│                Layer 6: Infrastructure                  │
│         スレッド管理、メモリ管理、リソース管理             │
│  nskk-layer-infrastructure.el                           │
├─────────────────────────────────────────────────────────┤
│                Layer 7: QA                              │
│          テスト、ベンチマーク統合、品質保証                │
│  nskk-layer-qa.el                                       │
└─────────────────────────────────────────────────────────┘

               統合レイヤー: nskk-architecture.el
```

## レイヤー詳細

### Layer 1: Presentation Layer

**ファイル**: `nskk-layer-presentation.el`

**責務**:
- UIコンポーネントの統合
- ユーザー入力のイベントハンドリング
- 候補ウィンドウ制御
- モードライン表示
- インライン表示

**主要機能**:
- `nskk-presentation-handle-key-event` - キーイベント処理
- `nskk-presentation-show-candidates` - 候補表示
- `nskk-presentation-update-mode-line` - モードライン更新

**依存レイヤー**: Extension, Application

### Layer 2: Extension Layer

**ファイル**: `nskk-layer-extension.el`

**責務**:
- フックポイント管理（300+拡張ポイント）
- イベントバス実装
- レイヤー間メッセージング
- プラグインインターフェース

**主要機能**:
- `nskk-extension-add-hook` - フック追加
- `nskk-extension-emit-event` - イベント発行
- `nskk-extension-send-message` - レイヤー間メッセージ送信
- `nskk-extension-register-extension-point` - 拡張ポイント登録

**依存レイヤー**: なし（すべてのレイヤーから利用される）

### Layer 3: Application Layer

**ファイル**: `nskk-layer-application.el`

**責務**:
- 日本語入力のビジネスロジック
- 変換プロセス制御
- 候補選択ロジック
- 学習機能統合
- モード管理

**主要機能**:
- `nskk-application-process-input` - 入力処理
- `nskk-application-start-conversion` - 変換開始
- `nskk-application-select-candidate` - 候補選択
- `nskk-application-switch-mode` - モード切り替え

**依存レイヤー**: Extension, Core, Data

### Layer 4: Core Engine Layer

**ファイル**: `nskk-layer-core.el`

**責務**:
- ローマ字→かな変換エンジン
- 辞書検索エンジン
- 文字変換ユーティリティ
- 変換アルゴリズム最適化

**主要機能**:
- `nskk-core-convert-romaji` - ローマ字変換
- `nskk-core-hiragana-to-katakana` - ひらがな→カタカナ変換
- `nskk-core-search-dictionary` - 辞書検索
- `nskk-core-optimize-table` - 変換テーブル最適化

**依存レイヤー**: Infrastructure

### Layer 5: Data Access Layer

**ファイル**: `nskk-layer-data.el`

**責務**:
- 辞書データの読み書き
- 学習データの永続化
- キャッシュ管理
- データ同期
- トランザクション管理

**主要機能**:
- `nskk-data-load-dictionary` - 辞書ロード
- `nskk-data-search` - 辞書検索
- `nskk-data-learn` - 学習データ記録
- `nskk-data-save-learning-data` - 学習データ保存

**依存レイヤー**: Infrastructure

### Layer 6: Infrastructure Layer

**ファイル**: `nskk-layer-infrastructure.el`

**責務**:
- スレッド管理
- メモリ管理
- ファイルI/O
- タイマー管理
- リソースプール

**主要機能**:
- `nskk-infrastructure-submit-task` - 非同期タスク投入
- `nskk-infrastructure-allocate-memory` - メモリ確保
- `nskk-infrastructure-read-file` - ファイル読み込み
- `nskk-infrastructure-schedule-timer` - タイマースケジュール

**依存レイヤー**: なし（最下層）

### Layer 7: QA Layer

**ファイル**: `nskk-layer-qa.el`

**責務**:
- テストフレームワーク統合
- ベンチマーク実行
- カバレッジ測定
- パフォーマンス検証
- 品質メトリクス収集

**主要機能**:
- `nskk-qa-run-all-tests` - 全テスト実行
- `nskk-qa-run-benchmarks` - ベンチマーク実行
- `nskk-qa-verify-performance` - パフォーマンス検証
- `nskk-qa-generate-report` - 品質レポート生成

**依存レイヤー**: すべて（テスト対象）

### 統合レイヤー

**ファイル**: `nskk-architecture.el`

**責務**:
- レイヤー間通信のセットアップ
- 依存性注入
- イベントフロー制御
- アーキテクチャ全体の初期化・シャットダウン

**主要機能**:
- `nskk-architecture-initialize` - アーキテクチャ初期化
- `nskk-architecture-health-check` - ヘルスチェック
- `nskk-architecture-show-diagram` - アーキテクチャ図表示

## レイヤー間通信フロー

### 入力処理フロー

```
ユーザーキー入力
  ↓
Presentation Layer (キーイベント受信)
  ↓ Extension Layer経由
Application Layer (入力処理、モード判定)
  ↓ Extension Layer経由
Core Layer (ローマ字→かな変換)
  ↓
Application Layer (変換結果を受信)
  ↓ Extension Layer経由
Presentation Layer (UI更新)
```

### 辞書検索フロー

```
Application Layer (変換開始)
  ↓ Extension Layer経由
Data Layer (辞書検索実行)
  ↓
Data Layer (キャッシュチェック)
  ↓ Infrastructure Layer経由
Infrastructure Layer (ファイルI/O)
  ↓
Data Layer (学習データでソート)
  ↓ Extension Layer経由
Application Layer (候補受信)
  ↓ イベント発行
Presentation Layer (候補表示)
```

### 学習データ保存フロー

```
Application Layer (候補確定)
  ↓ Extension Layer経由
Data Layer (学習データ記録)
  ↓ 自動保存タイマー
Data Layer (永続化)
  ↓ Infrastructure Layer経由
Infrastructure Layer (ファイル書き込み)
```

## イベント駆動アーキテクチャ

すべてのレイヤー間通信は、Extension Layerのイベントバスを経由します。

### 主要イベント

- `:conversion-started` - 変換開始
- `:conversion-committed` - 変換確定
- `:mode-changed` - モード変更
- `:candidate-selected` - 候補選択
- `:input-received` - 入力受信
- `:error` - エラー発生

### 標準拡張ポイント

- `:before-conversion` - 変換前処理
- `:after-conversion` - 変換後処理
- `:before-candidate-selection` - 候補選択前処理
- `:after-candidate-selection` - 候補選択後処理
- `:before-mode-change` - モード変更前処理
- `:after-mode-change` - モード変更後処理
- `:before-dictionary-lookup` - 辞書検索前処理
- `:after-dictionary-lookup` - 辞書検索後処理
- `:before-learning` - 学習前処理
- `:after-learning` - 学習後処理

## 初期化シーケンス

レイヤーは下から順に初期化されます：

```
1. Infrastructure Layer
   ↓
2. Data Layer
   ↓
3. Core Layer
   ↓
4. Application Layer
   ↓
5. Extension Layer
   ↓
6. Presentation Layer
   ↓
7. QA Layer
```

シャットダウンは逆順で行われます。

## 依存関係マトリクス

| Layer \ 依存先 | Infrastructure | Data | Core | Application | Extension | Presentation | QA |
|---------------|----------------|------|------|-------------|-----------|--------------|-----|
| Presentation  |                |      |      | ○           | ○         |              |     |
| Extension     |                |      |      |             |           |              |     |
| Application   |                | ○    | ○    |             | ○         |              |     |
| Core          | ○              |      |      |             |           |              |     |
| Data          | ○              |      |      |             |           |              |     |
| Infrastructure|                |      |      |             |           |              |     |
| QA            | ○              | ○    | ○    | ○           | ○         | ○            |     |

○ = 依存関係あり

## 設計原則

### 1. 単一責任の原則 (SRP)

各レイヤーは明確な単一の責任を持ちます。

### 2. 依存性逆転の原則 (DIP)

上位レイヤーは下位レイヤーの抽象に依存し、Extension Layerを通じて通信します。

### 3. オープン・クローズドの原則 (OCP)

拡張ポイントを通じて、既存コードを変更せずに機能拡張が可能です。

### 4. インターフェース分離の原則 (ISP)

各レイヤーは必要最小限のインターフェースのみを公開します。

### 5. イベント駆動設計

レイヤー間の疎結合を実現するため、イベント駆動アーキテクチャを採用しています。

## テスト戦略

### レイヤー別テスト

各レイヤーは独立してテスト可能です：

- **ユニットテスト**: 各レイヤー内の関数レベル
- **統合テスト**: レイヤー間通信
- **E2Eテスト**: Presentation → Infrastructure までの全フロー

### テスト実行

```elisp
;; 全テスト実行
(nskk-qa-run-all-tests)

;; 特定レイヤーのテスト
(nskk-qa-run-layer-tests 'application)

;; ベンチマーク実行
(nskk-qa-run-benchmarks)

;; パフォーマンス検証
(nskk-qa-verify-performance)
```

## デバッグ

### 全レイヤーのデバッグモード有効化

```elisp
(nskk-architecture-enable-debug)
```

### レイヤー間通信のトレース

```elisp
(setq nskk-architecture-enable-tracing t)
(nskk-architecture-get-communication-log)
```

### 個別レイヤーのデバッグ

```elisp
(nskk-presentation-enable-debug)
(nskk-application-enable-debug)
;; etc.
```

## パフォーマンス目標

| 処理 | 目標時間 | 関連レイヤー |
|------|---------|-------------|
| ローマ字変換 | < 0.1ms | Core |
| 辞書検索 | < 10ms | Data, Core |
| 候補表示 | < 0.5ms | Presentation |
| 学習処理 | < 5ms | Data |
| モード切り替え | < 1ms | Application |

## 拡張性

### プラグイン開発

Extension Layerの拡張ポイントを利用して、プラグインを開発できます：

```elisp
;; 変換前処理プラグイン
(nskk-extension-add-hook :before-conversion
  (lambda (input)
    ;; カスタム処理
    input))

;; 候補選択後処理プラグイン
(nskk-extension-add-hook :after-candidate-selection
  (lambda (candidate)
    ;; カスタム処理
    nil))
```

### カスタムレイヤー追加

新しいレイヤーを追加する場合は、`nskk-architecture.el`に登録します。

## まとめ

NSKK の7層アーキテクチャは：

- **保守性**: 各レイヤーの責務が明確で、変更の影響範囲が限定される
- **拡張性**: 300+の拡張ポイントにより、柔軟なカスタマイズが可能
- **テスタビリティ**: 各レイヤーが独立してテスト可能
- **パフォーマンス**: Infrastructure Layerでリソースを最適管理
- **品質保証**: QA Layerで継続的な品質検証

を実現しています。
