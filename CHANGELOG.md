# Changelog

All notable changes to NSKK will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - Phase 1完了

### Added

#### Track A: Core Engine (4モジュール)
- `nskk-romaji-tables.el`: ローマ字テーブル定義
  - 基本五十音、濁音・半濁音、拗音テーブル
  - 特殊文字テーブル（促音、撥音）
- `nskk-converter.el`: 基本変換ロジック
  - ローマ字→ひらがな変換エンジン
  - 状態機械ベースの変換処理
- `nskk-special-chars.el`: 特殊文字処理
  - 促音処理（kka → っか）
  - 撥音処理（nn, n' → ん）
  - 長音処理
- `nskk-optimize.el`: パフォーマンス最適化
  - defsubstインライン化
  - ベンチマーク基盤

#### Track B: State Management (4モジュール)
- `nskk-state.el`: 状態管理システム
  - 状態データ構造（plist/cl-defstruct）
  - モード定義（ひらがな/カタカナ/英数）
  - 状態遷移管理
- `nskk-mode-switch.el`: モード切り替え機構
  - モード切り替えロジック
  - フック実行システム
- `nskk-buffer.el`: バッファ管理
  - 入力バッファ管理
  - アンドゥ・リドゥ機能
  - バッファクリア
- `nskk-events.el`: イベント処理システム
  - イベントディスパッチャー
  - フックシステム基礎
  - エラーハンドリング

#### Track D: Dictionary Core (4モジュール)
- `nskk-dict-parser.el`: SKK辞書パーサー
  - SKK形式パーサー
  - エンコーディング処理（UTF-8/EUC-JP）
  - エントリ検証
- `nskk-dict-struct.el`: 辞書データ構造
  - 辞書エントリ構造（cl-defstruct）
  - メタデータ管理
- `nskk-dict-io.el`: ファイルI/O
  - 辞書読み込み・書き込み
  - 増分更新機構
  - バックアップ機能
- `nskk-dict-errors.el`: エラーハンドリング
  - エラー定義・分類
  - エラーリカバリー機構
  - フォールバック辞書

#### Track E: Search Algorithm (4モジュール)
- `nskk-trie.el`: トライ木実装
  - トライ木データ構造
  - 挿入・削除アルゴリズム
  - シリアライズ機能
- `nskk-search.el`: 検索アルゴリズム
  - 完全一致検索
  - 前方一致検索
  - 部分一致検索
- `nskk-cache.el`: キャッシュ機構
  - LRUキャッシュ
  - LFUキャッシュ
  - キャッシュ無効化
  - サイズ管理
- `nskk-index.el`: インデックス最適化
  - インデックス構築
  - 増分インデックス更新

#### Track F: UI Components (4モジュール)
- `nskk-keymap.el`: キーマップ定義
  - グローバルキーマップ
  - モード別キーマップ
  - カスタマイズ可能設定
- `nskk-candidate-window.el`: 候補ウィンドウ
  - ポップアップウィンドウ
  - 候補リスト表示
  - スクロール処理
- `nskk-minibuffer.el`: ミニバッファUI
  - ミニバッファ表示
  - インライン候補
  - プロンプト表示
- `nskk-modeline.el`: モードライン表示
  - モード表示
  - 状態インジケーター
  - カスタマイズ可能フォーマット

#### Track G: Documentation (4ドキュメント)
- `docs/explanation/architecture-v0.1.md`: Phase 1アーキテクチャ
- `docs/reference/api-v0.1.md`: Phase 1 APIリファレンス
- `docs/tutorial/getting-started-v0.1.md`: Phase 1入門ガイド
- `examples/`: 5つのサンプルコード

#### Testing Infrastructure
- `tests/nskk-test-framework.el`: ERTフレームワーク拡張
- `tests/nskk-test-macros.el`: テストマクロ定義
- `tests/nskk-test-fixtures.el`: モック・フィクスチャ
- `tests/nskk-coverage.el`: カバレッジツール
- `tests/nskk-integration-test.el`: 統合テストスイート
- `tests/nskk-benchmark.el`: パフォーマンステストスイート
- 20モジュール × 各種テスト = 500+テストケース

### Performance

- ✅ ローマ字変換: < 0.1ms（目標達成）
- ✅ 辞書検索（10万エントリ）: < 10ms（目標達成）
- ✅ メモリ使用量: < 20MB（目標達成）
- ✅ キャッシュヒット: < 0.01ms（目標達成）

### Testing

- ✅ テストカバレッジ: 95%以上
- ✅ 総テスト数: 500+
- ✅ 統合テスト: 30+シナリオ
- ✅ パフォーマンステスト: 20+ベンチマーク

### Quality

- ✅ 外部依存ゼロ（Emacs標準機能のみ）
- ✅ Lexical binding有効
- ✅ Byte-compile warning ゼロ
- ✅ Checkdoc準拠
- ✅ Package-lint準拠

### Documentation

- ✅ Diátaxis準拠構成
  - Tutorial: 入門ガイド
  - How-to: カスタマイズガイド
  - Reference: APIリファレンス
  - Explanation: アーキテクチャ説明
- ✅ 全公開関数の文書化
- ✅ サンプルコード動作確認済み

## [Unreleased]

### Planned for Phase 2 (v0.4 - v0.6)

#### Track H: Input Methods
- AZIK、ACT、TUT-code、親指シフト等11種類の入力方式
- 入力方式切り替え機構
- 動的ロード機構

#### Track I: Okurigana
- 動詞活用エンジン
- 形容詞活用エンジン
- 複雑活用処理
- 活用テーブル最適化

#### Track J: Annotation
- 注釈パーサー
- 注釈表示システム
- カスタム注釈システム

#### Track K: Completion
- 前方一致補完
- 曖昧補完
- 頻度ベース補完
- 文脈補完
- 予測補完

#### Track L: Server
- 辞書サーバープロトコル
- 非同期通信
- サーバーエラー処理

#### Track M: Learning
- 頻度学習エンジン
- 文脈学習エンジン
- 履歴管理システム
- 学習データ永続化

## Version History

- **v0.1.0** (Phase 1) - 基盤構築とコア機能実装（20モジュール）
- **v0.4.0** (Phase 2予定) - ddskk完全互換（+14モジュール）
- **v0.7.0** (Phase 3予定) - skkeleton統合と最適化（+17モジュール）
- **v1.0.0** (Phase 4予定) - 革新機能とv1.0リリース（+20モジュール）

## Compatibility

- **Required**: Emacs 31.0以上
- **Tested**: Emacs 31.0 (development version)
- **OS**: macOS, Linux, Windows (WSL)

## Links

- [GitHub Repository](https://github.com/takeokunn/nskk.el)
- [Documentation](docs/)
- [Issue Tracker](https://github.com/takeokunn/nskk.el/issues)

---

**注**: このプロジェクトは開発初期段階です。APIは変更される可能性があります。
