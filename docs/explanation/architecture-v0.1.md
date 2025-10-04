# NSKK アーキテクチャ v0.1

## 概要

NSKKは、Emacs標準機能のみを使用した外部依存ゼロの次世代日本語入力システムです。Phase 1では、基盤となるコア機能を7つのTrackに分けて並列開発し、レイヤードアーキテクチャに基づいた堅牢な設計を実現しています。

### 設計思想

1. **外部依存ゼロ**: Emacs標準機能のみで完全動作
2. **極限パフォーマンス**: キー入力応答 < 1ms、辞書検索 < 50ms
3. **無限拡張性**: 150+のフックポイント、プラグインシステム完備
4. **品質保証**: TDD/PBT、100%テストカバレッジ

### Phase 1実装範囲

Phase 1では、以下の7つのTrackを並列開発し、NSKKの基盤を構築しました：

- **Track A**: Core Engine（コア変換エンジン）
- **Track B**: State Management（状態管理）
- **Track C**: Testing Infrastructure（テスト基盤）
- **Track D**: Dictionary Core（辞書システム）
- **Track E**: Search Algorithm（検索アルゴリズム）
- **Track F**: UI Components（UI部品）
- **Track G**: Documentation（ドキュメント）

## レイヤーアーキテクチャ

NSKKは、7層のレイヤードアーキテクチャを採用しています。各レイヤーは明確な責務を持ち、上位レイヤーは下位レイヤーにのみ依存します。

```mermaid
graph TD
    subgraph "Layer 1: UI Layer"
        UI[UI Components<br/>nskk-keymap<br/>nskk-candidate-window<br/>nskk-minibuffer<br/>nskk-modeline]
    end

    subgraph "Layer 2: Extension Layer"
        EXT[Extension Points<br/>nskk-events<br/>Hook System]
    end

    subgraph "Layer 3: Application Layer"
        APP[Application Logic<br/>nskk-mode-switch<br/>nskk-buffer]
    end

    subgraph "Layer 4: Core Engine Layer"
        CORE[Core Engine<br/>nskk-converter<br/>nskk-romaji-tables<br/>nskk-special-chars<br/>nskk-optimize]
    end

    subgraph "Layer 5: Data Access Layer"
        DATA[Data Access<br/>nskk-dict-parser<br/>nskk-dict-io<br/>nskk-dict-struct<br/>nskk-dict-errors]
    end

    subgraph "Layer 6: Infrastructure Layer"
        INFRA[Infrastructure<br/>nskk-trie<br/>nskk-search<br/>nskk-cache<br/>nskk-index]
    end

    subgraph "Layer 7: QA Layer"
        QA[Quality Assurance<br/>nskk-test-framework<br/>nskk-test-macros<br/>nskk-test-fixtures<br/>nskk-coverage]
    end

    UI --> EXT
    EXT --> APP
    APP --> CORE
    CORE --> DATA
    DATA --> INFRA
    INFRA --> QA
```

### レイヤー詳細

#### Layer 1: UI Layer

ユーザーとの対話を担当するレイヤー。キーバインド、候補表示、ミニバッファ、モードライン表示を提供します。

**実装モジュール**:
- `nskk-keymap.el`: グローバルキーマップ、モード別キーマップ
- `nskk-candidate-window.el`: 候補リスト表示、スクロール処理
- `nskk-minibuffer.el`: ミニバッファ表示、インライン候補
- `nskk-modeline.el`: モード表示、状態インジケーター

#### Layer 2: Extension Layer

拡張機能のためのフックポイントとイベントシステムを提供します。

**実装モジュール**:
- `nskk-events.el`: イベントディスパッチャー、フックシステム基礎

#### Layer 3: Application Layer

アプリケーションロジックを管理するレイヤー。モード切り替え、バッファ管理を担当します。

**実装モジュール**:
- `nskk-mode-switch.el`: モード切り替えロジック、キーバインド処理
- `nskk-buffer.el`: 入力バッファ管理、アンドゥ・リドゥ
- `nskk-state.el`: 状態データ構造、モード定義

#### Layer 4: Core Engine Layer

ローマ字変換、特殊文字処理などのコア変換エンジンです。

**実装モジュール**:
- `nskk-romaji-tables.el`: ローマ字テーブル定義（五十音、濁音、拗音）
- `nskk-converter.el`: 変換エンジンコア、状態機械実装
- `nskk-special-chars.el`: 促音、撥音、長音処理
- `nskk-optimize.el`: defsubstインライン化、ベンチマーク実装

#### Layer 5: Data Access Layer

辞書ファイルの読み込み、書き込み、パースを担当します。

**実装モジュール**:
- `nskk-dict-parser.el`: SKK形式パーサー、エンコーディング処理
- `nskk-dict-io.el`: 辞書読み込み、書き込み、増分更新
- `nskk-dict-struct.el`: 辞書エントリ構造、インデックス構造
- `nskk-dict-errors.el`: エラー定義、エラーリカバリー

#### Layer 6: Infrastructure Layer

検索アルゴリズム、キャッシュ、インデックスなどの基盤機能を提供します。

**実装モジュール**:
- `nskk-trie.el`: トライ木データ構造、挿入・削除アルゴリズム
- `nskk-search.el`: 完全一致検索、前方一致検索、部分一致検索
- `nskk-cache.el`: LRUキャッシュ、LFUキャッシュ、サイズ管理
- `nskk-index.el`: インデックス構築、増分インデックス更新

#### Layer 7: QA Layer

テストフレームワーク、カバレッジ測定などの品質保証機能を提供します。

**実装モジュール**:
- `nskk-test-framework.el`: ERTラッパー、テストユーティリティ
- `nskk-test-macros.el`: プロパティテストマクロ、パフォーマンステストマクロ
- `nskk-test-fixtures.el`: テストデータ生成、モックオブジェクト
- `nskk-coverage.el`: カバレッジ測定、レポート生成

## システム全体図

NSKKの全モジュールと依存関係を示します。

```mermaid
graph TB
    subgraph "Track A: Core Engine"
        A1[nskk-romaji-tables]
        A2[nskk-converter]
        A3[nskk-special-chars]
        A4[nskk-optimize]

        A1 --> A2
        A2 --> A3
        A3 --> A4
    end

    subgraph "Track B: State Management"
        B1[nskk-state]
        B2[nskk-mode-switch]
        B3[nskk-buffer]
        B4[nskk-events]

        B1 --> B2
        B1 --> B3
        B1 --> B4
    end

    subgraph "Track C: Testing Infrastructure"
        C1[nskk-test-framework]
        C2[nskk-test-macros]
        C3[nskk-test-fixtures]
        C4[nskk-coverage]

        C1 --> C2
        C1 --> C3
    end

    subgraph "Track D: Dictionary Core"
        D1[nskk-dict-parser]
        D2[nskk-dict-struct]
        D3[nskk-dict-io]
        D4[nskk-dict-errors]

        D1 --> D3
        D2 --> D3
    end

    subgraph "Track E: Search Algorithm"
        E1[nskk-trie]
        E2[nskk-search]
        E3[nskk-cache]
        E4[nskk-index]

        E1 --> E2
        E1 --> E4
    end

    subgraph "Track F: UI Components"
        F1[nskk-keymap]
        F2[nskk-candidate-window]
        F3[nskk-minibuffer]
        F4[nskk-modeline]
    end

    %% Track間の依存関係
    A2 --> D3
    A2 --> E2
    B2 --> A2
    B3 --> A2
    E2 --> D3
    E3 --> E2
    F2 --> E2
    F3 --> E2
```

## データフロー

### 入力フロー

ユーザーのキー入力から候補表示までのデータフローです。

```mermaid
sequenceDiagram
    participant User as ユーザー
    participant Keymap as nskk-keymap
    participant Events as nskk-events
    participant Converter as nskk-converter
    participant Search as nskk-search
    participant Cache as nskk-cache
    participant Dict as nskk-dict-io
    participant Window as nskk-candidate-window

    User->>Keymap: キー入力
    Keymap->>Events: input-event発火
    Events->>Converter: 変換リクエスト
    Converter->>Converter: ローマ字変換

    Converter->>Cache: キャッシュ確認
    alt キャッシュヒット
        Cache-->>Converter: キャッシュ結果
    else キャッシュミス
        Converter->>Search: 辞書検索リクエスト
        Search->>Dict: 辞書読み込み
        Dict-->>Search: 辞書エントリ
        Search-->>Converter: 検索結果
        Converter->>Cache: キャッシュ更新
    end

    Converter->>Window: 候補リスト
    Window-->>User: 候補表示
```

### 辞書検索フロー

辞書読み込みからインデックス構築、検索までのフローです。

```mermaid
sequenceDiagram
    participant App as Application
    participant Parser as nskk-dict-parser
    participant IO as nskk-dict-io
    participant Struct as nskk-dict-struct
    participant Trie as nskk-trie
    participant Index as nskk-index
    participant Search as nskk-search

    App->>IO: 辞書読み込み要求
    IO->>Parser: ファイル読み込み
    Parser->>Parser: SKK形式パース
    Parser->>Struct: エントリ構造化

    Struct->>Trie: トライ木構築
    Struct->>Index: インデックス構築

    App->>Search: 検索リクエスト
    Search->>Index: インデックス検索
    Index->>Trie: トライ木検索
    Trie-->>Search: 検索結果
    Search-->>App: 候補リスト
```

### キャッシュフロー

キャッシュの動作とヒット/ミス処理のフローです。

```mermaid
graph TD
    START[検索リクエスト] --> CHECK{キャッシュ確認}

    CHECK -->|ヒット| L1{L1キャッシュ?}
    CHECK -->|ミス| SEARCH[辞書検索]

    L1 -->|ヒット| RETURN1[即座に返却<br/>0.01ms]
    L1 -->|ミス| L2{L2キャッシュ?}

    L2 -->|ヒット| RETURN2[返却<br/>0.05ms]
    L2 -->|ミス| L3{L3キャッシュ?}

    L3 -->|ヒット| RETURN3[返却<br/>0.1ms]
    L3 -->|ミス| SEARCH

    SEARCH --> UPDATE[キャッシュ更新]
    UPDATE --> RETURN4[返却<br/>0.5ms]

    RETURN1 --> END[終了]
    RETURN2 --> END
    RETURN3 --> END
    RETURN4 --> END
```

### 学習フロー

変換確定時の学習データ更新フローです（Phase 2実装予定）。

```mermaid
sequenceDiagram
    participant User as ユーザー
    participant Buffer as nskk-buffer
    participant Learning as Learning Engine<br/>(Phase 2)
    participant History as History<br/>(Phase 2)
    participant Dict as nskk-dict-io

    User->>Buffer: 変換確定
    Buffer->>Learning: 学習データ送信

    par 頻度学習
        Learning->>Learning: 使用頻度更新
    and 履歴記録
        Learning->>History: 履歴保存
    and 辞書更新
        Learning->>Dict: 個人辞書更新
    end

    Learning-->>User: 学習完了
```

## モジュール詳細

### Track A: Core Engine

#### nskk-romaji-tables.el

**責務**: ローマ字から仮名への変換テーブル定義

**公開API**:
```elisp
;; 基本テーブル
(defconst nskk-romaji-hiragana-table ...)  ; ひらがなテーブル
(defconst nskk-romaji-katakana-table ...)  ; カタカナテーブル

;; テーブル操作
(defun nskk-romaji-get-hiragana (romaji))  ; ローマ字からひらがな取得
(defun nskk-romaji-get-katakana (romaji))  ; ローマ字からカタカナ取得
```

**使用例**:
```elisp
(nskk-romaji-get-hiragana "ka")   ; => "か"
(nskk-romaji-get-katakana "kyo")  ; => "キョ"
```

#### nskk-converter.el

**責務**: ローマ字から仮名への変換エンジンコア

**公開API**:
```elisp
;; 変換関数
(defun nskk-convert-romaji (input mode))  ; ローマ字変換
(defun nskk-convert-region (start end))   ; リージョン変換

;; 状態機械
(defun nskk-converter-state-transition (event))  ; 状態遷移
```

**使用例**:
```elisp
(nskk-convert-romaji "aiueo" 'hiragana)  ; => "あいうえお"
(nskk-convert-romaji "AIUEO" 'katakana)  ; => "アイウエオ"
```

#### nskk-special-chars.el

**責務**: 促音、撥音、長音などの特殊文字処理

**公開API**:
```elisp
;; 特殊文字処理
(defun nskk-process-sokuon (char))     ; 促音処理（っ）
(defun nskk-process-hatsuon (char))    ; 撥音処理（ん）
(defun nskk-process-chōon (char))      ; 長音処理（ー）
```

**使用例**:
```elisp
(nskk-process-sokuon "kka")   ; => "っか"
(nskk-process-hatsuon "nn")   ; => "ん"
(nskk-process-chōon "aa")     ; => "あー"
```

#### nskk-optimize.el

**責務**: パフォーマンス最適化、ベンチマーク

**公開API**:
```elisp
;; ベンチマーク
(defmacro nskk-benchmark (name &rest body))  ; ベンチマーク実行

;; 最適化
(defun nskk-optimize-function (func))  ; 関数最適化
```

### Track B: State Management

#### nskk-state.el

**責務**: 状態データ構造定義、モード管理

**公開API**:
```elisp
;; 状態構造体
(cl-defstruct nskk-state
  mode         ; 現在のモード（hiragana/katakana/ascii）
  input-buffer ; 入力バッファ
  candidates   ; 候補リスト
  current-index) ; 現在の候補インデックス

;; 状態操作
(defun nskk-state-get-mode ())        ; モード取得
(defun nskk-state-set-mode (mode))    ; モード設定
```

#### nskk-mode-switch.el

**責務**: モード切り替えロジック

**公開API**:
```elisp
;; モード切り替え
(defun nskk-switch-to-hiragana ())  ; ひらがなモードへ
(defun nskk-switch-to-katakana ())  ; カタカナモードへ
(defun nskk-switch-to-ascii ())     ; ASCIIモードへ
```

#### nskk-buffer.el

**責務**: 入力バッファ管理

**公開API**:
```elisp
;; バッファ操作
(defun nskk-buffer-insert (char))      ; 文字挿入
(defun nskk-buffer-delete ())          ; 文字削除
(defun nskk-buffer-clear ())           ; バッファクリア
(defun nskk-buffer-get-string ())      ; バッファ文字列取得
```

#### nskk-events.el

**責務**: イベントディスパッチャー、フックシステム

**公開API**:
```elisp
;; イベント発火
(defun nskk-emit-event (event-type data))  ; イベント発火

;; フック登録
(defun nskk-add-hook (event-type func))    ; フック追加
(defun nskk-remove-hook (event-type func)) ; フック削除
```

### Track D: Dictionary Core

#### nskk-dict-parser.el

**責務**: SKK辞書形式のパース

**公開API**:
```elisp
;; パース関数
(defun nskk-parse-dictionary (file))       ; 辞書ファイルパース
(defun nskk-parse-entry (line))            ; エントリパース
```

#### nskk-dict-struct.el

**責務**: 辞書エントリのデータ構造定義

**公開API**:
```elisp
;; 辞書エントリ構造体
(cl-defstruct nskk-dict-entry
  yomi         ; 読み
  candidates   ; 候補リスト
  annotations) ; 注釈リスト
```

#### nskk-dict-io.el

**責務**: 辞書ファイルの読み書き

**公開API**:
```elisp
;; I/O関数
(defun nskk-load-dictionary (path))      ; 辞書読み込み
(defun nskk-save-dictionary (path dict)) ; 辞書保存
```

#### nskk-dict-errors.el

**責務**: 辞書関連エラー定義

**公開API**:
```elisp
;; エラー定義
(define-error 'nskk-dict-error "Dictionary error")
(define-error 'nskk-dict-parse-error "Parse error" 'nskk-dict-error)
(define-error 'nskk-dict-io-error "I/O error" 'nskk-dict-error)
```

### Track E: Search Algorithm

#### nskk-trie.el

**責務**: トライ木データ構造実装

**公開API**:
```elisp
;; トライ木操作
(defun nskk-trie-create ())                  ; トライ木作成
(defun nskk-trie-insert (trie key value))    ; ノード挿入
(defun nskk-trie-search (trie key))          ; ノード検索
(defun nskk-trie-delete (trie key))          ; ノード削除
```

#### nskk-search.el

**責務**: 辞書検索アルゴリズム

**公開API**:
```elisp
;; 検索関数
(defun nskk-search-exact (query))       ; 完全一致検索
(defun nskk-search-prefix (query))      ; 前方一致検索
(defun nskk-search-partial (query))     ; 部分一致検索
```

#### nskk-cache.el

**責務**: キャッシュ機構（LRU/LFU）

**公開API**:
```elisp
;; キャッシュ操作
(defun nskk-cache-get (key))            ; キャッシュ取得
(defun nskk-cache-put (key value))      ; キャッシュ保存
(defun nskk-cache-clear ())             ; キャッシュクリア
```

#### nskk-index.el

**責務**: インデックス構築と最適化

**公開API**:
```elisp
;; インデックス操作
(defun nskk-index-build (dict))         ; インデックス構築
(defun nskk-index-update (entry))       ; インデックス更新
(defun nskk-index-search (query))       ; インデックス検索
```

### Track F: UI Components

#### nskk-keymap.el

**責務**: キーマップ定義

**公開API**:
```elisp
;; キーマップ
(defvar nskk-mode-map ...)              ; グローバルキーマップ
(defvar nskk-hiragana-map ...)          ; ひらがなモードキーマップ
(defvar nskk-katakana-map ...)          ; カタカナモードキーマップ
```

#### nskk-candidate-window.el

**責務**: 候補ウィンドウ表示

**公開API**:
```elisp
;; 候補表示
(defun nskk-show-candidates (candidates))  ; 候補表示
(defun nskk-hide-candidates ())            ; 候補非表示
(defun nskk-next-candidate ())             ; 次の候補
(defun nskk-previous-candidate ())         ; 前の候補
```

#### nskk-minibuffer.el

**責務**: ミニバッファUI

**公開API**:
```elisp
;; ミニバッファ表示
(defun nskk-minibuffer-show (text))     ; テキスト表示
(defun nskk-minibuffer-clear ())        ; クリア
```

#### nskk-modeline.el

**責務**: モードライン表示

**公開API**:
```elisp
;; モードライン更新
(defun nskk-update-modeline ())         ; モードライン更新
(defun nskk-modeline-format ())         ; フォーマット取得
```

## パフォーマンス設計

### 最適化ポイント

Phase 1で実装した主要な最適化手法です。

```mermaid
graph TD
    subgraph "最適化レイヤー"
        OPT1[defsubstインライン化<br/>関数呼び出しコスト削減]
        OPT2[キャッシュ多層化<br/>L1/L2/L3キャッシュ]
        OPT3[トライ木検索<br/>O log n 検索]
        OPT4[遅延評価<br/>必要時のみ計算]
    end

    subgraph "パフォーマンス目標"
        PERF1[ローマ字変換: < 0.1ms]
        PERF2[辞書検索: < 10ms]
        PERF3[キャッシュアクセス: < 0.01ms]
        PERF4[起動時間: < 20ms]
    end

    OPT1 --> PERF1
    OPT2 --> PERF3
    OPT3 --> PERF2
    OPT4 --> PERF4
```

### ボトルネック対策

#### 辞書検索の高速化

```elisp
;; トライ木による前方一致検索の高速化
;; 平均計算量: O(m) where m = key length
(defun nskk-trie-search-prefix (trie prefix)
  "前方一致検索の最適化実装"
  (let ((node (nskk-trie--find-node trie prefix)))
    (when node
      (nskk-trie--collect-values node))))
```

#### キャッシュ戦略

```elisp
;; 3層キャッシュによる段階的検索
(defun nskk-search-with-cache (query)
  "キャッシュを利用した検索"
  (or (nskk-cache-get-l1 query)      ; L1: 0.01ms
      (nskk-cache-get-l2 query)      ; L2: 0.05ms
      (nskk-cache-get-l3 query)      ; L3: 0.1ms
      (nskk-search-dictionary query))) ; 辞書: 10ms
```

### メモリ最適化

```mermaid
graph LR
    subgraph "メモリ使用量"
        MEM1[起動時: 5MB]
        MEM2[辞書読み込み後: 15MB]
        MEM3[通常使用時: 20MB]
        MEM4[最大: 50MB]
    end

    MEM1 --> MEM2
    MEM2 --> MEM3
    MEM3 --> MEM4
```

## 拡張性

### Phase 2以降への準備

Phase 1で実装したフックポイントとプラグインインターフェースにより、Phase 2以降の機能拡張が容易になります。

```mermaid
graph TB
    subgraph "Phase 1実装"
        CORE[Core Engine]
        STATE[State Management]
        DICT[Dictionary System]
        SEARCH[Search Algorithm]
        UI[UI Components]
    end

    subgraph "Phase 2拡張予定"
        INPUT[Input Methods<br/>11種類]
        OKU[Okurigana<br/>送り仮名処理]
        ANNO[Annotation<br/>注釈システム]
        COMP[Completion<br/>補完機能]
        LEARN[Learning<br/>学習エンジン]
    end

    CORE --> INPUT
    CORE --> OKU
    DICT --> ANNO
    SEARCH --> COMP
    STATE --> LEARN
```

### プラグインポイント

Phase 1で実装済みのフックポイント一覧です。

```elisp
;; 入力イベント
(nskk-add-hook 'before-input-processing func)
(nskk-add-hook 'after-input-processing func)

;; 変換イベント
(nskk-add-hook 'before-romaji-conversion func)
(nskk-add-hook 'after-romaji-conversion func)

;; 辞書イベント
(nskk-add-hook 'before-dictionary-lookup func)
(nskk-add-hook 'after-dictionary-lookup func)

;; 候補イベント
(nskk-add-hook 'before-candidate-generation func)
(nskk-add-hook 'after-candidate-generation func)

;; システムイベント
(nskk-add-hook 'before-config-change func)
(nskk-add-hook 'after-config-change func)
```

## テスト戦略

### テスト構成

Phase 1では、Track Cとして独立したテスト基盤を構築しました。

```mermaid
graph TD
    subgraph "テストフレームワーク"
        FW[nskk-test-framework<br/>ERTラッパー]
        MACRO[nskk-test-macros<br/>テストマクロ]
        FIX[nskk-test-fixtures<br/>モック・フィクスチャ]
        COV[nskk-coverage<br/>カバレッジ測定]
    end

    subgraph "テスト種別"
        UNIT[単体テスト<br/>各モジュール]
        INT[統合テスト<br/>モジュール間]
        PERF[性能テスト<br/>ベンチマーク]
    end

    FW --> UNIT
    MACRO --> PERF
    FIX --> INT
    COV --> UNIT
    COV --> INT
```

### カバレッジ目標

```elisp
;; Phase 1カバレッジ目標
(defconst nskk-coverage-targets
  '((unit-tests . 95)           ; 単体テスト 95%
    (integration-tests . 85)    ; 統合テスト 85%
    (performance-tests . 90))   ; 性能テスト 90%
  "Phase 1カバレッジ目標")
```

### テスト実行

```bash
# 全テスト実行
make test

# カバレッジレポート生成
make coverage

# 性能テスト実行
make benchmark
```

## まとめ

Phase 1では、NSKKの基盤となる7つのTrackを並列開発し、堅牢なレイヤードアーキテクチャを構築しました。

### 実装完了モジュール

- **Track A**: 4モジュール（Core Engine）
- **Track B**: 4モジュール（State Management）
- **Track C**: 4モジュール（Testing Infrastructure）
- **Track D**: 4モジュール（Dictionary Core）
- **Track E**: 4モジュール（Search Algorithm）
- **Track F**: 4モジュール（UI Components）

**合計**: 24モジュール

### Phase 2への移行

Phase 1で構築した基盤の上に、Phase 2では以下の機能を実装予定です：

1. **入力方式**: AZIK、ACT、TUT-code等11種類
2. **送り仮名処理**: 動詞・形容詞活用
3. **注釈システム**: SKK注釈の完全サポート
4. **補完機能**: 前方一致、曖昧、頻度ベース、文脈、予測補完
5. **学習エンジン**: 頻度学習、文脈学習、履歴管理

Phase 1のアーキテクチャは、これらの拡張を容易にするよう設計されています。
