# NSKK API Reference v0.1

Phase 1完了モジュールのAPIリファレンス

## 目次

- [Core Engine (Track A)](#core-engine-track-a)
  - [nskk-romaji-tables](#nskk-romaji-tables)
  - [nskk-converter](#nskk-converter)
  - [nskk-special-chars](#nskk-special-chars)
  - [nskk-optimize](#nskk-optimize)
- [State Management (Track B)](#state-management-track-b)
  - [nskk-state](#nskk-state)
  - [nskk-mode-switch](#nskk-mode-switch)
  - [nskk-buffer](#nskk-buffer)
  - [nskk-events](#nskk-events)
- [Dictionary Core (Track D)](#dictionary-core-track-d)
  - [nskk-dict-parser](#nskk-dict-parser)
  - [nskk-dict-struct](#nskk-dict-struct)
  - [nskk-dict-io](#nskk-dict-io)
  - [nskk-dict-errors](#nskk-dict-errors)
- [Search Algorithm (Track E)](#search-algorithm-track-e)
  - [nskk-trie](#nskk-trie)
  - [nskk-search](#nskk-search)
  - [nskk-cache](#nskk-cache)
  - [nskk-index](#nskk-index)
- [UI Components (Track F)](#ui-components-track-f)
  - [nskk-keymap](#nskk-keymap)
  - [nskk-candidate-window](#nskk-candidate-window)
  - [nskk-minibuffer](#nskk-minibuffer)
  - [nskk-modeline](#nskk-modeline)

---

## Core Engine (Track A)

### nskk-romaji-tables

ローマ字からひらがなへの変換テーブル定義

#### 定数

##### `nskk-romaji-table`

430+エントリのローマ字→ひらがな変換テーブル

```elisp
;; 基本的な変換例
("ka" . "か")
("ki" . "き")
("kya" . "きゃ")
("kk" . (sokuon . "k"))
```

##### `nskk-romaji-nn-conversion-table`

「n」の特殊処理テーブル

```elisp
("n'" . "ん")
("nn" . "ん")
```

##### `nskk-romaji-special-sequences`

特殊シーケンステーブル（促音、撥音処理用）

#### 関数

##### `nskk-romaji-lookup`

```elisp
(nskk-romaji-lookup ROMAJI)
```

ローマ字文字列をひらがなに変換

**引数:**
- `ROMAJI` (string): 変換するローマ字文字列

**戻り値:**
- ひらがな文字列、または変換できない場合は nil

**使用例:**

```elisp
(nskk-romaji-lookup "ka")   ;; => "か"
(nskk-romaji-lookup "kya")  ;; => "きゃ"
(nskk-romaji-lookup "xyz")  ;; => nil
```

##### `nskk-romaji-get-candidates`

```elisp
(nskk-romaji-get-candidates PREFIX)
```

指定されたプレフィックスで始まる可能な変換候補を取得

**引数:**
- `PREFIX` (string): 検索するプレフィックス

**戻り値:**
- 候補のリスト

**使用例:**

```elisp
(nskk-romaji-get-candidates "k")
;; => ("ka" "ki" "ku" "ke" "ko" "kya" "kyu" "kyo" ...)
```

##### `nskk-romaji-init-hash-table`

```elisp
(nskk-romaji-init-hash-table)
```

高速検索用のハッシュテーブルを初期化

**戻り値:**
- 初期化されたハッシュテーブル

---

### nskk-converter

ローマ字からかな変換のコアエンジン

#### データ構造

##### `nskk-converter-result`

```elisp
(cl-defstruct nskk-converter-result
  converted   ;; 変換済み文字列
  pending     ;; 未変換の残り文字列
  consumed)   ;; 消費された文字数
```

#### カスタマイズ変数

##### `nskk-converter-use-sokuon`

```elisp
(defcustom nskk-converter-use-sokuon t
  "促音(っ)の自動変換を有効にするか"
  :type 'boolean
  :group 'nskk-converter)
```

**デフォルト:** `t`

##### `nskk-converter-n-processing-mode`

```elisp
(defcustom nskk-converter-n-processing-mode 'smart
  "「n」の処理モード"
  :type '(choice (const :tag "Smart (auto)" smart)
                 (const :tag "Strict (nn required)" strict)
                 (const :tag "Loose (single n ok)" loose))
  :group 'nskk-converter)
```

**デフォルト:** `'smart`

**オプション:**
- `'smart`: 文脈に応じた自動判定
- `'strict`: 「nn」必須
- `'loose`: 単一「n」でも変換

##### `nskk-converter-auto-start-henkan`

```elisp
(defcustom nskk-converter-auto-start-henkan t
  "大文字入力時に自動的に変換を開始するか"
  :type 'boolean
  :group 'nskk-converter)
```

**デフォルト:** `t`

#### 関数

##### `nskk-convert-romaji`

```elisp
(nskk-convert-romaji INPUT &optional CONTEXT)
```

ローマ字文字列をかなに変換（メイン関数）

**引数:**
- `INPUT` (string): 変換するローマ字文字列
- `CONTEXT` (optional): 変換コンテキスト

**戻り値:**
- `nskk-converter-result` 構造体

**使用例:**

```elisp
(nskk-convert-romaji "konnnitiha")
;; => #s(nskk-converter-result
;;       :converted "こんにちは"
;;       :pending ""
;;       :consumed 10)

(nskk-convert-romaji "k")
;; => #s(nskk-converter-result
;;       :converted ""
;;       :pending "k"
;;       :consumed 0)
```

##### `nskk-convert-romaji-simple`

```elisp
(nskk-convert-romaji-simple INPUT)
```

簡易版変換関数（文字列のみ返す）

**引数:**
- `INPUT` (string): 変換するローマ字文字列

**戻り値:**
- 変換されたひらがな文字列

**使用例:**

```elisp
(nskk-convert-romaji-simple "aiueo")  ;; => "あいうえお"
```

##### `nskk-converter-reset-context`

```elisp
(nskk-converter-reset-context CONTEXT)
```

変換コンテキストをリセット

---

### nskk-special-chars

ひらがな・カタカナ変換、特殊文字処理

#### 定数

##### `nskk-special-chars--hiragana-katakana-offset`

```elisp
(defconst nskk-special-chars--hiragana-katakana-offset 96
  "ひらがなとカタカナのUnicodeオフセット差")
```

#### 関数

##### `nskk-hiragana-to-katakana`

```elisp
(nskk-hiragana-to-katakana STR)
```

ひらがな文字列をカタカナに変換

**引数:**
- `STR` (string): ひらがな文字列

**戻り値:**
- カタカナ文字列

**使用例:**

```elisp
(nskk-hiragana-to-katakana "あいうえお")  ;; => "アイウエオ"
(nskk-hiragana-to-katakana "きゃきゅきょ")  ;; => "キャキュキョ"
```

##### `nskk-katakana-to-hiragana`

```elisp
(nskk-katakana-to-hiragana STR)
```

カタカナ文字列をひらがなに変換

**引数:**
- `STR` (string): カタカナ文字列

**戻り値:**
- ひらがな文字列

**使用例:**

```elisp
(nskk-katakana-to-hiragana "カタカナ")  ;; => "かたかな"
```

##### `nskk-is-hiragana-char-p`

```elisp
(nskk-is-hiragana-char-p CHAR)
```

文字がひらがなかどうかを判定

**引数:**
- `CHAR` (character): 判定する文字

**戻り値:**
- ひらがなの場合 t、それ以外 nil

##### `nskk-is-katakana-char-p`

```elisp
(nskk-is-katakana-char-p CHAR)
```

文字がカタカナかどうかを判定

##### `nskk-is-uppercase-input`

```elisp
(nskk-is-uppercase-input CHAR)
```

入力文字が大文字かどうかを判定（変換開始判定用）

**引数:**
- `CHAR` (character): 判定する文字

**戻り値:**
- 大文字の場合 t、それ以外 nil

---

### nskk-optimize

パフォーマンス計測・最適化ツール

#### カスタマイズ変数

##### `nskk-optimize-enable-profiling`

```elisp
(defcustom nskk-optimize-enable-profiling nil
  "プロファイリングを有効にするか"
  :type 'boolean
  :group 'nskk-optimize)
```

**デフォルト:** `nil`

##### `nskk-optimize-benchmark-iterations`

```elisp
(defcustom nskk-optimize-benchmark-iterations 10000
  "ベンチマーク実行回数"
  :type 'integer
  :group 'nskk-optimize)
```

**デフォルト:** `10000`

#### 関数

##### `nskk-benchmark-romaji-conversion`

```elisp
(nskk-benchmark-romaji-conversion &optional ITERATIONS)
```

ローマ字変換のベンチマーク実行

**引数:**
- `ITERATIONS` (optional integer): 実行回数（デフォルト: 10000）

**戻り値:**
- ベンチマーク結果のalist

**使用例:**

```elisp
(nskk-benchmark-romaji-conversion 1000)
;; => ((average . 0.000085)
;;     (total . 0.085)
;;     (iterations . 1000)
;;     (target-met . t))
```

##### `nskk-measure-time`

```elisp
(nskk-measure-time &rest BODY)
```

コードブロックの実行時間を計測（マクロ）

**引数:**
- `BODY`: 計測するコード

**戻り値:**
- (結果 . 実行時間) のコンスセル

**使用例:**

```elisp
(nskk-measure-time
  (nskk-convert-romaji "konnnitiha"))
;; => (#s(nskk-converter-result ...) . 0.000095)
```

##### `nskk-benchmark-suite`

```elisp
(nskk-benchmark-suite)
```

全モジュールの総合ベンチマーク実行

**戻り値:**
- 全モジュールのベンチマーク結果

---

## State Management (Track B)

### nskk-state

状態管理のコアデータ構造

#### データ構造

##### `nskk-state`

```elisp
(cl-defstruct nskk-state
  mode              ;; 現在のモード (symbol)
  input-buffer      ;; 入力バッファ (string)
  converted-buffer  ;; 変換済みバッファ (string)
  candidates        ;; 変換候補リスト (list)
  current-index     ;; 現在の候補インデックス (integer)
  henkan-position   ;; 変換開始位置 (integer or nil)
  marker-position   ;; カーソル位置マーカー (marker or nil)
  previous-mode     ;; 直前のモード (symbol)
  undo-stack        ;; undo用スタック (list)
  redo-stack        ;; redo用スタック (list)
  metadata)         ;; メタデータ (plist)
```

#### 定数

##### `nskk-state-modes`

```elisp
(defconst nskk-state-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin)
  "利用可能なモード一覧")
```

#### 関数

##### `nskk-state-create`

```elisp
(nskk-state-create &optional INITIAL-MODE)
```

新しい状態オブジェクトを作成

**引数:**
- `INITIAL-MODE` (optional symbol): 初期モード（デフォルト: `'ascii`）

**戻り値:**
- `nskk-state` 構造体

**使用例:**

```elisp
(setq my-state (nskk-state-create 'hiragana))
```

##### `nskk-state-transition`

```elisp
(nskk-state-transition STATE FROM-MODE TO-MODE)
```

状態遷移を実行

**引数:**
- `STATE` (nskk-state): 状態オブジェクト
- `FROM-MODE` (symbol): 遷移元モード
- `TO-MODE` (symbol): 遷移先モード

**戻り値:**
- 遷移が成功した場合 t、失敗した場合 nil

##### `nskk-state-set-mode`

```elisp
(nskk-state-set-mode STATE MODE)
```

モードを設定

**引数:**
- `STATE` (nskk-state): 状態オブジェクト
- `MODE` (symbol): 設定するモード

##### `nskk-state-valid-mode-p`

```elisp
(nskk-state-valid-mode-p MODE)
```

モードが有効かどうかを判定

**引数:**
- `MODE` (symbol): 判定するモード

**戻り値:**
- 有効な場合 t、無効な場合 nil

##### `nskk-state-reset`

```elisp
(nskk-state-reset STATE)
```

状態をリセット（モードは保持）

---

### nskk-mode-switch

モード切り替えロジック

#### カスタマイズ変数

##### `nskk-mode-switch-hook`

```elisp
(defcustom nskk-mode-switch-hook nil
  "モード切り替え時に実行されるフック"
  :type 'hook
  :group 'nskk-mode-switch)
```

##### `nskk-mode-switch-show-message`

```elisp
(defcustom nskk-mode-switch-show-message t
  "モード切り替え時にメッセージを表示するか"
  :type 'boolean
  :group 'nskk-mode-switch)
```

**デフォルト:** `t`

#### 関数

##### `nskk-mode-switch`

```elisp
(nskk-mode-switch STATE NEW-MODE)
```

指定されたモードに切り替え

**引数:**
- `STATE` (nskk-state): 状態オブジェクト
- `NEW-MODE` (symbol): 切り替え先モード

**戻り値:**
- 成功時 t、失敗時 nil

**使用例:**

```elisp
(nskk-mode-switch my-state 'katakana)
```

##### `nskk-mode-toggle-kana`

```elisp
(nskk-mode-toggle-kana STATE)
```

ひらがな/カタカナをトグル

**引数:**
- `STATE` (nskk-state): 状態オブジェクト

##### `nskk-mode-cycle-forward`

```elisp
(nskk-mode-cycle-forward STATE)
```

次のモードに循環的に切り替え

**使用例:**

```elisp
;; ascii -> hiragana -> katakana -> ascii -> ...
(nskk-mode-cycle-forward my-state)
```

##### `nskk-mode-cycle-backward`

```elisp
(nskk-mode-cycle-backward STATE)
```

前のモードに循環的に切り替え

##### `nskk-mode-switch-before-hook`

モード切り替え前に実行されるフック

##### `nskk-mode-switch-after-hook`

モード切り替え後に実行されるフック

---

### nskk-buffer

バッファ操作とundo/redo管理

#### カスタマイズ変数

##### `nskk-buffer-undo-limit`

```elisp
(defcustom nskk-buffer-undo-limit 100
  "undoスタックの最大サイズ"
  :type 'integer
  :group 'nskk-buffer)
```

**デフォルト:** `100`

##### `nskk-buffer-auto-commit`

```elisp
(defcustom nskk-buffer-auto-commit t
  "確定時に自動的にバッファにコミットするか"
  :type 'boolean
  :group 'nskk-buffer)
```

**デフォルト:** `t`

#### 関数

##### `nskk-buffer-insert`

```elisp
(nskk-buffer-insert STATE TEXT)
```

入力バッファにテキストを挿入

**引数:**
- `STATE` (nskk-state): 状態オブジェクト
- `TEXT` (string): 挿入するテキスト

**使用例:**

```elisp
(nskk-buffer-insert my-state "あ")
```

##### `nskk-buffer-delete-backward-char`

```elisp
(nskk-buffer-delete-backward-char STATE &optional N)
```

入力バッファから後方N文字削除

**引数:**
- `STATE` (nskk-state): 状態オブジェクト
- `N` (optional integer): 削除する文字数（デフォルト: 1）

##### `nskk-buffer-clear`

```elisp
(nskk-buffer-clear STATE)
```

入力バッファをクリア

##### `nskk-buffer-commit`

```elisp
(nskk-buffer-commit STATE)
```

入力バッファの内容を確定してEmacsバッファに挿入

**引数:**
- `STATE` (nskk-state): 状態オブジェクト

##### `nskk-buffer-undo`

```elisp
(nskk-buffer-undo STATE)
```

直前の操作をundo

**戻り値:**
- undo成功時 t、スタックが空の場合 nil

##### `nskk-buffer-redo`

```elisp
(nskk-buffer-redo STATE)
```

undoした操作をredo

**戻り値:**
- redo成功時 t、スタックが空の場合 nil

##### `nskk-buffer-save-snapshot`

```elisp
(nskk-buffer-save-snapshot STATE)
```

現在の状態をundoスタックに保存

---

### nskk-events

イベント駆動アーキテクチャ

#### 定数

##### `nskk-events-types`

```elisp
(defconst nskk-events-types
  '(:state-changed
    :mode-switched
    :input-received
    :conversion-started
    :candidate-selected
    :committed
    :error
    :dict-loaded
    :cache-updated)
  "定義済みイベント型一覧")
```

#### 関数

##### `nskk-events-add-listener`

```elisp
(nskk-events-add-listener EVENT-TYPE CALLBACK &optional PRIORITY)
```

イベントリスナーを登録

**引数:**
- `EVENT-TYPE` (keyword): イベント型（例: `:state-changed`）
- `CALLBACK` (function): コールバック関数
- `PRIORITY` (optional integer): 優先度（デフォルト: 0、大きいほど優先）

**戻り値:**
- リスナーID（削除時に使用）

**使用例:**

```elisp
(nskk-events-add-listener
  :state-changed
  (lambda (event-data)
    (message "State changed: %s -> %s"
             (plist-get event-data :from)
             (plist-get event-data :to))))
```

##### `nskk-events-remove-listener`

```elisp
(nskk-events-remove-listener LISTENER-ID)
```

イベントリスナーを削除

**引数:**
- `LISTENER-ID`: `nskk-events-add-listener` が返したID

##### `nskk-events-emit`

```elisp
(nskk-events-emit EVENT-TYPE &rest DATA)
```

イベントを発行

**引数:**
- `EVENT-TYPE` (keyword): イベント型
- `DATA` (plist): イベントデータ

**使用例:**

```elisp
(nskk-events-emit :mode-switched
                  :from 'ascii
                  :to 'hiragana
                  :timestamp (current-time))
```

##### `nskk-events-clear-listeners`

```elisp
(nskk-events-clear-listeners &optional EVENT-TYPE)
```

リスナーをクリア

**引数:**
- `EVENT-TYPE` (optional keyword): 指定した場合、その型のリスナーのみクリア

---

## Dictionary Core (Track D)

### nskk-dict-parser

SKK辞書ファイルのパーサー

#### カスタマイズ変数

##### `nskk-dict-parser-buffer-size`

```elisp
(defcustom nskk-dict-parser-buffer-size 8192
  "パース用バッファサイズ（バイト）"
  :type 'integer
  :group 'nskk-dict-parser)
```

**デフォルト:** `8192`

##### `nskk-dict-parser-encoding-priority`

```elisp
(defcustom nskk-dict-parser-encoding-priority '(utf-8 euc-jp)
  "エンコーディング検出の優先順位"
  :type '(repeat coding-system)
  :group 'nskk-dict-parser)
```

**デフォルト:** `'(utf-8 euc-jp)`

#### 関数

##### `nskk-parse-dictionary`

```elisp
(nskk-parse-dictionary FILE-PATH &optional ENCODING)
```

SKK辞書ファイルをパース

**引数:**
- `FILE-PATH` (string): 辞書ファイルパス
- `ENCODING` (optional coding-system): エンコーディング（省略時は自動検出）

**戻り値:**
- パース結果のalist
  - `:entries` - エントリリスト
  - `:metadata` - メタデータ（エンコーディング、エントリ数等）
  - `:okuri-ari` - 送り仮名ありエントリ
  - `:okuri-nasi` - 送り仮名なしエントリ

**使用例:**

```elisp
(nskk-parse-dictionary "~/.skk/SKK-JISYO.L")
;; => ((:entries . [...])
;;     (:metadata . (:encoding utf-8 :total 500000))
;;     (:okuri-ari . [...])
;;     (:okuri-nasi . [...]))
```

##### `nskk-parse-entry`

```elisp
(nskk-parse-entry LINE)
```

1行のエントリをパース

**引数:**
- `LINE` (string): エントリ行（例: "あい /愛/藍/"）

**戻り値:**
- (見出し . 候補リスト) のコンスセル

**使用例:**

```elisp
(nskk-parse-entry "あい /愛/藍/")
;; => ("あい" . ("愛" "藝"))
```

##### `nskk-dict-parser-detect-encoding`

```elisp
(nskk-dict-parser-detect-encoding FILE-PATH)
```

辞書ファイルのエンコーディングを検出

**引数:**
- `FILE-PATH` (string): 辞書ファイルパス

**戻り値:**
- 検出されたエンコーディング（coding-system）

##### `nskk-dict-parser-validate-entry`

```elisp
(nskk-dict-parser-validate-entry ENTRY)
```

エントリの妥当性を検証

**引数:**
- `ENTRY`: パースされたエントリ

**戻り値:**
- 有効な場合 t、無効な場合 nil

---

### nskk-dict-struct

最適化された辞書データ構造

#### データ構造

##### `nskk-dict-index`

```elisp
(cl-defstruct nskk-dict-index
  entries-count      ;; エントリ総数
  trie              ;; 検索用Trie構造
  hash-table        ;; 完全一致検索用ハッシュテーブル
  metadata          ;; メタデータ
  okuri-ari-index   ;; 送り仮名ありインデックス
  okuri-nasi-index) ;; 送り仮名なしインデックス
```

##### `nskk-dict-entry`

```elisp
(cl-defstruct nskk-dict-entry
  key              ;; 見出し語
  candidates       ;; 候補リスト
  okuri-type       ;; 送り仮名タイプ (:ari/:nasi/:none)
  frequency)       ;; 使用頻度
```

##### `nskk-dict-candidate`

```elisp
(cl-defstruct nskk-dict-candidate
  text             ;; 候補テキスト
  annotation       ;; アノテーション
  frequency)       ;; 使用頻度
```

#### 関数

##### `nskk-dict-struct-from-parser`

```elisp
(nskk-dict-struct-from-parser PARSED-DATA)
```

パーサー出力から最適化された構造を生成

**引数:**
- `PARSED-DATA`: `nskk-parse-dictionary` の出力

**戻り値:**
- `nskk-dict-index` 構造体

**使用例:**

```elisp
(setq parsed (nskk-parse-dictionary "~/.skk/SKK-JISYO.L"))
(setq index (nskk-dict-struct-from-parser parsed))
```

##### `nskk-dict-struct-lookup`

```elisp
(nskk-dict-struct-lookup INDEX KEY)
```

完全一致検索

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `KEY` (string): 検索キー

**戻り値:**
- `nskk-dict-entry` または nil

##### `nskk-dict-struct-prefix-search`

```elisp
(nskk-dict-struct-prefix-search INDEX PREFIX &optional LIMIT)
```

前方一致検索

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `PREFIX` (string): 検索プレフィックス
- `LIMIT` (optional integer): 最大結果数

**戻り値:**
- `nskk-dict-entry` のリスト

##### `nskk-dict-struct-add-entry`

```elisp
(nskk-dict-struct-add-entry INDEX ENTRY)
```

新しいエントリを追加

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `ENTRY` (nskk-dict-entry): 追加するエントリ

---

### nskk-dict-io

辞書ファイルのI/O操作

#### カスタマイズ変数

##### `nskk-dict-io-cache-enabled`

```elisp
(defcustom nskk-dict-io-cache-enabled t
  "辞書キャッシュを有効にするか"
  :type 'boolean
  :group 'nskk-dict-io)
```

**デフォルト:** `t`

##### `nskk-dict-io-auto-backup`

```elisp
(defcustom nskk-dict-io-auto-backup t
  "保存時に自動バックアップを作成するか"
  :type 'boolean
  :group 'nskk-dict-io)
```

**デフォルト:** `t`

##### `nskk-dict-io-backup-directory`

```elisp
(defcustom nskk-dict-io-backup-directory "~/.skk/backup"
  "バックアップディレクトリ"
  :type 'directory
  :group 'nskk-dict-io)
```

**デフォルト:** `"~/.skk/backup"`

#### 関数

##### `nskk-load-dictionary`

```elisp
(nskk-load-dictionary FILE-PATH &optional USE-CACHE)
```

辞書を読み込み

**引数:**
- `FILE-PATH` (string): 辞書ファイルパス
- `USE-CACHE` (optional boolean): キャッシュ使用（デフォルト: t）

**戻り値:**
- `nskk-dict-index` 構造体

**使用例:**

```elisp
(setq my-dict (nskk-load-dictionary "~/.skk/SKK-JISYO.L"))
```

##### `nskk-save-dictionary`

```elisp
(nskk-save-dictionary INDEX FILE-PATH &optional ENCODING)
```

辞書を保存

**引数:**
- `INDEX` (nskk-dict-index): 保存する辞書インデックス
- `FILE-PATH` (string): 保存先パス
- `ENCODING` (optional coding-system): エンコーディング（デフォルト: utf-8）

**戻り値:**
- 成功時 t、失敗時 nil

##### `nskk-backup-dictionary`

```elisp
(nskk-backup-dictionary FILE-PATH)
```

辞書ファイルをバックアップ

**引数:**
- `FILE-PATH` (string): バックアップ対象ファイル

**戻り値:**
- バックアップファイルパス

##### `nskk-dict-io-cache-invalidate`

```elisp
(nskk-dict-io-cache-invalidate FILE-PATH)
```

指定された辞書のキャッシュを無効化

##### `nskk-dict-io-verify-checksum`

```elisp
(nskk-dict-io-verify-checksum FILE-PATH)
```

辞書ファイルのチェックサム検証

**戻り値:**
- 検証成功時 t、失敗時 nil

---

### nskk-dict-errors

エラーハンドリングとリカバリ

#### カスタマイズ変数

##### `nskk-dict-errors-auto-recovery`

```elisp
(defcustom nskk-dict-errors-auto-recovery t
  "エラー時の自動リカバリを有効にするか"
  :type 'boolean
  :group 'nskk-dict-errors)
```

**デフォルト:** `t`

##### `nskk-dict-errors-fallback-enabled`

```elisp
(defcustom nskk-dict-errors-fallback-enabled t
  "フォールバック辞書を使用するか"
  :type 'boolean
  :group 'nskk-dict-errors)
```

**デフォルト:** `t`

##### `nskk-dict-errors-log-level`

```elisp
(defcustom nskk-dict-errors-log-level 'warning
  "ログレベル"
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warning)
                 (const :tag "Error" error))
  :group 'nskk-dict-errors)
```

**デフォルト:** `'warning`

#### エラー型

```elisp
(define-error 'nskk-dict-error "NSKK dictionary error")
(define-error 'nskk-dict-parse-error "Dictionary parse error" 'nskk-dict-error)
(define-error 'nskk-dict-io-error "Dictionary I/O error" 'nskk-dict-error)
(define-error 'nskk-dict-encoding-error "Dictionary encoding error" 'nskk-dict-error)
(define-error 'nskk-dict-validation-error "Dictionary validation error" 'nskk-dict-error)
(define-error 'nskk-dict-cache-error "Dictionary cache error" 'nskk-dict-error)
(define-error 'nskk-dict-corruption-error "Dictionary corruption error" 'nskk-dict-error)
```

#### 関数

##### `nskk-dict-errors-load-with-recovery`

```elisp
(nskk-dict-errors-load-with-recovery FILE-PATH)
```

エラーリカバリ付き辞書読み込み

**引数:**
- `FILE-PATH` (string): 辞書ファイルパス

**戻り値:**
- 読み込み成功時は `nskk-dict-index`、失敗時はフォールバック辞書

**使用例:**

```elisp
(setq my-dict (nskk-dict-errors-load-with-recovery "~/.skk/SKK-JISYO.L"))
;; エラー発生時も最小限の辞書で動作継続
```

##### `nskk-dict-errors-create-fallback-index`

```elisp
(nskk-dict-errors-create-fallback-index)
```

フォールバック辞書を生成

**戻り値:**
- 最小限のエントリを含む `nskk-dict-index`（45+エントリ）

##### `nskk-dict-errors-handle`

```elisp
(nskk-dict-errors-handle ERROR CONTEXT)
```

エラーを処理

**引数:**
- `ERROR`: エラーオブジェクト
- `CONTEXT` (plist): エラーコンテキスト

##### `nskk-dict-errors-log`

```elisp
(nskk-dict-errors-log LEVEL MESSAGE &rest ARGS)
```

エラーログを記録

**引数:**
- `LEVEL` (symbol): ログレベル（debug/info/warning/error）
- `MESSAGE` (string): メッセージフォーマット
- `ARGS`: フォーマット引数

---

## Search Algorithm (Track E)

### nskk-trie

Trie（トライ木）データ構造実装

#### データ構造

##### `nskk-trie`

```elisp
(cl-defstruct nskk-trie
  root              ;; ルートノード
  size              ;; ノード総数
  entry-count)      ;; エントリ総数
```

##### `nskk-trie-node`

```elisp
(cl-defstruct nskk-trie-node
  char              ;; 文字
  children          ;; 子ノードのハッシュテーブル
  value             ;; 終端ノードの値
  is-terminal)      ;; 終端フラグ
```

#### 関数

##### `nskk-trie-create`

```elisp
(nskk-trie-create)
```

新しいTrieを作成

**戻り値:**
- `nskk-trie` 構造体

**使用例:**

```elisp
(setq my-trie (nskk-trie-create))
```

##### `nskk-trie-insert`

```elisp
(nskk-trie-insert TRIE KEY VALUE)
```

キーと値をTrieに挿入

**引数:**
- `TRIE` (nskk-trie): Trie構造体
- `KEY` (string): 挿入するキー
- `VALUE`: 格納する値

**使用例:**

```elisp
(nskk-trie-insert my-trie "あい" '("愛" "藍"))
(nskk-trie-insert my-trie "あいさつ" '("挨拶"))
```

##### `nskk-trie-lookup`

```elisp
(nskk-trie-lookup TRIE KEY)
```

完全一致検索

**引数:**
- `TRIE` (nskk-trie): Trie構造体
- `KEY` (string): 検索キー

**戻り値:**
- 見つかった値、または nil

**使用例:**

```elisp
(nskk-trie-lookup my-trie "あい")  ;; => ("愛" "藍")
```

##### `nskk-trie-prefix-search`

```elisp
(nskk-trie-prefix-search TRIE PREFIX &optional LIMIT)
```

前方一致検索

**引数:**
- `TRIE` (nskk-trie): Trie構造体
- `PREFIX` (string): 検索プレフィックス
- `LIMIT` (optional integer): 最大結果数

**戻り値:**
- (キー . 値) のリスト

**計算量:** O(k + n) （k=プレフィックス長、n=結果数）

**使用例:**

```elisp
(nskk-trie-prefix-search my-trie "あい")
;; => (("あい" . ("愛" "藍"))
;;     ("あいさつ" . ("挨拶")))
```

##### `nskk-trie-delete`

```elisp
(nskk-trie-delete TRIE KEY)
```

キーを削除

**引数:**
- `TRIE` (nskk-trie): Trie構造体
- `KEY` (string): 削除するキー

**戻り値:**
- 削除成功時 t、キーが存在しない場合 nil

---

### nskk-search

統合検索インターフェース

#### カスタマイズ変数

##### `nskk-search-default-algorithm`

```elisp
(defcustom nskk-search-default-algorithm 'exact
  "デフォルトの検索アルゴリズム"
  :type '(choice (const :tag "Exact match" exact)
                 (const :tag "Prefix match" prefix)
                 (const :tag "Partial match" partial)
                 (const :tag "Fuzzy match" fuzzy))
  :group 'nskk-search)
```

**デフォルト:** `'exact`

##### `nskk-search-fuzzy-threshold`

```elisp
(defcustom nskk-search-fuzzy-threshold 2
  "ファジー検索の最大編集距離"
  :type 'integer
  :group 'nskk-search)
```

**デフォルト:** `2`

##### `nskk-search-max-results`

```elisp
(defcustom nskk-search-max-results 100
  "検索結果の最大数"
  :type 'integer
  :group 'nskk-search)
```

**デフォルト:** `100`

#### 関数

##### `nskk-search`

```elisp
(nskk-search INDEX QUERY &optional TYPE LIMIT)
```

統合検索関数

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `QUERY` (string): 検索クエリ
- `TYPE` (optional symbol): 検索タイプ（exact/prefix/partial/fuzzy）
- `LIMIT` (optional integer): 最大結果数

**戻り値:**
- `nskk-dict-entry` のリスト

**使用例:**

```elisp
;; 完全一致検索
(nskk-search my-index "あい" 'exact)

;; 前方一致検索
(nskk-search my-index "あい" 'prefix 10)

;; ファジー検索
(nskk-search my-index "あお" 'fuzzy)
```

##### `nskk-search-exact`

```elisp
(nskk-search-exact INDEX KEY)
```

完全一致検索（高速版）

**計算量:** O(1)

##### `nskk-search-prefix`

```elisp
(nskk-search-prefix INDEX PREFIX &optional LIMIT)
```

前方一致検索

**計算量:** O(k + n)

##### `nskk-search-partial`

```elisp
(nskk-search-partial INDEX SUBSTRING &optional LIMIT)
```

部分一致検索

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `SUBSTRING` (string): 検索する部分文字列
- `LIMIT` (optional integer): 最大結果数

##### `nskk-search-fuzzy`

```elisp
(nskk-search-fuzzy INDEX QUERY &optional THRESHOLD LIMIT)
```

ファジー検索（Levenshtein距離ベース）

**引数:**
- `INDEX` (nskk-dict-index): 辞書インデックス
- `QUERY` (string): 検索クエリ
- `THRESHOLD` (optional integer): 最大編集距離（デフォルト: 2）
- `LIMIT` (optional integer): 最大結果数

---

### nskk-cache

LRU/LFUキャッシュ実装

#### データ構造

##### `nskk-cache-lru`

```elisp
(cl-defstruct nskk-cache-lru
  capacity          ;; 最大容量
  size              ;; 現在のサイズ
  hash-table        ;; データストア
  access-list)      ;; アクセス順リスト
```

##### `nskk-cache-lfu`

```elisp
(cl-defstruct nskk-cache-lfu
  capacity          ;; 最大容量
  size              ;; 現在のサイズ
  hash-table        ;; データストア
  frequency-table)  ;; 頻度カウンタ
```

#### カスタマイズ変数

##### `nskk-cache-default-capacity`

```elisp
(defcustom nskk-cache-default-capacity 1000
  "デフォルトキャッシュ容量"
  :type 'integer
  :group 'nskk-cache)
```

**デフォルト:** `1000`

##### `nskk-cache-strategy`

```elisp
(defcustom nskk-cache-strategy 'lru
  "キャッシュ戦略"
  :type '(choice (const :tag "LRU (Least Recently Used)" lru)
                 (const :tag "LFU (Least Frequently Used)" lfu))
  :group 'nskk-cache)
```

**デフォルト:** `'lru`

#### 関数

##### `nskk-cache-create`

```elisp
(nskk-cache-create &optional STRATEGY CAPACITY)
```

新しいキャッシュを作成

**引数:**
- `STRATEGY` (optional symbol): キャッシュ戦略（lru/lfu）
- `CAPACITY` (optional integer): 最大容量

**戻り値:**
- キャッシュ構造体

**使用例:**

```elisp
(setq my-cache (nskk-cache-create 'lru 500))
```

##### `nskk-cache-get`

```elisp
(nskk-cache-get CACHE KEY)
```

キャッシュから値を取得

**引数:**
- `CACHE`: キャッシュ構造体
- `KEY`: 検索キー

**戻り値:**
- 見つかった値、または nil

**計算量:** O(1)

**使用例:**

```elisp
(nskk-cache-get my-cache "あい")  ;; => ("愛" "藍")
```

##### `nskk-cache-put`

```elisp
(nskk-cache-put CACHE KEY VALUE)
```

キャッシュに値を格納

**引数:**
- `CACHE`: キャッシュ構造体
- `KEY`: キー
- `VALUE`: 格納する値

**計算量:** O(1)

**使用例:**

```elisp
(nskk-cache-put my-cache "あい" '("愛" "藍"))
```

##### `nskk-cache-invalidate`

```elisp
(nskk-cache-invalidate CACHE &optional KEY)
```

キャッシュを無効化

**引数:**
- `CACHE`: キャッシュ構造体
- `KEY` (optional): 特定のキーのみ無効化（省略時は全体）

##### `nskk-cache-stats`

```elisp
(nskk-cache-stats CACHE)
```

キャッシュ統計を取得

**戻り値:**
- 統計情報のplist
  - `:size` - 現在のサイズ
  - `:capacity` - 最大容量
  - `:hit-rate` - ヒット率
  - `:evictions` - 退避回数

---

### nskk-index

最適化されたインデックス統合レイヤー

#### データ構造

##### `nskk-index`

```elisp
(cl-defstruct nskk-index
  dict-struct       ;; 基本辞書構造
  trie              ;; Trie構造
  cache             ;; キャッシュ
  metadata          ;; メタデータ
  stats             ;; 統計情報
  okuri-ari-trie    ;; 送り仮名ありTrie
  okuri-nasi-trie   ;; 送り仮名なしTrie
  user-dict         ;; ユーザー辞書
  auto-register     ;; 自動登録フラグ
  modified          ;; 変更フラグ
  last-updated      ;; 最終更新時刻
  version)          ;; バージョン
```

#### カスタマイズ変数

##### `nskk-index-enable-cache`

```elisp
(defcustom nskk-index-enable-cache t
  "インデックスキャッシュを有効にするか"
  :type 'boolean
  :group 'nskk-index)
```

**デフォルト:** `t`

##### `nskk-index-cache-size`

```elisp
(defcustom nskk-index-cache-size 1000
  "インデックスキャッシュサイズ"
  :type 'integer
  :group 'nskk-index)
```

**デフォルト:** `1000`

##### `nskk-index-auto-optimize`

```elisp
(defcustom nskk-index-auto-optimize t
  "自動最適化を有効にするか"
  :type 'boolean
  :group 'nskk-index)
```

**デフォルト:** `t`

#### 関数

##### `nskk-index-create`

```elisp
(nskk-index-create DICT-INDEX)
```

辞書インデックスから最適化インデックスを作成

**引数:**
- `DICT-INDEX` (nskk-dict-index): 辞書インデックス

**戻り値:**
- `nskk-index` 構造体

**使用例:**

```elisp
(setq my-dict (nskk-load-dictionary "~/.skk/SKK-JISYO.L"))
(setq my-index (nskk-index-create my-dict))
```

##### `nskk-index-build`

```elisp
(nskk-index-build DICT-PATH &optional OPTIONS)
```

辞書ファイルから直接インデックスを構築

**引数:**
- `DICT-PATH` (string): 辞書ファイルパス
- `OPTIONS` (optional plist): オプション
  - `:enable-cache` - キャッシュ有効化
  - `:cache-size` - キャッシュサイズ
  - `:use-fallback` - フォールバック使用

**戻り値:**
- `nskk-index` 構造体

**使用例:**

```elisp
(setq my-index
  (nskk-index-build "~/.skk/SKK-JISYO.L"
                    :enable-cache t
                    :cache-size 2000))
```

##### `nskk-index-search`

```elisp
(nskk-index-search INDEX QUERY &optional TYPE)
```

インデックスを検索（キャッシュ統合）

**引数:**
- `INDEX` (nskk-index): インデックス構造体
- `QUERY` (string): 検索クエリ
- `TYPE` (optional symbol): 検索タイプ

**戻り値:**
- 検索結果のリスト

**使用例:**

```elisp
(nskk-index-search my-index "あい")
```

##### `nskk-index-add-entry`

```elisp
(nskk-index-add-entry INDEX KEY CANDIDATES)
```

新しいエントリを追加

**引数:**
- `INDEX` (nskk-index): インデックス構造体
- `KEY` (string): 見出し語
- `CANDIDATES` (list): 候補リスト

##### `nskk-index-optimize`

```elisp
(nskk-index-optimize INDEX)
```

インデックスを最適化

**引数:**
- `INDEX` (nskk-index): インデックス構造体

**効果:**
- Trie構造の再構築
- キャッシュのクリア
- 統計情報のリセット

##### `nskk-index-stats`

```elisp
(nskk-index-stats INDEX)
```

インデックス統計を取得

**戻り値:**
- 統計情報のplist
  - `:total-entries` - 総エントリ数
  - `:cache-hit-rate` - キャッシュヒット率
  - `:average-candidates` - 平均候補数
  - `:memory-usage` - メモリ使用量（推定）

---

## UI Components (Track F)

### nskk-keymap

キーバインディング定義

#### 変数

##### `nskk-ascii-mode-map`

ASCIIモード用キーマップ

##### `nskk-hiragana-mode-map`

ひらがなモード用キーマップ

##### `nskk-katakana-mode-map`

カタカナモード用キーマップ

##### `nskk-abbrev-mode-map`

abbrevモード用キーマップ

##### `nskk-henkan-mode-map`

変換モード用キーマップ

##### `nskk-candidate-selection-map`

候補選択モード用キーマップ

##### `nskk-minibuffer-map`

ミニバッファ用キーマップ

##### `nskk-global-map`

グローバルキーマップ

#### カスタマイズ変数

##### `nskk-sticky-shift-enable`

```elisp
(defcustom nskk-sticky-shift-enable t
  "Sticky Shift (;キー) を有効にするか"
  :type 'boolean
  :group 'nskk-keymap)
```

**デフォルト:** `t`

##### `nskk-sticky-shift-key`

```elisp
(defcustom nskk-sticky-shift-key ";"
  "Sticky Shiftキー"
  :type 'string
  :group 'nskk-keymap)
```

**デフォルト:** `";"`

##### `nskk-ddskk-compatible`

```elisp
(defcustom nskk-ddskk-compatible t
  "ddskk互換キーバインドを使用するか"
  :type 'boolean
  :group 'nskk-keymap)
```

**デフォルト:** `t`

#### 関数

##### `nskk-setup-keybindings`

```elisp
(nskk-setup-keybindings &optional MAP)
```

キーバインディングを設定

**引数:**
- `MAP` (optional keymap): 設定先キーマップ（省略時はグローバル）

**使用例:**

```elisp
(nskk-setup-keybindings)
```

##### `nskk-define-key`

```elisp
(nskk-define-key MAP KEY COMMAND)
```

キーバインディングを定義

**引数:**
- `MAP` (keymap): キーマップ
- `KEY` (string or vector): キーシーケンス
- `COMMAND` (symbol or lambda): コマンド

**使用例:**

```elisp
(nskk-define-key nskk-hiragana-mode-map "q" 'nskk-toggle-kana)
```

##### `nskk-customize-key`

```elisp
(nskk-customize-key MODE KEY COMMAND)
```

モード別キーカスタマイズ

**引数:**
- `MODE` (symbol): モード名（ascii/hiragana/katakana等）
- `KEY` (string): キー
- `COMMAND` (symbol): コマンド

**使用例:**

```elisp
(nskk-customize-key 'hiragana "C-j" 'nskk-kakutei)
```

##### `nskk-reset-keybindings`

```elisp
(nskk-reset-keybindings)
```

キーバインディングをデフォルトにリセット

---

### nskk-candidate-window

候補ウィンドウ表示

#### データ構造

##### `nskk-candidate-window`

```elisp
(cl-defstruct nskk-candidate-window
  overlay           ;; オーバーレイ
  buffer            ;; バッファ
  candidates        ;; 候補リスト
  current-index     ;; 現在のインデックス
  page-size         ;; ページサイズ
  current-page      ;; 現在のページ
  visible)          ;; 表示フラグ
```

#### カスタマイズ変数

##### `nskk-candidate-window-page-size`

```elisp
(defcustom nskk-candidate-window-page-size 7
  "候補ウィンドウの1ページあたりの候補数"
  :type 'integer
  :group 'nskk-candidate-window)
```

**デフォルト:** `7`

##### `nskk-candidate-window-use-annotation`

```elisp
(defcustom nskk-candidate-window-use-annotation t
  "候補にアノテーションを表示するか"
  :type 'boolean
  :group 'nskk-candidate-window)
```

**デフォルト:** `t`

##### `nskk-candidate-window-position`

```elisp
(defcustom nskk-candidate-window-position 'bottom
  "候補ウィンドウの表示位置"
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Top" top)
                 (const :tag "Inline" inline))
  :group 'nskk-candidate-window)
```

**デフォルト:** `'bottom`

#### 関数

##### `nskk-show-candidates`

```elisp
(nskk-show-candidates CANDIDATES &optional INDEX)
```

候補ウィンドウを表示

**引数:**
- `CANDIDATES` (list): 表示する候補リスト
- `INDEX` (optional integer): 初期選択インデックス（デフォルト: 0）

**使用例:**

```elisp
(nskk-show-candidates '("愛" "藍" "相" "合"))
```

##### `nskk-update-candidates`

```elisp
(nskk-update-candidates WINDOW INDEX)
```

候補選択を更新

**引数:**
- `WINDOW` (nskk-candidate-window): ウィンドウ構造体
- `INDEX` (integer): 新しいインデックス

##### `nskk-hide-candidates`

```elisp
(nskk-hide-candidates WINDOW)
```

候補ウィンドウを非表示

##### `nskk-scroll-candidates-forward`

```elisp
(nskk-scroll-candidates-forward WINDOW)
```

次のページへスクロール

##### `nskk-scroll-candidates-backward`

```elisp
(nskk-scroll-candidates-backward WINDOW)
```

前のページへスクロール

##### `nskk-candidate-window-format`

```elisp
(nskk-candidate-window-format CANDIDATES INDEX PAGE-SIZE)
```

候補リストをフォーマット

**戻り値:**
- フォーマットされた文字列

---

### nskk-minibuffer

ミニバッファUI

#### データ構造

##### `nskk-minibuffer-state`

```elisp
(cl-defstruct nskk-minibuffer-state
  prompt            ;; プロンプト文字列
  input             ;; 入力文字列
  overlay           ;; オーバーレイ
  active)           ;; アクティブフラグ
```

#### カスタマイズ変数

##### `nskk-minibuffer-show-inline-candidate`

```elisp
(defcustom nskk-minibuffer-show-inline-candidate t
  "インライン候補表示を有効にするか"
  :type 'boolean
  :group 'nskk-minibuffer)
```

**デフォルト:** `t`

##### `nskk-minibuffer-inline-candidate-face`

```elisp
(defcustom nskk-minibuffer-inline-candidate-face 'shadow
  "インライン候補のフェイス"
  :type 'face
  :group 'nskk-minibuffer)
```

**デフォルト:** `'shadow`

#### 関数

##### `nskk-minibuffer-show`

```elisp
(nskk-minibuffer-show PROMPT &optional INITIAL-INPUT)
```

ミニバッファUIを表示

**引数:**
- `PROMPT` (string): プロンプト文字列
- `INITIAL-INPUT` (optional string): 初期入力

**戻り値:**
- `nskk-minibuffer-state` 構造体

##### `nskk-minibuffer-update`

```elisp
(nskk-minibuffer-update STATE INPUT)
```

ミニバッファの表示を更新

**引数:**
- `STATE` (nskk-minibuffer-state): ステート構造体
- `INPUT` (string): 新しい入力文字列

##### `nskk-minibuffer-show-inline-candidate`

```elisp
(nskk-minibuffer-show-inline-candidate CANDIDATE)
```

インライン候補を表示（グレーアウト表示）

**引数:**
- `CANDIDATE` (string): 表示する候補

**使用例:**

```elisp
;; "あ" と入力時に "愛" をグレー表示
(nskk-minibuffer-show-inline-candidate "愛")
```

##### `nskk-minibuffer-hide`

```elisp
(nskk-minibuffer-hide STATE)
```

ミニバッファUIを非表示

---

### nskk-modeline

モードライン表示

#### カスタマイズ変数

##### `nskk-modeline-format`

```elisp
(defcustom nskk-modeline-format "[%m%s]"
  "モードライン表示フォーマット
%m: モード名
%s: 状態インジケータ"
  :type 'string
  :group 'nskk-modeline)
```

**デフォルト:** `"[%m%s]"`

##### `nskk-modeline-mode-names`

```elisp
(defcustom nskk-modeline-mode-names
  '((ascii . "A")
    (hiragana . "あ")
    (katakana . "ア")
    (katakana-半角 . "ｱ")
    (abbrev . "aA")
    (latin . "L"))
  "モード名の表示マッピング"
  :type 'alist
  :group 'nskk-modeline)
```

##### `nskk-modeline-use-color`

```elisp
(defcustom nskk-modeline-use-color t
  "モードラインに色を使用するか"
  :type 'boolean
  :group 'nskk-modeline)
```

**デフォルト:** `t`

#### 関数

##### `nskk-modeline-update`

```elisp
(nskk-modeline-update STATE)
```

モードライン表示を更新

**引数:**
- `STATE` (nskk-state): 状態オブジェクト

**使用例:**

```elisp
(nskk-modeline-update my-state)
;; => モードラインに "[あ▽]" と表示
```

##### `nskk-modeline-format`

```elisp
(nskk-modeline-format MODE STATE-INDICATOR)
```

モードライン文字列をフォーマット

**引数:**
- `MODE` (symbol): モード
- `STATE-INDICATOR` (string): 状態インジケータ（"▽"/"▼"等）

**戻り値:**
- フォーマットされた文字列

##### `nskk-modeline-install`

```elisp
(nskk-modeline-install)
```

モードライン表示機能をインストール

##### `nskk-modeline-uninstall`

```elisp
(nskk-modeline-uninstall)
```

モードライン表示機能をアンインストール

##### `nskk-modeline-get-indicator`

```elisp
(nskk-modeline-get-indicator STATE)
```

状態インジケータを取得

**引数:**
- `STATE` (nskk-state): 状態オブジェクト

**戻り値:**
- インジケータ文字列
  - `"▽"` - 入力待ち
  - `"▼"` - 変換中
  - `""` - ASCIIモード

---

## パフォーマンス目標

### Core Engine
- ローマ字変換: < 0.1ms per conversion
- 特殊文字変換: < 0.05ms

### Dictionary
- 辞書読み込み: < 3秒（50万エントリ）
- 完全一致検索: < 0.1ms
- 前方一致検索: < 1ms（10件取得）

### Search & Cache
- Trie検索: O(k + n)
- キャッシュアクセス: O(1)
- キャッシュヒット率: > 80%

### UI
- キー入力応答: < 10ms
- 候補表示更新: < 50ms

---

## 使用例

### 基本的な使用フロー

```elisp
;; 1. 辞書読み込み
(setq my-dict (nskk-load-dictionary "~/.skk/SKK-JISYO.L"))

;; 2. インデックス構築
(setq my-index (nskk-index-build "~/.skk/SKK-JISYO.L"
                                 :enable-cache t
                                 :cache-size 1000))

;; 3. 状態初期化
(setq my-state (nskk-state-create 'hiragana))

;; 4. ローマ字変換
(setq result (nskk-convert-romaji "konnnitiha"))
;; => #s(nskk-converter-result :converted "こんにちは" ...)

;; 5. 辞書検索
(setq candidates (nskk-index-search my-index "あい"))
;; => (("愛" "藍" "相" "合"))

;; 6. 候補表示
(nskk-show-candidates candidates)

;; 7. 確定
(nskk-buffer-commit my-state)
```

### カスタマイズ例

```elisp
;; 促音処理を無効化
(setq nskk-converter-use-sokuon nil)

;; キャッシュサイズを増やす
(setq nskk-cache-default-capacity 2000)

;; Sticky Shiftキーを変更
(setq nskk-sticky-shift-key ":")

;; 候補ウィンドウのページサイズ変更
(setq nskk-candidate-window-page-size 10)

;; モード切り替えフック登録
(add-hook 'nskk-mode-switch-hook
          (lambda ()
            (message "Mode switched to: %s"
                     (nskk-state-mode my-state))))
```

### イベントリスナー登録例

```elisp
;; 状態変更イベント
(nskk-events-add-listener
  :state-changed
  (lambda (data)
    (message "State: %s -> %s"
             (plist-get data :from)
             (plist-get data :to))))

;; エラーイベント
(nskk-events-add-listener
  :error
  (lambda (data)
    (message "Error: %s" (plist-get data :message))))
```

### ベンチマーク実行例

```elisp
;; ローマ字変換ベンチマーク
(nskk-benchmark-romaji-conversion 10000)
;; => ((average . 0.000082)
;;     (total . 0.82)
;;     (iterations . 10000)
;;     (target-met . t))

;; 総合ベンチマーク
(nskk-benchmark-suite)
```

---

## データ構造サマリー

| 構造体 | 主な用途 | 主要フィールド |
|--------|----------|----------------|
| `nskk-converter-result` | 変換結果 | converted, pending, consumed |
| `nskk-state` | 状態管理 | mode, input-buffer, candidates |
| `nskk-dict-index` | 辞書インデックス | entries-count, trie, hash-table |
| `nskk-dict-entry` | 辞書エントリ | key, candidates, okuri-type |
| `nskk-trie` | Trie構造 | root, size, entry-count |
| `nskk-cache-lru` | LRUキャッシュ | capacity, hash-table, access-list |
| `nskk-index` | 統合インデックス | dict-struct, trie, cache |
| `nskk-candidate-window` | 候補ウィンドウ | overlay, candidates, current-index |

---

## エラー型一覧

```elisp
nskk-dict-error              ;; 基底エラー
├── nskk-dict-parse-error    ;; パースエラー
├── nskk-dict-io-error       ;; I/Oエラー
├── nskk-dict-encoding-error ;; エンコーディングエラー
├── nskk-dict-validation-error ;; 検証エラー
├── nskk-dict-cache-error    ;; キャッシュエラー
└── nskk-dict-corruption-error ;; 破損エラー
```

---

## 統計情報

- **総モジュール数:** 20
- **公開関数数:** 150+
- **カスタマイズ変数数:** 60+
- **データ構造数:** 15
- **イベント型数:** 9
- **エラー型数:** 7
- **使用例数:** 25+
