# NSKKシナリオテスト

## 概要

NSKKシナリオテストは、ユーザーが実際に手を動かして試すためのテストフレームワークです。

## 特徴

- **直感的なDSL**: `nskk-defscenario`マクロで簡潔にシナリオを記述
- **ERT統合**: 既存のテストインフラとシームレスに統合
- **タグシステム**: 難易度や機能別にシナリオを分類
- **キー入力シミュレーション**: `execute-kbd-macro`による高精度な再現

## 実行方法

### Emacs内で実行

```elisp
;; 全シナリオ実行
M-x nskk-scenario-run-all

;; 基本シナリオのみ
M-x nskk-scenario-run-basic

;; 初心者向けシナリオのみ
M-x nskk-scenario-run-beginner

;; シナリオリスト表示
M-x nskk-scenario-list-all
```

### コマンドラインで実行

```bash
# 全シナリオ実行
make test-scenarios

# 基本シナリオのみ
make test-scenarios-basic

# 初心者向けシナリオのみ
make test-scenarios-beginner

# シナリオリスト表示
make list-scenarios
```

## シナリオ一覧

### 基本シナリオ

#### ひらがな入力
- `basic-hiragana-input`: 基本的なひらがな入力
- `hiragana-youon-input`: ひらがな拗音入力（きゃ、きゅ、きょ）
- `hiragana-sokuon-input`: ひらがな促音入力（っ）

#### カタカナ入力
- `basic-katakana-input`: 基本的なカタカナ入力
- `katakana-youon-input`: カタカナ拗音入力（キャ、キュ、キョ）
- `mode-toggle-hiragana-katakana`: ひらがな⇔カタカナモード切り替え

#### 漢字変換
- `basic-kanji-conversion`: 基本的な漢字変換
- `kanji-candidate-selection`: 変換候補の選択操作
- `kanji-conversion-not-found`: 辞書に存在しない見出し語の処理

## タグ一覧

- `:scenario` - すべてのシナリオテスト
- `:basic` - 基本機能
- `:beginner` - 初心者向け
- `:intermediate` - 中級者向け
- `:hiragana` - ひらがな関連
- `:katakana` - カタカナ関連
- `:kanji` - 漢字変換関連
- `:youon` - 拗音処理
- `:sokuon` - 促音処理
- `:conversion` - 変換機能
- `:candidates` - 候補選択
- `:mode-switch` - モード切り替え
- `:error-handling` - エラー処理

## 新しいシナリオの追加

```elisp
(require 'nskk-scenario-dsl)

(nskk-defscenario my-custom-scenario
  "カスタムシナリオの説明"
  :tags '(:custom :advanced)
  :initial-mode 'hiragana

  (step "ステップ1の説明"
    (type "input")
    (expect-pending "期待値"))

  (step "ステップ2の説明"
    (press 'return)
    (expect-buffer-contains "確定後の期待値")))
```
