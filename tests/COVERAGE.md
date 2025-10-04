# NSKKカバレッジツール

## 概要

`nskk-coverage.el`は、NSKKプロジェクト専用のコードカバレッジ測定ツールです。Emacsの標準ライブラリ`testcover.el`をベースに、HTMLレポート生成、閾値チェック、CI統合機能を提供します。

## 特徴

- **外部依存ゼロ**: Emacs 31標準機能のみを使用
- **値の多様性チェック**: testcover.elの機能により、テストの質も評価
- **多様なレポート形式**: HTML、JSON、テキスト形式に対応
- **閾値チェック**: カバレッジ率が指定値を下回るとエラー
- **CI統合**: makeコマンドやスクリプトから簡単に実行可能

## 使用方法

### 基本的な使い方

```bash
# カバレッジ測定とレポート生成
make coverage

# HTMLレポートをブラウザで開く
make coverage-report
```

### Emacs Lispから使用

```elisp
;; カバレッジ測定開始
(require 'nskk-coverage)
(nskk-coverage-start)

;; テスト実行
(ert-run-tests-batch-and-exit)

;; レポート生成
(nskk-coverage-report 'html)   ; HTMLレポート
(nskk-coverage-report 'json)   ; JSONレポート
(nskk-coverage-report 'text)   ; テキストレポート

;; 閾値チェック（95%）
(nskk-coverage-check-threshold 95.0)
```

### スクリプトから使用

```bash
emacs -Q --batch -L . -L tests -l tests/run-coverage.el
```

## レポート形式

### HTMLレポート

- **ファイル**: `coverage/index.html`
- **特徴**:
  - 視覚的なプログレスバー
  - カバレッジ率による色分け（緑: ≥90%, 黄: ≥75%, 赤: <75%）
  - レスポンシブデザイン
  - インタラクティブな表示

### JSONレポート

- **ファイル**: `coverage/coverage.json`
- **用途**: CI/CDパイプラインでの自動処理
- **形式**:
```json
{
  "summary": {
    "total-files": 8,
    "total-forms": 1000,
    "total-covered": 950,
    "coverage-rate": 95.0
  },
  "files": [
    {
      "file": "nskk-state.el",
      "total": 100,
      "covered": 95,
      "coverage_rate": 95.0
    }
  ]
}
```

### テキストレポート

- **出力**: バッファ `*NSKK Coverage Report*`
- **用途**: コマンドライン環境での確認
- **例**:
```
=== NSKK Code Coverage Report ===

Total Coverage: 95.00%
OK Coverage:    90.00%
Total Forms:    1000
Covered Forms:  950
Total Files:    8

File Coverage:
--------------------------------------------------------------------------------
nskk-state.el                                      95.00% ( 95/ 100)
nskk-converter.el                                  92.00% ( 92/ 100)
...
```

## カスタマイズ

### カスタマイズ変数

```elisp
;; 出力ディレクトリ
(setq nskk-coverage-output-dir "coverage")

;; 除外パターン
(setq nskk-coverage-exclude-patterns '("nskk-test.*\\.el$" "nskk-coverage\\.el$"))

;; デフォルト閾値
(setq nskk-coverage-threshold 95.0)

;; 詳細出力
(setq nskk-coverage-verbose t)
```

## コマンド一覧

### インタラクティブコマンド

- `M-x nskk-coverage-start` - カバレッジ測定開始
- `M-x nskk-coverage-collect` - カバレッジデータ収集
- `M-x nskk-coverage-report` - レポート生成
- `M-x nskk-coverage-check-threshold` - 閾値チェック
- `M-x nskk-coverage-clear` - カバレッジデータクリア
- `M-x nskk-coverage-stats` - 統計情報表示

### プログラマティックAPI

```elisp
;; ソースファイル検索
(nskk-coverage--find-source-files)

;; 除外判定
(nskk-coverage--excluded-p "path/to/file.el")

;; 集計サマリー
(nskk-coverage--aggregate-summary)
```

## CI統合例

### GitHub Actions

```yaml
name: Coverage

on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: purcell/setup-emacs@master
        with:
          version: 31.0
      - name: Run tests with coverage
        run: make coverage
      - name: Check coverage threshold
        run: |
          emacs -Q --batch -L . -L tests \
            -l tests/nskk-coverage.el \
            --eval "(progn (nskk-coverage-start) (nskk-coverage-check-threshold 95.0))"
      - name: Upload coverage report
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report
          path: coverage/
```

## トラブルシューティング

### カバレッジが0%になる

**原因**: インストルメント前にファイルがロードされている

**解決策**:
```bash
# クリーンな状態で実行
emacs -Q --batch -L . -L tests -l tests/run-coverage.el
```

### HTMLレポートが生成されない

**原因**: 出力ディレクトリの権限エラー

**解決策**:
```elisp
(setq nskk-coverage-output-dir (expand-file-name "~/nskk-coverage"))
```

### testcover.elが見つからない

**原因**: Emacsバージョンが古い

**解決策**: Emacs 31.0以降を使用

## パフォーマンス

- **オーバーヘッド**: 通常のテスト実行の2-5倍
- **メモリ使用量**: +10-30MB（プロジェクトサイズに依存）
- **推奨環境**: CI環境での使用を想定（開発時は通常のテストを実行）

## 実装詳細

### アーキテクチャ

```
┌─────────────────────────────┐
│  nskk-coverage.el           │
│  (メインモジュール)          │
└─────────────────────────────┘
         │
         ├─ testcover.el (Emacs標準)
         │  └─ インストルメンテーション
         │
         ├─ データ収集・集計
         │  └─ edebug-coverageプロパティ解析
         │
         └─ レポート生成
            ├─ HTML (視覚的レポート)
            ├─ JSON (CI統合)
            └─ Text (コンソール出力)
```

### カバレッジ判定基準

testcover.elは以下の3つの状態を記録:

1. **edebug-unknown**: 未実行（赤：カバレッジ0%）
2. **値オブジェクト**: 初回実行時の値（茶色：カバレッジ不十分）
3. **edebug-ok-coverage**: 複数の異なる値を返した（カバレッジ十分）

### データ構造

```elisp
;; ファイル単位のカバレッジデータ
(list :total 100              ; 総フォーム数
      :covered 95             ; カバーされたフォーム数
      :ok-coverage 90         ; 十分にカバーされたフォーム数
      :coverage-rate 95.0)    ; カバレッジ率（%）
```

## 参考資料

- [GNU Emacs Lisp Reference Manual - Test Coverage](https://www.gnu.org/software/emacs/manual/html_node/elisp/Test-Coverage.html)
- [testcover.el source](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/testcover.el)
- [NSKK Test Framework](./nskk-test-framework.el)

## ライセンス

NSKK Code Coverage Tool は GNU General Public License v3.0 の下で配布されています。
