# ddskk UI比較チェックリスト

## 目的
- NSKKがddskkと同じ操作感・UIを提供できているかを体系的に確認する。
- 自動検証と手動検証を組み合わせ、差分が発生した場合に再現手順と是正方針を明確化する。

## 前提条件
- Emacsにddskk(`skk`パッケージ)がインストールされ、`load-path`に設定されていること。
- NSKKリポジトリのルートで作業すること。
- 検証用プロファイルは `emacs -Q` で起動し、副作用のある設定を読み込まないこと。

## 自動検証フロー
1. ddskkキーマップ差分チェック
   - 事前準備: `vendor/ddskk/` に本家ddskkを配置（例: `git clone https://github.com/skk-dev/ddskk.git vendor/ddskk`）。
   - コマンド: 
     ```
     emacs -Q --batch -L . -L tests \
       --eval '(add-to-list \='load-path "vendor/ddskk")' \
       -l tests/tools/compare-ddskk.el \
       -f nskk-compare-ddskk-run
     ```
   - 期待: 差分なしの場合は終了コード0、差分ありの場合はキー一覧が出力される。
   - オプション: `--eval '(setq nskk-compare-ddskk-strict nil)'` を付与すると差分があっても終了コード0で継続確認できる。
2. 既存ERTスイートの実行
   - `emacs --batch -L . -l ert -l tests/nskk-test.el -f ert-run-tests-batch-and-exit`
   - 特にキーマップ関連テストとモード制御テストの成功を確認する。
   - 注: `tests/nskk-keymap-test.el` と `tests/nskk-e2e-mode-control-test.el` は今後作成予定。

## 手動検証チェックリスト
| 番号 | 項目 | 検証観点 | 手順 | 判定 | 備考 |
| --- | --- | --- | --- | --- | --- |
| M-01 | モードインジケータ | ミニバッファに▽/▼/◇が表示される | `emacs -Q` でNSKKを有効化し、`C-j`/`q`/`/`でモード切替を行う |  |  |
| M-02 | モード切替キー | `C-j`,`q`,`Q`,`l`,`L`,`/`,`\` が即時にモードを切り替える | `describe-key`で割り当てを確認し、実際に入力して挙動確認 |  |  |
| M-03 | 促音・撥音入力 | `tt`, `nn` などで自動変換される | 日本語バッファでローマ字入力し、期待文字になるか確認 |  |  |
| M-04 | 候補ウィンドウ表示 | 候補番号・ハイライト・注釈表示がddskkと揃っている | `SPC`で変換候補を表示し、番号および強調表示を比較 |  |  |
| M-05 | 候補ページング | 候補数が多い場合にページ情報が表示される | 大語彙候補を表示し、`C-n`/`C-p`でページ遷移 |  |  |
| M-06 | `skk-auto-fill-mode` 互換 | モード有効化で既存バインドと衝突しない | `M-x nskk-auto-fill-mode` を実行し、`C-h`などが本来のコマンドを保持しているか確認 |  |  |
| M-07 | `skk-tutorial` 互換 | チュートリアルバッファが表示され、操作が継続できる | `M-x nskk-tutorial` を実行し、ddskk付属ファイルが読み込まれるか確認 |  |  |
| M-08 | 候補ウィンドウ配色 | ddskkのフェイスと色味が揃っている | `list-colors-display` で差分がないかを目視で比較 |  |  |
| M-09 | ステータスライン表示 | mode-lineにNSKK状態が表示される | mode-lineの表示内容をddskkと比較 |  |  |
| M-10 | マクロ再生互換 | `execute-kbd-macro` で記録した入力が再現できる | 文字入力とモード切替を含むキーボードマクロを録画・再生 |  |  |

- 判定欄には `OK` / `NG` / `SKIP` を記載し、NGの際は備考に差分内容を詳細に記入する。
- 画面キャプチャは `docs/usability/screenshots/` に保管し、ファイル名に項目番号を含める。

## 差分発生時の対応
1. 自動比較レポートの差分に対応するテストを特定する。
2. 再現手順と期待値を `docs/usability/diff-log.md` に追加する（なければ作成する）。
3. 必要に応じてテストファイルを新規作成し、回帰防止を図る。

## 既存検証状況のメモ
- `tests/nskk-keymap-test.el` でモード切替キーの境界条件を自動検証予定（テストファイル作成予定）。
- `tests/nskk-candidate-window-test.el` で候補番号付与・注釈表示ロジックをカバー予定（テストファイル作成予定）。
- UIの視覚差分（配色・インジケータ表示）は未記録のため、手動検証結果を残すこと。

## 記録テンプレート
```
## YYYY/MM/DD 実施メモ
- 実施者: 
- 使用Emacs: 
- ddskkバージョン: 
- NSKKコミット: 
- 自動比較結果: 差分なし / 差分あり (要約)
- チェックリスト結果:
  - M-01: OK
  - M-02: ...
- 差分スクリーンショット: screenshots/M-04.png ほか
- 追加メモ:
```

## 参考コマンド一覧
```
# キーマップ差分レポートのみ出力
emacs -Q --batch -L . -L tests \
  -l tests/tools/compare-ddskk.el \
  --eval '(setq nskk-compare-ddskk-strict nil)' \
  -f nskk-compare-ddskk-run

# UIテスト用にNSKKを起動
emacs -Q \
  --eval '(require \='nskk)' \
  --eval '(nskk-setup-keybindings)'
```
