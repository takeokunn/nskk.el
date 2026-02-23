# ddskk キーマップ差分ログ

## 2025-10-11 実行メモ
- 実施者: 自動化スクリプト (nskk-compare-ddskk-run)
- NSKKコミット: 未確定（ローカル検証）
- ddskkバージョン: vendor/ddskk (master)
- 実行コマンド:
  ```
  emacs -Q --batch -L . -L tests \
    --eval '(add-to-list \='load-path "vendor/ddskk")' \
    -l tests/tools/compare-ddskk.el \
    --eval '(setq nskk-compare-ddskk-strict nil)' \
    -f nskk-compare-ddskk-run
  ```
- 差分サマリ:
  - ひらがな・カタカナ・abbrev モードで `C-g`, `C-n`, `C-p`, `RET`, `TAB` などがNSKK側にのみバインド。ddskkはグローバル/内部処理に委譲しているため仕様差と判断。
  - 全角英数モードで ddskk は `C-q` を `skk-toggle-characters` に割り当て。NSKKでは同等機能未実装。
- 対応メモ:
  - NSKK側のバインドはテストカバレッジ済み (`tests/nskk-keymap-test.el`)。ddskkとの差分としてドキュメント化し、必要に応じて互換レイヤーで調整する。
  - 全角英数モードの `C-q` 差分はUI比較検証（チェック項目 M-02, M-04）で確認し、追加実装の要否を判断する。
