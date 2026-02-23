# Track S: ミニバッファUI設計仕様書

## 概要

Track S（Task 3.23-3.25）では、外部ライブラリ（Transient等）への依存を持たず、純粋なEmacs Lispだけで動作する管理UIを設計します。`completing-read` とプレーンなミニバッファプロンプトを基盤に、設定・プラグイン・デバッグ各メニューを統一的に扱える仕組みを提供します。

**注意**: ファイル名に `nskk-transient-*` というプレフィックスが使われていますが、これは開発初期にTransient UIを想定していた名残であり、実際にはTransientへの依存はありません。内部名称として継続使用しています。

## モジュール設計

### Task 3.23: 設定メニュー

- **主要ファイル**: `nskk-transient-config.el`
- **設計内容**
  - `nskk-config-menu` でカテゴリ選択 → 項目ごとの質問を逐次実行
  - `nskk-config--prompt-*` 系ユーティリティで型に応じた入力を整備
  - `setopt` ベースの安全な適用ロジック、プレビュー表示、プリセット保存/読込/削除
- **利用例**
  ```elisp
  (keymap-global-set "C-c n c" #'nskk-config-menu)
  (nskk-config-preset-save "workstation")
  ```

### Task 3.24: プラグイン管理UI

- **主要ファイル**: `nskk-transient-plugins.el`
- **設計内容**
  - `nskk-plugins-menu` で操作一覧を `completing-read` から選択
  - 状態概要は専用バッファに整形出力（有効数/インストール済み数/レジストリ取得状況）
  - インストール/アンインストール、有効化、レジストリ更新・検索を既存APIへ委譲
- **利用例**
  ```elisp
  (keymap-global-set "C-c n p" #'nskk-plugins-menu)
  (nskk-plugin-install 'nskk-plugin-emoji)
  ```

### Task 3.25: デバッグメニュー

- **主要ファイル**: `nskk-transient-debug.el`
- **設計内容**
  - `nskk-debug-menu` からプロファイリング、ログ、メトリクス、エクスポート等を選択
  - 「状態を再表示」アクションで概要バッファを再描画
  - 自動リフレッシュやスナップショット等の既存機能をメニュー化
- **利用例**
  ```elisp
  (keymap-global-set "C-c n d" #'nskk-debug-menu)
  (nskk-debug-profile-start)
  ```

## トップレベルメニュー

`nskk-menu` は上記3メニューを統合するエントリーポイントです。ロード済み関数のみを候補に表示するため柔軟に拡張できます。

```elisp
(keymap-global-set "C-x C-j" #'nskk-menu)
```

## 計画ファイル構成

| ファイル                      | 主なインタラクション | 外部依存 |
|-------------------------------|----------------------|----------|
| nskk-transient-config.el      | sequential prompt    | なし     |
| nskk-transient-plugins.el     | completing-read      | なし     |
| nskk-transient-debug.el       | completing-read      | なし     |

## テスト計画

- バッチロードの検証: `emacs --batch -Q -L . --eval '(progn (require \\='nskk-transient-config) (require \\='nskk-transient-plugins) (require \\='nskk-transient-debug))'`
- 手動操作テスト
  - `nskk-menu` → 設定/プラグイン/デバッグを順に実行
  - それぞれのバッファ表示・設定反映・コマンド呼び出しの確認

## 設計上の特徴

- 外部ライブラリ依存ゼロの管理UI
- 既存の設定・プラグイン・デバッグ機能をそのまま利用可能
- トップレベル `nskk-menu` により拡張可能な運用導線を確保
- `completing-read` ベースのため、vertico/orderless等の補完フレームワークとも自然に連携
