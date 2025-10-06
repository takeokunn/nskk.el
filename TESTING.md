# NSKKテストガイド

このドキュメントでは、NSKKのテスト方法とトラブルシューティングについて説明します。

## テスト実行方法

### 基本的なテスト

```bash
# 全テスト実行
make test

# 統合テストのみ
make test-integration

# E2Eテストのみ
make test-e2e

# E2E Setterテスト
make test-e2e-setter
```

### シナリオテスト

```bash
# 全シナリオテスト
make test-scenarios

# 基本シナリオのみ
make test-scenarios-basic

# 利用可能なシナリオをリスト表示
make list-scenarios
```

### Nix環境でのテスト

```bash
# 開発環境でのテスト
nix develop --command make test

# E2E Setterテストの実行
nix develop --command emacs --batch --load run-e2e-test.el

# カスタムテストスクリプトの実行
nix develop --command emacs --batch \
  --eval "(progn (add-to-list 'load-path \".\") (load \"nskk.el\"))" \
  --load your-test.el
```

## トラブルシューティング

### Setterエラー

もし以下のようなエラーが発生した場合:

```
Symbol's function definition is void: (setf nskk-state-input-buffer)
```

これは`nskk-state.el`のsetterが正しくロードされていないことを意味します。

**解決方法**:

1. `nskk-state.el`が最新版であることを確認
2. `gv-define-setter`が正しく定義されていることを確認
3. Emacsを再起動してキャッシュをクリア

### CLI環境での実行

`nix run .`はターミナル環境を必要とします。CI/CDやスクリプトから実行する場合:

```bash
# バッチモードでテスト
nix develop --command emacs --batch --load run-e2e-test.el

# または開発環境でmakeを使用
nix develop --command make test-e2e-setter
```

## テストファイル構成

- `tests/*-test.el` - ERT形式のユニットテスト
- `tests/scenarios/` - シナリオベーステスト
- `test-e2e-setter.el` - E2E Setterテスト
- `run-e2e-test.el` - E2Eテストランナー
- `test-batch-input.el` - バッチモード入力テスト
- `test-key-simulation.el` - キー入力シミュレーションテスト

## 新しいテストの追加

### ERTテストの追加

`tests/`ディレクトリに`*-test.el`ファイルを作成:

```elisp
(require 'ert)
(require 'nskk)

(ert-deftest my-test ()
  "テストの説明"
  (should (equal expected actual)))
```

### E2Eテストの追加

`test-e2e-setter.el`を参考に、新しいテストケースを追加してください。

## カバレッジ測定

```bash
# カバレッジ測定
make coverage

# HTMLレポートを開く
make coverage-report
```

## 継続的インテグレーション

GitHub Actionsでのテスト実行例:

```yaml
- name: Run tests
  run: nix develop --command make test

- name: Run E2E tests
  run: nix develop --command make test-e2e-setter
```
