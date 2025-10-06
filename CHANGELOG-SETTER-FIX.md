# Setter修正のChangelog

## 問題

CLI環境（`nix run .`）でNSKKモードを起動し、キー入力を行うと以下のエラーが発生:

```
nskk-application--set-input-buffer: Symbol's function definition is void: (setf nskk-state-input-buffer)
```

### 根本原因

`nskk-state`構造体の定義で、`cl-defstruct`がsetterを自動生成していなかった。

Emacs Lispの`cl-defstruct`はgetterは自動生成するが、setterは明示的に定義する必要がある。

## 解決策

### 1. `nskk-state.el`にsetterを追加

`gv-define-setter`を使用して、全てのスロットに対するsetterを明示的に定義:

```elisp
(gv-define-setter nskk-state-input-buffer (value state)
  `(setf (cl-struct-slot-value 'nskk-state 'input-buffer ,state) ,value))

;; 他のスロットについても同様...
```

これにより、以下のような自然な書き方が可能になった:

```elisp
(setf (nskk-state-input-buffer state) "new-value")
```

### 2. テストの追加

#### a. バッチモードテスト

- `test-batch-input.el` - 基本的なsetter動作確認
- `test-key-simulation.el` - キー入力シミュレーション

#### b. E2Eテスト

- `test-e2e-setter.el` - 完全なE2Eテスト
  - 基本的なsetter
  - 全スロットのsetter
  - アプリケーション層のsetter
  - append操作
  - reset操作

- `run-e2e-test.el` - E2Eテストランナー

#### c. Makefileターゲット

```bash
make test-e2e-setter
```

### 3. ドキュメントの追加

- `TESTING.md` - テストガイド
- このファイル (`CHANGELOG-SETTER-FIX.md`)

## テスト結果

全てのテストがパス:

```
=== NSKK End-to-End Setter Test ===

✓ [Basic Setter] Got: test1
✓ [All Slots] All 6 slots tested
✓ [Application Layer] Got: app-test
✓ [Append Operation] Got: ka
✓ [Reset Operation] Buffer reset successful

Total:  5
Passed: 5
Failed: 0

✓ ALL TESTS PASSED
```

## 影響範囲

### 変更ファイル

1. **nskk-state.el**
   - 全スロット用のsetterを追加（38行追加）

2. **Makefile**
   - `test-e2e-setter`ターゲットを追加

3. **.gitignore**
   - テストログファイルを追加

### 新規ファイル

- `test-batch-input.el`
- `test-key-simulation.el`
- `test-e2e-setter.el`
- `run-e2e-test.el`
- `init-test.el`
- `test-cli-input.el`
- `run-nix-test.sh`
- `TESTING.md`
- `CHANGELOG-SETTER-FIX.md`（このファイル）

## 後方互換性

この変更は完全に後方互換性があります。既存のコードは何も変更する必要がありません。

## 今後の推奨事項

1. 新しい構造体を定義する際は、必ずsetterも定義する
2. CI/CDパイプラインに`make test-e2e-setter`を追加
3. 定期的にCLI環境での動作確認を実施

## 参考

- Emacs Lisp Manual: Generalized Variables
- `cl-defstruct` documentation
- `gv-define-setter` documentation
