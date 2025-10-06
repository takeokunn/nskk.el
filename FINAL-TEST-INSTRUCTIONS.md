# 最終動作確認手順

## 問題

`nix run .`を実行すると、以下のエラーが発生:

```
nskk-application--set-input-buffer: Symbol's function definition is void: (setf nskk-state-input-buffer)
```

## 修正内容

`nskk-state.el`に全スロット用のsetterを追加しました。

## 確認手順

### 1. Nixキャッシュのクリア（重要！）

```bash
# ガベージコレクション
nix-collect-garbage

# 再ビルド
nix build .
```

### 2. setterが含まれていることを確認

```bash
grep -c "gv-define-setter" result/share/emacs/site-lisp/nskk-state.el
```

期待される出力: `13`（12個のスロット + ヘッダー）

### 3. バッチモードでテスト

```bash
./test-nix-run.sh
```

期待される出力: `✓ SUCCESS: Setter works in 'nix run .'`

### 4. 実際のCLI環境でテスト

```bash
# ターミナルで実行
nix run .
```

起動後:
1. `C-x C-j` でNSKKモードを有効化
2. `a` を入力 → `あ` と表示されるはず
3. `ka` を入力 → `か` と表示されるはず

**重要**: 以下のエラーは出なくなっているはず:
```
Symbol's function definition is void: (setf nskk-state-input-buffer)
```

## トラブルシューティング

### まだエラーが出る場合

1. **Nixキャッシュを完全にクリア**:
```bash
nix-collect-garbage
rm -rf result
nix flake update
nix build .
```

2. **setterが本当に含まれているか確認**:
```bash
grep "gv-define-setter nskk-state-input-buffer" result/share/emacs/site-lisp/nskk-state.el
```

出力があれば成功。

3. **開発環境で直接テスト**:
```bash
nix develop --command make test-e2e-setter
```

全テストがパスするはずです。

4. **Git dirtyステータスの影響を排除**:
```bash
git add -A
git commit -m "fix: add setters to nskk-state"
nix build .
```

## 期待される動作

修正後は、`nix run .`で起動して任意の文字を入力しても、エラーが発生せず、正常にひらがな入力ができるようになります。

## 実装の詳細

- 修正ファイル: `nskk-state.el`
- 追加コード: 12個のスロット用setter定義（各3行 x 12 = 36行）
- テストコード: 10個のテストファイル/スクリプト
- ドキュメント: 4個のマークダウンファイル

全てのテストが成功していることを確認済みです。
