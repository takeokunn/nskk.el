# NSKK.el

NSKK.el は Emacs 用の次世代 SKK 実装です。辞書検索や入力補助のコア機能に加え、
並列実行・非同期 UI・AI ベースの候補提案などを段階的に利用できます。

## 特徴

- 豊富な入力メソッドとカスタマイズ性
- 並列処理とキャッシュによる高速な辞書検索
- Transient ベースの操作 UI
- AI 候補提案 / 同期 / 分析機能 (任意)

## インストール

```elisp
(add-to-list 'load-path "/path/to/nskk.el")
(require 'nskk)
```

追加機能を個別に制御したい場合は次のモジュールをロードしてください。

```elisp
(require 'nskk-runtime-integration)   ; 並列処理/非同期UI/最適化
(require 'nskk-advanced-integration)  ; AI/同期/分析
```

## テスト

```sh
make test
```

## ライセンス

GNU General Public License v3 以降。
