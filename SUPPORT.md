# NSKK Support Guide

NSKKプロジェクトへようこそ! このガイドでは、サポートの受け方、問題の報告方法、コミュニティへの参加方法を説明します。

## 📚 ドキュメント

問題が発生した場合、まず以下のドキュメントを確認してください:

### 初心者向け

- **[Getting Started](docs/tutorial/getting-started.md)** - NSKKの基本的な使い方
- **[Troubleshooting Guide](docs/tutorial/troubleshooting.md)** - よくある問題と解決方法
- **[FAQ](docs/FAQ.md)** - よくある質問

### 中級者向け

- **[Advanced Customization](docs/how-to/advanced-customization.md)** - カスタマイズガイド
- **[Performance Tuning](docs/how-to/performance-tuning.md)** - パフォーマンス最適化
- **[Migration from ddskk](docs/tutorial/migration-from-ddskk.md)** - ddskk移行ガイド

### 上級者・開発者向け

- **[API Reference](docs/reference/api-reference.md)** - 完全APIドキュメント
- **[Architecture Overview](docs/explanation/comprehensive-architecture-overview.md)** - アーキテクチャ詳解
- **[Plugin Development](docs/how-to/plugin-development.md)** - プラグイン開発ガイド

## 💬 コミュニティサポート

### GitHub Discussions (推奨)

一般的な質問、使い方の相談、アイデアの共有は **GitHub Discussions** をご利用ください:

https://github.com/takeokunn/nskk.el/discussions

#### カテゴリ

- **Q&A**: 使い方の質問
- **Show and Tell**: カスタマイズ・プラグインの共有
- **Ideas**: 機能要望・提案
- **General**: その他の話題

### Reddit

- **Subreddit**: r/emacs
- **Tag**: [NSKK]

グローバルなEmacsコミュニティでの議論に参加できます。

### SNS

- **Twitter/X**: #NSKK ハッシュタグ
- **@takeokunn**: メンテナーのアカウント

## 🐛 バグレポート

### バグを報告する前に

1. **最新版を使用していますか?**
   ```elisp
   M-x nskk-version
   ```

2. **既知の問題ではありませんか?**
   - [Issue Tracker](https://github.com/takeokunn/nskk.el/issues) を検索
   - [Troubleshooting Guide](docs/tutorial/troubleshooting.md) を確認

3. **再現可能ですか?**
   - 最小限の設定で再現するか確認
   - `emacs -Q` で起動して試す

### バグレポートの作成

GitHub Issue Trackerでバグを報告:

https://github.com/takeokunn/nskk.el/issues/new

#### Bug Report Template

```markdown
## 環境情報

- **OS**: [e.g., macOS 14.5 / Ubuntu 24.04 / Windows 11]
- **Emacs version**: [M-x emacs-version の出力]
- **NSKK version**: [M-x nskk-version の出力]
- **インストール方法**: [MELPA / Git / その他]

## 問題の説明

[問題の簡潔な説明]

## 再現手順

1. [ステップ1]
2. [ステップ2]
3. [ステップ3]

## 期待される動作

[何が起こるべきか]

## 実際の動作

[実際に何が起こったか]

## エラーメッセージ

\`\`\`
[エラーメッセージやスタックトレースを貼り付け]
\`\`\`

## 追加情報

### 設定ファイル

\`\`\`elisp
[関連する設定を貼り付け]
\`\`\`

### 再現可能な最小設定

\`\`\`elisp
;; emacs -Q で実行
[最小限の設定]
\`\`\`

## スクリーンショット (任意)

[該当する場合、スクリーンショットを添付]
```

### 優先度の目安

- **Critical**: Emacsがクラッシュ、データ損失
- **High**: 主要機能が動作しない
- **Medium**: 一部機能に問題、回避策あり
- **Low**: マイナーな不具合、改善提案

## 💡 機能要望

新しい機能のリクエストは大歓迎です!

### Feature Request Template

https://github.com/takeokunn/nskk.el/issues/new

```markdown
## 機能の説明

[提案する機能の簡潔な説明]

## 動機・ユースケース

[なぜこの機能が必要か、どのような場面で使うか]

## 提案する実装方法 (任意)

[実装のアイデアがあれば]

## 代替案 (任意)

[他に考えられる解決方法]

## 追加コンテキスト

[その他の関連情報]
```

## 🔒 セキュリティ問題

セキュリティに関する問題は、**公開Issue Trackerを使用せず**、直接報告してください。

### 報告方法

**Email**: bararararatty@gmail.com
**件名**: [NSKK Security] [問題の簡潔な説明]

### セキュリティレポートに含めるべき情報

1. **脆弱性の種類**: [e.g., XSS, 情報漏洩, コード実行]
2. **影響範囲**: [どのバージョン、どの機能が影響を受けるか]
3. **再現手順**: [詳細な再現方法]
4. **影響**: [攻撃者が何を達成できるか]
5. **緩和策**: [一時的な回避方法があれば]

### 対応プロセス

1. **24時間以内**: 受領確認
2. **7日以内**: 初期評価と対応方針の連絡
3. **30日以内**: 修正版のリリース (深刻度による)
4. **リリース後**: 公開と謝辞

## 📖 ドキュメント改善

ドキュメントの誤り、不明瞭な説明、不足している情報があれば、ぜひ報告または改善してください。

### 報告方法

- **GitHub Issue**: ドキュメント問題として報告
- **Pull Request**: 直接修正を提案

### ドキュメント改善のガイドライン

1. **明確さ**: 初心者にも分かりやすく
2. **正確さ**: 最新バージョンに基づく
3. **例**: 具体的なコード例を含める
4. **構造**: Diátaxis framework準拠

## 🤝 コントリビューション

NSKKへの貢献方法については、[Contributing Guide](docs/how-to/contributing.md) を参照してください。

### コントリビューションの種類

- **コード**: バグ修正、新機能
- **ドキュメント**: 改善、翻訳
- **テスト**: テストケース追加
- **プラグイン**: エコシステムへの貢献
- **レビュー**: Pull Requestのレビュー

## 📞 直接サポート

### メンテナーへの連絡

**一般的な質問**: GitHub Discussions を使用してください
**バグ・機能要望**: GitHub Issues を使用してください
**セキュリティ問題**: bararararatty@gmail.com
**その他**: bararararatty@gmail.com

### 対応時間

- **営業時間**: 不定 (オープンソースプロジェクト)
- **タイムゾーン**: JST (UTC+9)
- **対応言語**: 日本語、英語

### 期待される応答時間

- **Critical bugs**: 24時間以内
- **High priority**: 3-5日以内
- **Medium/Low**: 1-2週間以内
- **Feature requests**: タイミングによる

## 🌐 言語サポート

### 対応言語

- **日本語**: フルサポート
- **English**: ドキュメント80%、コミュニティサポート有
- **中文**: 準備中

### 翻訳への貢献

ドキュメントの翻訳貢献を歓迎します! 詳細は [Contributing Guide](docs/how-to/contributing.md) を参照してください。

## 🔍 トラブルシューティング

### よくある問題

#### 1. NSKKが起動しない

**症状**: `M-x nskk-mode` がエラーになる

**解決方法**:
```elisp
;; 1. load-pathを確認
(member "~/.emacs.d/nskk.el" load-path)

;; 2. ファイルが存在するか確認
(file-exists-p "~/.emacs.d/nskk.el/nskk.el")

;; 3. バイトコンパイルエラーがないか確認
(require 'nskk)
```

#### 2. 辞書が読み込めない

**症状**: 変換候補が表示されない

**解決方法**:
```elisp
;; 1. 辞書ファイルが存在するか確認
(file-exists-p nskk-dictionary)
(file-exists-p nskk-large-dictionary)

;; 2. エンコーディングを確認
M-x nskk-dict-check-encoding

;; 3. 辞書を再読み込み
M-x nskk-reload-dictionary
```

#### 3. パフォーマンスが遅い

**解決方法**:
```elisp
;; 1. プロファイラーを実行
M-x nskk-profiler-start
;; ... 使用 ...
M-x nskk-profiler-report

;; 2. キャッシュを有効化
(setopt nskk-use-cache t)

;; 3. ネイティブコンパイルを確認
(native-comp-available-p)
```

詳細は [Troubleshooting Guide](docs/tutorial/troubleshooting.md) を参照してください。

## 📊 診断情報の収集

バグレポートや質問の際、以下のコマンドで診断情報を収集できます:

```elisp
;; 環境情報をコピー
M-x nskk-report-environment

;; パフォーマンスレポート
M-x nskk-profiler-export-report

;; デバッグログ
M-x nskk-export-debug-log
```

## 🎓 学習リソース

### 公式リソース

- **GitHub Wiki**: https://github.com/takeokunn/nskk.el/wiki
- **Examples**: https://github.com/takeokunn/nskk.el/tree/main/examples
- **Blog Posts**: (準備中)

### コミュニティリソース

- **Awesome NSKK**: プラグイン・カスタマイズ集 (準備中)
- **YouTube Tutorials**: (コミュニティ投稿歓迎)
- **Qiita/Zenn**: 日本語記事

### SKK一般

- **SKK Wiki**: http://openlab.ring.gr.jp/skk/
- **ddskk Manual**: https://github.com/skk-dev/ddskk

## 📜 Code of Conduct

NSKKプロジェクトは、すべての参加者にとって安全で歓迎的な環境を維持することに尽力しています。

### 期待される行動

- 他者を尊重する
- 建設的なフィードバック
- 多様性の尊重
- 協力的な態度

### 受け入れられない行動

- ハラスメント
- 攻撃的な言動
- スパム
- プライバシー侵害

詳細は [Code of Conduct](CODE_OF_CONDUCT.md) を参照してください。

## ⚖️ ライセンス

NSKKは **GNU General Public License v3.0 or later** の下でライセンスされています。

詳細は [LICENSE](LICENSE) を参照してください。

## 🙏 謝辞

NSKKプロジェクトへの皆様の支援に感謝します:

- **ユーザー**: フィードバックとバグレポート
- **コントリビューター**: コードとドキュメントの改善
- **ベータテスター**: v1.0への貴重な貢献
- **コミュニティ**: サポートと励まし

---

**Last Updated**: 2025-12-XX
**Version**: v1.0.0
**Maintainer**: takeokunn
**Contact**: bararararatty@gmail.com
