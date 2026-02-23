# NSKK セキュリティ設計チェックリスト・目標

## 概要

本ドキュメントはNSKKのセキュリティ設計方針と、実装時に確認すべきチェックリストを定義します。

## セキュリティ設計方針

### ゼロ依存のセキュリティ上の利点

NSKKはEmacs標準機能のみを使用し、外部パッケージへの依存を持ちません。

**利点**:
- サプライチェーン攻撃のリスクがない
- 依存関係の脆弱性が存在しない
- バージョン互換性問題を最小化できる

### Emacs組み込み機能の使用方針

| 機能 | 使用状況 | セキュリティ上の注意点 |
|------|----------|----------------------|
| スレッド | 使用予定 | Emacs 30+の協調的スレッド |
| ファイルI/O | 使用予定 | 権限チェックを実装すること |
| ネットワーク | 使用予定（サーバー接続） | TLS検証を実装すること |
| プロセス実行 | 不使用 | 外部プロセス実行を避ける |
| シェル実行 | 不使用 | シェル実行を避ける |

## セキュリティチェックリスト

### A01: アクセス制御

**対象**: ファイルシステムアクセス、辞書ファイル読み書き

**確認事項**:
- [ ] ファイル権限の適切なチェック
- [ ] ユーザーホームディレクトリ外へのアクセス制限
- [ ] シンボリックリンク攻撃の防止（`file-truename`によるパス正規化）

**設計例**:
```elisp
(defun nskk-dict-load-file (path)
  "辞書ファイルをロード。権限チェック付き。"
  ;; パス正規化（シンボリックリンク解決）
  (setq path (file-truename path))
  ;; ホームディレクトリ外へのアクセス防止
  (unless (string-prefix-p (expand-file-name "~/") path)
    (error "Dictionary must be in user home directory"))
  ;; 読み込み権限チェック
  (unless (file-readable-p path)
    (error "Cannot read dictionary file: %s" path))
  ;; 安全にロード
  (with-temp-buffer
    (insert-file-contents path)
    (nskk-dict-parse (buffer-string))))
```

### A02: 暗号化

**対象**: 現時点ではローカル辞書とオフライン利用のみを想定

**確認事項**:
- [ ] 将来ネットワーク通信を追加する場合はTLSを必須とする
- [ ] 学習データの暗号化はオプション機能として検討

### A03: インジェクション

**対象**: ユーザー入力処理、辞書データパース

**確認事項**:
- [ ] すべての入力に対する厳格な検証
- [ ] 正規表現による入力サニタイゼーション
- [ ] `eval`を使用しない
- [ ] `read`は信頼できるデータのみに制限

**設計例**:
```elisp
(defun nskk-validate-dict-entry (entry)
  "辞書エントリの検証。"
  (unless (string-match-p "\\`[ぁ-ん]+\\'" (car entry))
    (error "Invalid key: must be hiragana only"))
  (dolist (candidate (cdr entry))
    (unless (stringp candidate)
      (error "Invalid candidate: must be string"))
    (when (string-match-p "[<>\"']" candidate)
      (error "Invalid candidate: contains unsafe characters"))))

;; eval は使用しない
;; read は信頼できる設定ファイルのみ
(defun nskk-load-config (file)
  "設定ファイルをロード（read使用）。"
  ;; ファイルパス検証
  (unless (file-in-directory-p file (expand-file-name "~/.emacs.d/nskk/"))
    (error "Config file must be in NSKK directory"))
  ;; 読み取り専用モードで読み込み
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))
```

### A04: 安全な設計

**対象**: アーキテクチャ全体

**設計原則**:
1. **最小権限の原則**: 必要最小限の権限のみ要求
2. **深層防御**: 複数層のセキュリティチェック
3. **フェイルセーフ**: エラー時は安全側に倒す
4. **分離**: モジュール間の適切な境界
5. **検証**: すべての入力を検証

**確認事項**:
- [ ] 辞書ファイルは読み取り専用でオープン
- [ ] ネットワークエラー時は自動的にローカルにフォールバック
- [ ] スレッド間通信はメッセージパッシング（共有状態を避ける）

### A05: セキュリティの設定

**対象**: デフォルト設定、推奨設定

**デフォルト設定方針**:
- 学習データの自動保存: 有効（ユーザーディレクトリ内）
- サーバー接続のTLS検証: 有効
- ファイル権限: 0600（所有者のみ読み書き）
- デバッグモード: 無効

**推奨設定例**:
```elisp
(setopt nskk-server-tls-verify t)          ; TLS検証有効
(setopt nskk-dict-permission #o600)        ; ファイル権限
(setopt nskk-debug-mode nil)               ; デバッグモード無効
```

### A06: 脆弱で古いコンポーネント

**対象**: 依存関係

**確認事項**:
- [ ] 外部パッケージ依存がゼロであること
- [ ] 最小要求バージョン: Emacs 30+

### A07: 識別と認証

**対象**: サーバー接続（オプション機能）

**確認事項**:
- [ ] サーバー接続は完全にオプション
- [ ] TLS証明書検証の実装
- [ ] 接続失敗時のフォールバック（ローカル辞書）

### A08: データ整合性

**対象**: 辞書ファイル、設定ファイル

**確認事項**:
- [ ] 辞書ファイルのチェックサム検証（オプション）
- [ ] ファイル破損検出とリカバリ
- [ ] 自動バックアップ機能
- [ ] アトミックな書き込み操作

**設計例**:
```elisp
(defun nskk-dict-save-atomic (dict path)
  "辞書をアトミックに保存。"
  (let ((temp-file (make-temp-file "nskk-dict-")))
    ;; 一時ファイルに書き込み
    (with-temp-file temp-file
      (insert (nskk-dict-serialize dict))
      ;; チェックサム追加
      (goto-char (point-max))
      (insert (format "\n;; SHA256: %s\n"
                      (secure-hash 'sha256 (buffer-string)))))
    ;; 権限設定
    (set-file-modes temp-file #o600)
    ;; アトミックに移動（rename）
    (rename-file temp-file path t)))
```

### A09: セキュリティログ

**対象**: 異常検知

**確認事項**:
- [ ] セキュリティイベントのロギング
- [ ] 異常な入力パターンの検出
- [ ] オプトイン型ログ（デフォルトで詳細ログは無効）

**ログ対象イベント**:
- 認証失敗（サーバー接続）
- ファイルアクセスエラー
- 入力検証失敗
- 予期しない例外

**設計例**:
```elisp
(defun nskk-log-security-event (event-type details)
  "セキュリティイベントをログ。"
  (when nskk-security-logging-enabled
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
          (log-file (expand-file-name "~/.emacs.d/nskk/security.log")))
      (with-temp-buffer
        (insert (format "[%s] %s: %s\n" timestamp event-type details))
        (append-to-file (point-min) (point-max) log-file)))))
```

### A10: SSRF防止

**対象**: サーバー接続機能

**確認事項**:
- [ ] 接続先ホストのホワイトリスト
- [ ] プライベートIPアドレスへの接続禁止（オプション）
- [ ] ユーザー確認プロンプト（新規ホスト接続時）

**設計例**:
```elisp
(defun nskk-server-validate-host (host)
  "接続先ホストの検証。"
  ;; プライベートIPの検証
  (when nskk-server-block-private-ip
    (when (string-match-p
           "\\(^127\\.\\|^10\\.\\|^172\\.1[6-9]\\.\\|^172\\.2[0-9]\\.\\|^172\\.3[0-1]\\.\\|^192\\.168\\.\\)"
           host)
      (error "Connection to private IP is blocked")))
  ;; ホワイトリストチェック
  (unless (member host nskk-server-whitelist)
    (unless (y-or-n-p (format "Connect to new server %s? " host))
      (error "Connection rejected by user"))))
```

## 注意すべきセキュリティ問題

### タイミング攻撃への対策

学習データの比較処理では定数時間比較を使用すべきです。

```elisp
;; 定数時間比較の設計
(defun nskk-learning-compare (a b)
  (let ((len-a (length a))
        (len-b (length b))
        (result 0))
    (setq result (logxor result (logxor len-a len-b)))
    (dotimes (i (max len-a len-b))
      (setq result (logxor result
                          (logxor (if (< i len-a) (aref a i) 0)
                                  (if (< i len-b) (aref b i) 0)))))
    (zerop result)))
```

### 一時ファイルの権限

一時ファイル作成時はデフォルト権限ではなく、即座にファイル権限を制限すべきです。

```elisp
(defun nskk-create-temp-file ()
  "安全な一時ファイル作成。"
  (let ((temp-file (make-temp-file "nskk-")))
    (set-file-modes temp-file #o600)
    temp-file))
```

## セキュリティ改善案リスト

### 短期

1. 定数時間比較の実装
2. 一時ファイル権限の適切な設定
3. セキュリティログの実装

### 中期

4. ファイル署名検証機能
5. サーバーリクエストに対するレート制限
6. ファジングテストの整備

### 長期

7. セキュリティドキュメントの充実
8. CI/CDパイプラインへのセキュリティテスト統合

## 静的解析ツール

実装時に使用すべきツール:

- **byte-compile**: Emacs組み込みコンパイラ（警告ゼロを目標）
- **elint**: Emacs Lisp linter
- **package-lint**: パッケージ規約チェック

## オープンソースライセンス準拠

**ライセンス**: GPL v3+

**確認事項**:
- [ ] すべてのファイルにライセンスヘッダー記載
- [ ] COPYING ファイルの同梱
- [ ] サードパーティコードなし（外部依存ゼロ）

## 脆弱性報告方針

脆弱性報告の受付方法はリリース時に GitHub Issues またはリポジトリ内の SECURITY.md で定義する予定です。
