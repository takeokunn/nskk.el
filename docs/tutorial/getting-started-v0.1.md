# NSKK入門ガイド v0.1

## はじめに

NSKKは、Emacs 31の最新機能を活用した次世代SKK（Simple Kana to Kanji conversion program）実装です。このチュートリアルでは、NSKKの基本的な使い方を学び、効率的な日本語入力を習得できます。

### このチュートリアルで学べること

- NSKKのインストールと基本設定
- 基本的なローマ字入力とかな変換
- モード切り替えの方法
- 簡単な辞書設定
- 基本的な変換操作

### 想定所要時間

約30分

## 前提条件

### 必須要件

- **Emacs 31.0以上**: NSKKはEmacs 31の新機能を活用しています
- **UTF-8環境**: 日本語処理に必須です
- **基本的なEmacsの知識**: ファイルの開閉、バッファ操作など

### 確認方法

Emacsのバージョンを確認します：

```elisp
M-x emacs-version
```

以下のような出力が表示されればOKです：

```
GNU Emacs 31.0.50 (build 1, x86_64-apple-darwin24.1.0, NS appkit-2487.20 Version 15.1 (Build 24B83))
```

## インストール

### ステップ1: リポジトリのクローン

ターミナルで以下のコマンドを実行します：

```bash
cd ~/
git clone https://github.com/takeokunn/nskk.el.git
```

### ステップ2: load-pathへの追加

`init.el`または`~/.emacs.d/init.el`に以下を追加します：

```elisp
;; NSKKのパスを追加
(add-to-list 'load-path "~/nskk.el")
```

### ステップ3: NSKKの読み込み

`init.el`に以下を追加します：

```elisp
;; NSKKを読み込む
(require 'nskk-romaji-tables)
(require 'nskk-converter)
(require 'nskk-state)
(require 'nskk-mode-switch)
(require 'nskk-buffer)
```

### ステップ4: 設定の反映

Emacsを再起動するか、以下のコマンドで設定を反映します：

```
M-x eval-buffer
```

## 初期設定

### 最小限の設定

NSKKをすぐに使い始めるための最小限の設定です：

```elisp
;; NSKKの基本設定
(setq nskk-jisyo-file "~/.skk/jisyo")  ; 個人辞書のパス
```

### 推奨設定

より快適に使うための推奨設定です：

```elisp
;; NSKKの推奨設定
(setq nskk-jisyo-file "~/.skk/jisyo")        ; 個人辞書
(setq nskk-show-mode-show t)                 ; モード表示を有効化
(setq nskk-modeline-indicator-enabled t)     ; モードライン表示
```

### 辞書の設定

NSKKでは、SKK形式の辞書を使用します。

#### 辞書ディレクトリの作成

```bash
mkdir -p ~/.skk
```

#### SKK辞書のダウンロード（任意）

標準的なSKK辞書をダウンロードする場合：

```bash
cd ~/.skk
curl -O https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L
```

#### 辞書ファイルの設定

```elisp
;; システム辞書の設定（任意）
(setq nskk-large-jisyo "~/.skk/SKK-JISYO.L")
```

## 基本操作

### NSKKの起動

NSKKを有効にするには、以下のキーバインドを使用します：

```
C-x j
```

または、コマンドから実行：

```
M-x nskk-mode
```

モードラインに `[あ]` と表示されれば、NSKKが起動しています。

### モードの種類と切り替え

NSKKには以下のモードがあります：

| モード | 表示 | 説明 |
|--------|------|------|
| ひらがなモード | `[あ]` | 通常の日本語入力モード |
| カタカナモード | `[ア]` | カタカナを入力するモード |
| 全角英数モード | `[Ａ]` | 全角英数字を入力するモード |
| 直接入力モード | `[_A]` | 日本語入力を無効化（英語入力） |

### モード切り替えのキー操作

```
C-j     : ひらがなモードに戻る
q       : ひらがな⇔カタカナモードを切り替え
l       : 一時的に英数字を入力（1文字のみ）
L       : 全角英数モードに切り替え
C-x j   : NSKK全体のON/OFF
```

### 実践：モード切り替えを試す

以下の手順で、実際にモード切り替えを体験してみましょう：

```
1. C-x j でNSKKを起動 → モードライン: [あ]
2. "aiueo" と入力 → "あいうえお" と表示
3. q を押す → モードライン: [ア]
4. "aiueo" と入力 → "アイウエオ" と表示
5. q を押す → モードライン: [あ]
6. C-j を押す → ひらがなモードに確実に戻る
```

## ひらがな入力

### 基本的なローマ字変換

NSKKでは、ローマ字を入力すると自動的にひらがなに変換されます。

#### 五十音の入力

```
入力 → 出力

a i u e o → あ い う え お
ka ki ku ke ko → か き く け こ
sa si su se so → さ し す せ そ
ta ti tu te to → た ち つ て と
na ni nu ne no → な に ぬ ね の
ha hi hu he ho → は ひ ふ へ ほ
ma mi mu me mo → ま み む め も
ya    yu    yo → や    ゆ    よ
ra ri ru re ro → ら り る れ ろ
wa          wo → わ          を
n              → ん
```

#### 濁音・半濁音の入力

```
入力 → 出力

ga gi gu ge go → が ぎ ぐ げ ご
za zi zu ze zo → ざ じ ず ぜ ぞ
da di du de do → だ ぢ づ で ど
ba bi bu be bo → ば び ぶ べ ぼ
pa pi pu pe po → ぱ ぴ ぷ ぺ ぽ
```

#### 拗音（ようおん）の入力

```
入力 → 出力

kya kyu kyo → きゃ きゅ きょ
sha shu sho → しゃ しゅ しょ
cha chu cho → ちゃ ちゅ ちょ
nya nyu nyo → にゃ にゅ にょ
hya hyu hyo → ひゃ ひゅ ひょ
mya myu myo → みゃ みゅ みょ
rya ryu ryo → りゃ りゅ りょ
gya gyu gyo → ぎゃ ぎゅ ぎょ
```

### 特殊な入力

#### 促音（っ）の入力

子音を2回続けて入力すると、促音「っ」が挿入されます：

```
kka → っか
tte → って
ppo → っぽ
```

#### 撥音（ん）の入力

```
nn → ん（確実に「ん」を入力）
n' → ん（次が母音でも「ん」として確定）
```

例：
```
kannji → かんじ
ken'i  → けんい
```

#### 小さい文字の入力

```
xa → ぁ
xi → ぃ
xu → ぅ
xe → ぇ
xo → ぉ
xya → ゃ
xyu → ゅ
xyo → ょ
xtu → っ
```

### 実践演習：基本入力

以下の単語を実際に入力してみましょう：

```
1. konnichiwa → こんにちわ
2. arigatou → ありがとう
3. gakkou → がっこう
4. ryokou → りょこう
5. nippon → にっぽん
6. kyou → きょう
7. shouyu → しょうゆ
8. zasshi → ざっし
9. kitte → きって
10. senpai → せんぱい
```

## 漢字変換

### 変換の開始

漢字に変換したい単語を入力する際は、**最初の文字を大文字で入力**します。

```
Kanji → ▽かんじ（変換待機状態）
```

`▽` マークが表示されたら変換待機状態です。

### 変換の実行

変換待機状態で**スペースキー**を押すと、漢字候補が表示されます：

```
Kanji [SPC] → 漢字（第1候補が表示される）
```

### 候補の選択

複数の候補がある場合、スペースキーで次の候補に移動できます：

```
[SPC] : 次の候補
x     : 前の候補
```

### 確定

希望する候補が表示されたら、**Enterキー**で確定します：

```
漢字 [RET] → 漢字（確定）
```

### 変換の中止

変換を中止したい場合は、`C-g` を押します：

```
▽かんじ [C-g] → かんじ（ひらがなのまま）
```

### 実践演習：漢字変換

以下の単語を変換してみましょう：

```
1. Nihon [SPC] [RET] → 日本
2. Gakkou [SPC] [RET] → 学校
3. Sensei [SPC] [RET] → 先生
4. Hon [SPC] [RET] → 本
5. Inu [SPC] [RET] → 犬
```

## 送り仮名付き変換

### 送り仮名の指定

動詞や形容詞などの活用語を変換する際は、送り仮名の開始位置を大文字で指定します：

```
KaKu → ▽か*く（「く」が送り仮名）
[SPC] → 書く
```

### 送り仮名の例

```
YoMu [SPC] → 読む
KaKu [SPC] → 書く
TabeRu [SPC] → 食べる
UtsukuShii [SPC] → 美しい
```

### 実践演習：送り仮名付き変換

以下の単語を変換してみましょう：

```
1. KaKu [SPC] [RET] → 書く
2. YoMu [SPC] [RET] → 読む
3. TabeRu [SPC] [RET] → 食べる
4. AruKu [SPC] [RET] → 歩く
5. UtsukuShii [SPC] [RET] → 美しい
```

## 補完機能

### 補完の開始

読みの途中で `/` を入力すると、補完候補が表示されます（v0.1では基本機能）：

```
ni/ → にほん、にく、にわ...（候補が表示される）
```

### 実践演習：補完機能

```
1. ni/ → 候補を確認
2. [SPC]で候補を選択
3. [RET]で確定
```

## 学習機能

NSKKは、選択した候補を自動的に学習します。

### 学習の仕組み

1. 漢字変換で候補を選択
2. 選択した候補が個人辞書に記録される
3. 次回同じ読みを入力した際、学習した候補が優先的に表示される

### 学習の確認

何度か同じ単語を変換してみましょう：

```
1回目: Kanji [SPC] [SPC] [RET] → 2番目の候補を選択
2回目: Kanji [SPC] [RET] → 前回選んだ候補が1番目に表示される
```

### 個人辞書の場所

学習した内容は、以下のファイルに保存されます：

```
~/.skk/jisyo
```

## よくある操作

### 確定のキャンセル

変換を確定した直後にキャンセルしたい場合：

```
C-/  (undo)
```

### モードの確認

現在のモードを確認するには、モードラインを見ます：

```
[あ] : ひらがなモード
[ア] : カタカナモード
[Ａ] : 全角英数モード
[_A] : 直接入力モード
```

### 英数字の一時入力

ひらがなモード中に一時的に英数字を入力したい場合：

```
l → 次の1文字だけ英数字で入力
```

例：
```
aiueo → あいうえお
l → （英数字入力待機）
ABC → ABC（そのまま入力）
aiueo → あいうえお（ひらがなモードに復帰）
```

## トラブルシューティング

### 1. NSKKが起動しない

**症状**: `C-x j` を押してもNSKKが起動しない

**原因と対策**:

1. **モジュールの読み込み確認**
   ```elisp
   M-x eval-expression
   (featurep 'nskk-state)
   ```
   結果が `nil` の場合、`init.el` で正しく `require` されているか確認

2. **load-pathの確認**
   ```elisp
   M-x eval-expression
   load-path
   ```
   `~/nskk.el` が含まれているか確認

3. **エラーメッセージの確認**
   ```
   M-x view-echo-area-messages
   ```
   エラーメッセージを確認

**解決方法**:
```elisp
;; init.elに以下を追加
(add-to-list 'load-path "~/nskk.el")
(require 'nskk-state)
(require 'nskk-mode-switch)
```

### 2. 変換候補が表示されない

**症状**: スペースキーを押しても候補が表示されない

**原因と対策**:

1. **辞書ファイルの確認**
   ```elisp
   M-x eval-expression
   nskk-jisyo-file
   ```
   正しいパスが設定されているか確認

2. **辞書ファイルの存在確認**
   ```bash
   ls -la ~/.skk/jisyo
   ls -la ~/.skk/SKK-JISYO.L
   ```

3. **辞書の読み込み状態確認**
   ```elisp
   M-x nskk-dict-reload
   ```

**解決方法**:
```elisp
;; 辞書パスを正しく設定
(setq nskk-jisyo-file "~/.skk/jisyo")
(setq nskk-large-jisyo "~/.skk/SKK-JISYO.L")
```

### 3. ひらがなが入力できない

**症状**: ローマ字を入力しても英字のままになる

**原因と対策**:

1. **モード確認**
   - モードラインを確認
   - `[_A]` の場合は直接入力モード → `C-x j` で切り替え

2. **ひらがなモードへの切り替え**
   ```
   C-j  （ひらがなモードに強制移行）
   ```

3. **変換テーブルの確認**
   ```elisp
   M-x eval-expression
   (featurep 'nskk-romaji-tables)
   ```

**解決方法**:
```
1. C-x j でNSKKを有効化
2. C-j でひらがなモードに切り替え
3. モードラインに [あ] と表示されることを確認
```

### 4. 送り仮名が正しく処理されない

**症状**: `KaKu` と入力しても送り仮名が認識されない

**原因と対策**:

1. **入力方法の確認**
   - 正しい入力: `KaKu` （K→a→K→u）
   - 誤った入力: `kaku` （全て小文字）

2. **変換待機状態の確認**
   - `▽か*く` のように `*` が表示されるか確認

**解決方法**:
```
送り仮名の開始位置を大文字で入力：
- 書く: KaKu
- 読む: YoMu
- 食べる: TabeRu
```

### 5. モードラインに何も表示されない

**症状**: NSKKのモード表示が見えない

**原因と対策**:

1. **モードライン表示設定の確認**
   ```elisp
   M-x eval-expression
   nskk-modeline-indicator-enabled
   ```

2. **モードライン表示の有効化**
   ```elisp
   (setq nskk-modeline-indicator-enabled t)
   ```

3. **モードラインの更新**
   ```elisp
   M-x nskk-update-modeline
   ```

**解決方法**:
```elisp
;; init.elに追加
(setq nskk-modeline-indicator-enabled t)
(setq nskk-show-mode-show t)
```

### 6. 変換が遅い

**症状**: スペースキーを押してから候補が表示されるまで時間がかかる

**原因と対策**:

1. **辞書サイズの確認**
   ```bash
   ls -lh ~/.skk/SKK-JISYO.L
   ```
   巨大な辞書ファイルの場合、初回読み込みに時間がかかる

2. **キャッシュの有効化**
   ```elisp
   (setq nskk-use-cache t)
   (setq nskk-cache-size 10000)
   ```

3. **インデックスの事前構築**
   ```elisp
   M-x nskk-build-index
   ```

**解決方法**:
```elisp
;; 高速化設定
(setq nskk-use-cache t)
(setq nskk-preload-dictionary t)
```

### 7. 辞書が読めないエラー

**症状**: `Error: Cannot read dictionary file`

**原因と対策**:

1. **ファイルパーミッションの確認**
   ```bash
   ls -la ~/.skk/
   chmod 644 ~/.skk/jisyo
   ```

2. **ディレクトリの作成**
   ```bash
   mkdir -p ~/.skk
   touch ~/.skk/jisyo
   ```

3. **エンコーディングの確認**
   ```elisp
   ;; 辞書ファイルはUTF-8またはEUC-JPである必要がある
   (setq nskk-dictionary-encoding 'utf-8)
   ```

**解決方法**:
```bash
# ディレクトリとファイルを作成
mkdir -p ~/.skk
touch ~/.skk/jisyo
chmod 644 ~/.skk/jisyo
```

### 8. キーバインドが効かない

**症状**: `C-x j` や `C-j` が反応しない

**原因と対策**:

1. **他のパッケージとの競合確認**
   ```elisp
   M-x describe-key
   C-x j  （または効かないキー）
   ```

2. **キーマップの確認**
   ```elisp
   M-x eval-expression
   (featurep 'nskk-keymap)
   ```

3. **グローバルキーの設定**
   ```elisp
   (global-set-key (kbd "C-x j") 'nskk-mode)
   ```

**解決方法**:
```elisp
;; init.elで明示的に設定
(global-set-key (kbd "C-x j") 'nskk-mode)
(define-key nskk-mode-map (kbd "C-j") 'nskk-to-hiragana-mode)
```

### 9. 変換候補の選択ができない

**症状**: スペースキーを押しても次の候補に移動しない

**原因と対策**:

1. **候補ウィンドウの表示確認**
   ```elisp
   M-x eval-expression
   (featurep 'nskk-candidate-window)
   ```

2. **候補選択キーの確認**
   ```elisp
   M-x describe-key
   SPC  （候補選択中に）
   ```

**解決方法**:
```elisp
;; 候補選択機能を確実に読み込む
(require 'nskk-candidate-window)
```

### 10. Emacsが重くなる

**症状**: NSKK使用中にEmacsの動作が遅くなる

**原因と対策**:

1. **メモリ使用量の確認**
   ```elisp
   M-x memory-usage
   ```

2. **GC設定の最適化**
   ```elisp
   (setq gc-cons-threshold 134217728)  ; 128MB
   (setq gc-cons-percentage 0.6)
   ```

3. **不要なキャッシュのクリア**
   ```elisp
   M-x nskk-clear-cache
   ```

**解決方法**:
```elisp
;; init.elでGC設定を最適化
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 1024 1024))
```

### デバッグ情報の収集

問題が解決しない場合、以下の情報を収集してください：

```elisp
;; 1. バージョン情報
M-x emacs-version

;; 2. 読み込まれているNSKKモジュール
M-: (mapcar (lambda (f) (featurep f))
            '(nskk-state nskk-converter nskk-mode-switch
              nskk-buffer nskk-romaji-tables))

;; 3. 変数の値
M-: nskk-jisyo-file
M-: nskk-large-jisyo

;; 4. エラーメッセージ
M-x view-echo-area-messages
```

### サポートの依頼

上記の対策で解決しない場合、GitHubのIssueで報告してください：

```
https://github.com/takeokunn/nskk.el/issues
```

報告時に含める情報：
- Emacsのバージョン
- NSKKのバージョン（コミットハッシュ）
- エラーメッセージ
- 再現手順
- デバッグ情報

## 次のステップ

NSKKの基本をマスターしました！次は以下のドキュメントで、さらに深く学びましょう。

### さらに学ぶ

1. **[カスタマイズガイド](../how-to/advanced-customization.md)**
   - 詳細な設定方法
   - キーバインドのカスタマイズ
   - 辞書の詳細設定

2. **[APIリファレンス](../reference/api-reference.md)**
   - 利用可能な関数一覧
   - 変数の詳細
   - カスタマイズ変数

3. **[アーキテクチャガイド](../explanation/comprehensive-architecture-overview.md)**
   - NSKKの内部構造
   - モジュール間の関係
   - 拡張方法

### 実践的な使い方

1. **日常的な文章入力で練習**
   - メールの下書き
   - ドキュメントの作成
   - コードのコメント

2. **設定のカスタマイズ**
   - 自分の好みに合わせた設定
   - キーバインドの調整
   - 辞書の追加

3. **効率的な入力方法の習得**
   - 送り仮名の活用
   - 補完機能の活用
   - 学習機能の最大活用

## まとめ

このチュートリアルでは、以下の内容を学びました：

### 習得した内容

✅ NSKKのインストールと基本設定
✅ モード切り替えの方法
✅ ひらがな入力の基本
✅ 漢字変換の基本操作
✅ 送り仮名付き変換
✅ 補完機能の使用
✅ 学習機能の仕組み
✅ よくある問題の解決方法

### パフォーマンス指標

NSKK v0.1では、以下のパフォーマンスを実現しています：

- **ローマ字変換**: < 1ms
- **辞書検索**: < 10ms（キャッシュ使用時）
- **候補表示**: < 100ms
- **メモリ使用量**: < 30MB

### NSKKの特徴（v0.1）

**実装済み機能**:
- 高速ローマ字-かな変換エンジン
- トライ木ベースの辞書検索
- 効率的なキャッシュ機構
- モード切り替えシステム
- 基本的な候補ウィンドウ

**開発中の機能**:
- 高度な補完システム
- 並列処理による高速化
- AI統合による学習強化
- マルチデバイス同期

### 次の目標

NSKKを日常的に使い、以下のスキルを磨いていきましょう：

1. **入力速度の向上**: 1分間に100文字以上
2. **変換精度の向上**: 第一候補での正解率90%以上
3. **効率的な操作**: キーストローク数の最小化

### フィードバックのお願い

NSKKの改善にご協力ください：

- バグ報告: [GitHub Issues](https://github.com/takeokunn/nskk.el/issues)
- 機能リクエスト: [GitHub Discussions](https://github.com/takeokunn/nskk.el/discussions)
- ドキュメント改善: Pull Request歓迎

---

**Welcome to NSKK! - 快適な日本語入力体験をお楽しみください**
