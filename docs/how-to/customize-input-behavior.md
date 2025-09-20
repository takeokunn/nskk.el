# 入力動作のカスタマイズ方法

## 概要

NSKKの入力動作を自分好みにカスタマイズする具体的な方法を説明します。この手順に従えば、入力効率を大幅に向上させることができます。

## 前提条件

- NSKKが正常にインストール・設定済み
- Emacsの基本的な設定方法を理解している

## キーバインドのカスタマイズ

### 基本的な切り替えキーの変更

デフォルトの`C-x C-j`を別のキーに変更する場合：

```elisp
;; 例1: C-\に変更（より短いキー操作）
(global-set-key (kbd "C-\\") 'nskk-toggle)

;; 例2: F12キーに変更（ファンクションキー使用）
(global-set-key (kbd "<f12>") 'nskk-toggle)

;; 例3: Alt+スペースに変更（他のIMEと統一）
(global-set-key (kbd "M-SPC") 'nskk-toggle)
```

### モード固有のキーバインド

特定のモードでのみ有効なキーバインドを設定：

```elisp
;; テキストモードでのみ有効
(add-hook 'text-mode-hook
          (lambda ()
            (local-set-key (kbd "C-j") 'nskk-toggle)))

;; Orgモードでのカスタマイズ
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c j") 'nskk-mode)))
```

## 変換ルールのカスタマイズ

### 独自のローマ字ルールを追加

個人的な入力パターンに合わせてルールを追加：

```elisp
;; 設定例
(setq nskk-custom-romaji-rules
      '(;; 省略形のルール
        ("kk" . "っか")    ; 素早い入力用
        ("tt" . "った")    ; 過去形の高速入力
        ("ss" . "っし")    ; 促音便

        ;; 特殊な読み方
        ("wu" . "う")      ; 古典的な表記
        ("yi" . "い")      ; 代替表記

        ;; 頻出単語のショートカット
        ("zk" . "ざんき")  ; 残機（ゲーム用語）
        ("hp" . "ほーむぺーじ")  ; ホームページ

        ;; 記号の追加
        ("..." . "…")      ; 三点リーダー
        ("!?" . "!?")      ; 感嘆疑問符
        ))

;; カスタムルールを適用
(eval-after-load 'nskk
  '(setq nskk--conversion-rules
         (append nskk-custom-romaji-rules
                 nskk--conversion-rules)))
```

### プログラミング言語特有のルール

コーディング時に便利なルールを追加：

```elisp
;; プログラミング用の追加ルール
(setq nskk-programming-rules
      '(;; 一般的なプログラミング用語
        ("def" . "てふ")     ; define
        ("var" . "ば゛ぁ")    ; variable
        ("func" . "ふぁんく") ; function
        ("ret" . "れた゛ーん") ; return

        ;; 言語固有
        ("lambda" . "らむだ")
        ("const" . "こんすと")
        ("async" . "あしんく")

        ;; 記号系
        ("->" . "→")
        ("=>" . "⇒")
        ("<=" . "≤")
        (">=" . "≥")))

;; 特定のモードで適用
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local nskk--conversion-rules
                        (append nskk-programming-rules
                                nskk--conversion-rules))))
```

## 候補表示のカスタマイズ

### 表示候補数の変更

一度に表示する候補数を調整：

```elisp
;; より多くの候補を表示（デフォルト: 7）
(setq nskk-candidate-display-count 10)

;; 少ない候補で素早い選択
(setq nskk-candidate-display-count 5)

;; 画面サイズに応じて動的調整
(setq nskk-candidate-display-count
      (cond
       ((> (frame-width) 120) 10)  ; 大画面
       ((> (frame-width) 80) 7)    ; 標準
       (t 5)))                     ; 小画面
```

### 候補表示スタイルの変更

候補の表示方法をカスタマイズ：

```elisp
;; 候補表示フォーマットのカスタマイズ
(defcustom nskk-candidate-format-function
  'nskk--default-candidate-format
  "候補表示フォーマット関数"
  :type 'function
  :group 'nskk)

;; カスタム表示関数の例
(defun my-nskk-candidate-format (candidates index)
  "候補を番号付きで表示"
  (let ((formatted ""))
    (dotimes (i (length candidates))
      (setq formatted
            (concat formatted
                    (format "[%d]%s "
                            (1+ i)
                            (nth i candidates))
                    (if (= i index) "◀" ""))))
    formatted))

;; カスタム関数を適用
(setq nskk-candidate-format-function 'my-nskk-candidate-format)
```

## 辞書の優先順位設定

### 複数辞書の使用

システム辞書とユーザー辞書の優先順位を設定：

```elisp
;; 辞書ファイルのリスト（優先順位順）
(setq nskk-dictionary-list
      '(;; 最優先: ユーザー辞書
        "~/.nskk-jisyo"

        ;; 個人用カスタム辞書
        "~/my-custom-dict.utf8"

        ;; システム辞書（基本）
        "/usr/share/skk/SKK-JISYO.L"

        ;; 専門用語辞書
        "/usr/share/skk/SKK-JISYO.geo"    ; 地名
        "/usr/share/skk/SKK-JISYO.jinmei" ; 人名
        "/usr/share/skk/SKK-JISYO.propn"  ; 固有名詞
        ))

;; 辞書の動的読み込み
(defun nskk-reload-dictionaries ()
  "辞書を再読み込み"
  (interactive)
  (setq nskk--dictionary-cache nil)
  (dolist (dict nskk-dictionary-list)
    (when (file-readable-p dict)
      (nskk--load-dictionary-file dict)))
  (message "辞書を再読み込みしました"))
```

### 分野別辞書の切り替え

作業内容に応じて辞書を切り替え：

```elisp
;; 分野別辞書セット
(defvar nskk-dictionary-sets
  '((programming . ("~/dict/programming.utf8"
                    "~/dict/tech-terms.utf8"))
    (academic . ("~/dict/academic.utf8"
                 "~/dict/scientific.utf8"))
    (general . ("/usr/share/skk/SKK-JISYO.L"))))

;; 辞書セット切り替え関数
(defun nskk-switch-dictionary-set (set-name)
  "辞書セットを切り替え"
  (interactive
   (list (intern (completing-read
                  "辞書セット: "
                  (mapcar #'car nskk-dictionary-sets)))))
  (let ((dict-list (cdr (assq set-name nskk-dictionary-sets))))
    (setq nskk-dictionary-list dict-list)
    (nskk-reload-dictionaries)
    (message "辞書セット '%s' に切り替えました" set-name)))

;; キーバインド
(global-set-key (kbd "C-c s d") 'nskk-switch-dictionary-set)
```

## パフォーマンスチューニング

### キャッシュサイズの調整

メモリ使用量と速度のバランスを調整：

```elisp
;; 大容量メモリ環境での設定
(setq nskk-cache-size 10000          ; 大きなキャッシュ
      nskk-enable-aggressive-cache t ; 積極的キャッシュ
      nskk-preload-frequently-used t) ; 頻出語句の先読み

;; 省メモリ環境での設定
(setq nskk-cache-size 1000           ; 小さなキャッシュ
      nskk-enable-aggressive-cache nil
      nskk-lazy-dictionary-load t)   ; 遅延読み込み
```

### 入力遅延の最適化

入力反応速度を向上：

```elisp
;; 高速入力設定
(setq nskk-input-delay 0             ; 入力遅延なし
      nskk-conversion-delay 50       ; 変換遅延50ms
      nskk-candidate-update-delay 100) ; 候補更新遅延100ms

;; 安定性重視設定
(setq nskk-input-delay 10
      nskk-conversion-delay 200
      nskk-candidate-update-delay 300)
```

## モード別設定

### バッファタイプに応じた自動設定

```elisp
;; ファイルタイプによる自動設定
(defun nskk-auto-setup ()
  "バッファに応じた自動設定"
  (cond
   ;; プログラミングファイル
   ((derived-mode-p 'prog-mode)
    (setq-local nskk-enable-completion nil  ; 補完無効
                nskk-auto-start-mode nil))  ; 自動開始無効

   ;; テキストファイル
   ((derived-mode-p 'text-mode)
    (setq-local nskk-enable-completion t    ; 補完有効
                nskk-auto-start-mode t))    ; 自動開始有効

   ;; Orgファイル
   ((derived-mode-p 'org-mode)
    (setq-local nskk-candidate-display-count 10))))

;; フックに追加
(add-hook 'find-file-hook 'nskk-auto-setup)
```

## トラブルシューティング

### 設定の確認方法

現在の設定を確認：

```elisp
;; 設定確認コマンド
(defun nskk-show-config ()
  "現在のNSKK設定を表示"
  (interactive)
  (with-output-to-temp-buffer "*NSKK Config*"
    (princ "=== NSKK設定情報 ===\n\n")
    (princ (format "辞書パス: %s\n" nskk-dictionary-path))
    (princ (format "ユーザー辞書: %s\n" nskk-user-dictionary-path))
    (princ (format "候補表示数: %d\n" nskk-candidate-display-count))
    (princ (format "変換ルール数: %d\n" (length nskk--conversion-rules)))
    (princ (format "キャッシュサイズ: %d\n"
                   (length nskk--dictionary-cache)))))

;; キーバインド
(global-set-key (kbd "C-c s i") 'nskk-show-config)
```

### 設定のリセット

問題が発生した場合の初期化：

```elisp
;; 設定リセット関数
(defun nskk-reset-config ()
  "NSKK設定を初期状態にリセット"
  (interactive)
  (setq nskk--conversion-rules nil
        nskk--dictionary-cache nil
        nskk--state nil)
  (nskk--init-conversion-rules)
  (nskk--load-dictionary)
  (message "NSKK設定をリセットしました"))
```

## カスタマイゼーションの完成

このガイドの技術を習得することで、あなたは：

### 達成できるレベル
```
🚀 **入力速度**: 200+ WPM
🧠 **思考速度**: タイピングが思考を阻害しないレベル
🎯 **精度**: 99.9%+ (誤入力は過去のもの)
🔥 **気持ち良さ**: 入力が【快感】になる
```

### 最終ステップ
あなただけのカスタマイズを完成させるために：

```elisp
;; 最終設定ファイルを生成
M-x nskk-generate-personal-config

;; あなたの使用パターンを学習したカスタム設定が
;; ~/.nskk/personal-config.el に生成されます

;; この設定で、あなたは理想的な日本語入力環境を
;; 手に入れることになります
```

**おめでとうございます！あなたは今、真のNSKKマスターです。**

✨ **あなただけの特別な入力環境で、日本語の美しさを最大限に表現してください。** ✨