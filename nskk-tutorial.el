;;; nskk-tutorial.el --- Interactive tutorial for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactive SKK tutorial for NSKK (Standalone, above Main).
;;
;; Layer position: Standalone (above Main) -- depends on nskk.el.
;;
;; Provides `M-x nskk-tutorial', a hybrid tutorial combining read-only
;; lesson explanations with interactive exercise sections that validate
;; user input.  A bundled mini dictionary ensures predictable conversion
;; results without requiring prior dictionary configuration.
;;
;; The Prolog database is saved on tutorial entry and restored on exit
;; (buffer kill), so the user's personal dictionary is never affected.
;;
;; Lesson plan (15 lessons):
;;
;;   基本編:
;;   1. はじめに -- NSKK起動とモード確認
;;   2. ひらがな入力 -- ローマ字からひらがなへ
;;   3. カタカナ入力 -- qキーでカタカナ切替
;;   4. 漢字変換（基本） -- 大文字で▽開始、SPCで変換
;;   5. 候補選択 -- SPC連打とxで候補ナビゲーション
;;   6. 送りあり変換 -- 大文字で送り仮名位置を指定
;;   7. 辞書登録 -- 未知語の登録フロー
;;   8. モード切替とまとめ -- 全モード復習
;;
;;   応用編:
;;   9. Abbrevモード -- 英字略語変換
;;  10. 動的補完 -- TABキーによるdcomp
;;  11. 候補リスト表示 -- A/S/D/F/J/K/L選択
;;  12. Stickyシフト -- ;キーによる疑似Shift
;;  13. 数値変換 -- #型コードによる数値変換
;;  14. AZIK拡張ローマ字 -- 効率的な入力方式
;;  15. 総合練習とヒント -- 応用テクニックまとめ

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk)
(require 'nskk-prolog)
(require 'nskk-state)
(require 'nskk-trie)

(declare-function nskk--set-mode "nskk-input")
(declare-function nskk-modeline-update "nskk-modeline")
(declare-function nskk-state-mode "nskk-state")
(declare-function nskk--dict-maybe-save "nskk-dictionary")


;;;;
;;;; Faces
;;;;

(defface nskk-tutorial-header-face
  '((t :inherit bold :height 1.2))
  "Face for lesson titles in the NSKK tutorial."
  :group 'nskk
  :package-version '(nskk . "0.1.0"))

(defface nskk-tutorial-instruction-face
  '((t :inherit italic))
  "Face for exercise instruction text."
  :group 'nskk
  :package-version '(nskk . "0.1.0"))

(defface nskk-tutorial-input-area-face
  '((((class color) (background light)) :background "#f0f0f0")
    (((class color) (background dark))  :background "#2a2a2a"))
  "Face for editable input areas in tutorial exercises."
  :group 'nskk
  :package-version '(nskk . "0.1.0"))

(defface nskk-tutorial-success-face
  '((t :foreground "green" :inherit bold))
  "Face for success feedback."
  :group 'nskk
  :package-version '(nskk . "0.1.0"))

(defface nskk-tutorial-hint-face
  '((t :foreground "gray50"))
  "Face for hint text."
  :group 'nskk
  :package-version '(nskk . "0.1.0"))


;;;;
;;;; Mini Dictionary
;;;;

(defconst nskk-tutorial--mini-dict
  '(;; Lesson 4: basic conversion
    ("かんじ"     . ("漢字" "感じ" "幹事"))
    ("へんかん"   . ("変換"))
    ("にほんご"   . ("日本語"))
    ("にほん"     . ("日本" "二本"))
    ;; Lesson 5: candidate selection
    ("ひと"       . ("人" "一"))
    ("がくせい"   . ("学生"))
    ;; Lesson 6: okurigana
    ("おおk"      . ("大"))
    ("ちいs"      . ("小"))
    ("かk"        . ("書" "掛"))
    ("よm"        . ("読"))
    ("みr"        . ("見"))
    ("いk"        . ("行"))
    ("たべr"      . ("食"))
    ;; Lesson 8: summary
    ("さくら"     . ("桜"))
    ("やま"       . ("山"))
    ("かわ"       . ("川" "河"))
    ("はな"       . ("花" "鼻"))
    ("そら"       . ("空"))
    ("うみ"       . ("海"))
    ("ひ"         . ("日" "火" "陽"))
    ("つき"       . ("月"))
    ("ほし"       . ("星"))
    ;; Lesson 10: dcomp (dynamic completion)
    ("かんけい"   . ("関係"))
    ("かんきょう" . ("環境"))
    ("かんたん"   . ("簡単"))
    ("さくぶん"   . ("作文"))
    ("にほんじん" . ("日本人"))
    ;; Lesson 11: candidate list selection
    ("こうえん"   . ("公園" "講演" "後援" "公演" "抗炎" "高遠" "広遠" "紅炎"))
    ;; Lesson 12: sticky shift
    ("でんわ"     . ("電話"))
    ("がっこう"   . ("学校"))
    ("はしr"      . ("走"))
    ("あるk"      . ("歩"))
    ;; Lesson 13: numeric conversion
    ("#にち"      . ("第#3日" "#1日"))
    ("#じ"        . ("第#3時" "#1時"))
    ("#こ"        . ("#0個" "#2個"))
    ;; Lesson 15: comprehensive practice
    ("ぶんしょう" . ("文章"))
    ("さくせい"   . ("作成"))
    ("かんせい"   . ("完成" "歓声" "感性"))
    ("おわr"      . ("終")))
  "Bundled mini dictionary for tutorial exercises.
Entries are asserted as `user-dict-entry/2' Prolog facts.")


;;;;
;;;; Lesson Data
;;;;

(defconst nskk-tutorial--lessons
  '(;; Lesson 1
    (:title "はじめに ── NSKKを起動しよう"
     :explanation
     "NSKKへようこそ！

NSKKは、SKK（Simple Kana to Kanji converter）方式の
日本語入力メソッドです。Emacsのバッファ内で直接
日本語を入力するためのマイナーモードとして動作します。

■ SKKと一般的なIMEの違い

一般的なIME（Google日本語入力、ATOK等）：
  → システムが自動で文節を区切り、変換候補を推測
  → 便利だが、意図しない区切りになることがある

SKK方式：
  → ユーザー自身が変換範囲を大文字キーで明示指定
  → 誤変換が少なく、慣れると非常に高速
  → 文法解析に依存しないシンプルな仕組み

■ NSKKの特徴
  ・Emacs Lispだけで完成（外部依存ゼロ）
  ・Prolog推論エンジンによる高速辞書検索
  ・DDSKK互換のキーバインドと操作体系

■ このチュートリアルの構成

  基本編（レッスン1〜8）：
    1. NSKKの起動とモード確認
    2. ひらがな入力
    3. カタカナ入力
    4. 漢字変換の基本
    5. 候補の選択
    6. 送りあり変換
    7. 辞書登録
    8. モード切替のまとめ

  応用編（レッスン9〜15）：
    9. Abbrevモード（英字略語変換）
   10. 動的補完（TABキー）
   11. 候補リスト表示
   12. Stickyシフト
   13. 数値変換
   14. AZIK拡張ローマ字
   15. 総合練習とヒント

現在、このバッファではNSKKが有効になっています。
モードラインに「かな」と表示されていれば、ひらがなモードです。

まず、モード切替の基本操作を試してみましょう。"
     :exercises
     ((:instruction "l キーでASCIIモードに切り替え、C-j でひらがなモードに戻ってください。"
       :hint "l → ASCIIモード → C-j → ひらがなモード"
       :expected nil
       :validator nskk-tutorial--validate-hiragana-mode)))

    ;; Lesson 2
    (:title "ひらがな入力"
     :explanation
     "ひらがなモードでは、ローマ字をタイプするだけで
ひらがなに変換されます。入力は確定的に行われるため、
変換キーを押す必要はありません。

■ 基本の50音

  a → あ    ka → か    sa → さ    ta → た    na → な
  ha → は    ma → ま    ya → や    ra → ら    wa → わ
  nn → ん

■ 濁音・半濁音

  ga → が    za → ざ    da → だ    ba → ば    pa → ぱ

■ 拗音（ようおん）

  kya → きゃ    sha → しゃ    cha → ちゃ
  nya → にゃ    hya → ひゃ    mya → みゃ
  rya → りゃ

■ ローマ字の複数表記

  si = shi → し    ti = chi → ち    tu = tsu → つ
  hu = fu  → ふ    zi = ji  → じ

■ 小さい文字（x プレフィックス）

  xa → ぁ    xi → ぃ    xu → ぅ    xe → ぇ    xo → ぉ
  xtu → っ    xya → ゃ    xyu → ゅ    xyo → ょ

■ 促音（っ）── 子音の連打

  kka → っか    tta → った    ppa → っぱ    ssi → っし"
     :exercises
     ((:instruction "「さくら」と入力してください。（sakura）"
       :hint "s a k u r a"
       :expected "さくら"
       :validator nil)
      (:instruction "「にほんご」と入力してください。（nihongo）"
       :hint "n i h o n g o"
       :expected "にほんご"
       :validator nil)
      (:instruction "「がっこう」と入力してください。（gakkou）"
       :hint "g a k k o u"
       :expected "がっこう"
       :validator nil)))

    ;; Lesson 3
    (:title "カタカナ入力"
     :explanation
     "ひらがなモードで q キーを押すと、カタカナモードに
切り替わります。もう一度 q を押すとひらがなに戻ります。

  q → カタカナモード（モードラインが「カナ」に変わる）
  q → ひらがなモード（モードラインが「かな」に戻る）

カタカナモードでの入力はひらがなと同じローマ字ですが、
結果がカタカナになります：

  ka → カ    sa → サ    ta → タ

また、▽モード（変換待ち）で q を押すと、
入力中のひらがなをカタカナに変換して確定できます。"
     :exercises
     ((:instruction "q でカタカナモードに切り替え、「カタカナ」と入力してください。"
       :hint "q → k a t a k a n a"
       :expected "カタカナ"
       :validator nil)
      (:instruction "q でひらがなモードに戻り、「ひらがな」と入力してください。"
       :hint "q → h i r a g a n a"
       :expected "ひらがな"
       :validator nil)))

    ;; Lesson 4
    (:title "漢字変換（基本）"
     :explanation
     "SKKで漢字変換をするには、変換したい語の先頭を
大文字で入力します。これがSKK最大の特徴です。

■ 3つの状態

  通常    → ひらがながそのまま確定される状態
  ▽モード → 見出し語（読み）を入力中の状態
  ▼モード → 変換候補を選択中の状態

■ 基本フロー

  ステップ1: 大文字キーで▽モード開始
    K を押す → ▽（見出し語入力開始）

  ステップ2: 読みを入力
    a n z i → ▽かんじ

  ステップ3: SPC で変換（▼モードへ）
    SPC → ▼漢字

  ステップ4: C-j で確定
    C-j → 漢字（確定、通常状態に戻る）

■ 暗黙の確定

  ▼モードで次の文字を打ち始めると、前の変換が
  自動的に確定されます：
    ▼漢字H → 「漢字」確定 → ▽（新規変換開始）

  これにより、いちいち C-j を押す必要がなくなり、
  連続した入力が高速になります。

■ キャンセル

  ▽モードで C-g → 入力を破棄して通常状態に戻る
  ▼モードで C-g → ▽モードに戻る（読みを再編集可能）"
     :exercises
     ((:instruction "「漢字」と変換して確定してください。"
       :hint "K a n z i SPC C-j"
       :expected "漢字"
       :validator nil)
      (:instruction "「変換」と変換して確定してください。"
       :hint "H e n k a n n SPC C-j"
       :expected "変換"
       :validator nil)
      (:instruction "「日本語」と変換して確定してください。"
       :hint "N i h o n g o SPC C-j"
       :expected "日本語"
       :validator nil)))

    ;; Lesson 5
    (:title "候補の選択"
     :explanation
     "一つの読みに対して複数の変換候補がある場合、
SPCキーを繰り返し押すことで次の候補を表示できます。

■ 候補の移動

  ▽かんじ → SPC → ▼漢字（1番目）
                → SPC → ▼感じ（2番目）
                → SPC → ▼幹事（3番目）

■ 前の候補に戻る

  x キーで1つ前の候補に戻れます：
    ▼幹事 → x → ▼感じ → x → ▼漢字

■ 確定とキャンセル

  C-j  → 現在の候補で確定
  RET  → 確定して改行
  C-g  → 候補選択をキャンセルして▽モードに戻る

■ 学習機能

  SKKの辞書は使うたびに学習します。確定した候補は
  次回から先頭に来るようになるため、よく使う語ほど
  少ないSPC回数で選べるようになります。"
     :exercises
     ((:instruction "「かんじ」を変換し、2番目の候補「感じ」を確定してください。"
       :hint "K a n z i SPC SPC C-j"
       :expected "感じ"
       :validator nil)
      (:instruction "「かんじ」を変換し、3番目の候補「幹事」を確定してください。"
       :hint "K a n z i SPC SPC SPC C-j"
       :expected "幹事"
       :validator nil)))

    ;; Lesson 6
    (:title "送りあり変換"
     :explanation
     "「大きい」「読む」のように活用語尾（送り仮名）がある語を
変換する場合、送り仮名の開始位置も大文字で指定します。

■ 基本的な流れ

  読む の場合：
    Y（大文字）→ ▽ 開始（語幹の先頭）
    o          → ▽よ
    M（大文字）→ 送り仮名開始 → 辞書検索発動
    u          → 読む（送り仮名付加＋確定）

  書く の場合：
    K（大文字）→ ▽ 開始
    a          → ▽か
    K（大文字）→ 送り仮名開始 → 辞書検索
    u          → 書く

■ 辞書の引き方

  辞書は「語幹の読み＋送り仮名の子音」で検索されます：
    書く  → 辞書キー「かk」
    読む  → 辞書キー「よm」
    見る  → 辞書キー「みr」
    行く  → 辞書キー「いk」
    食べる → 辞書キー「たべr」

■ 2文字以上の語幹

  大きい の場合：
    O o k I i  → 辞書キー「おおk」→ 大きい
  語幹が「おお」の2文字、送り仮名が「き」から始まります。

■ ポイント
  ・大文字は2回使う：1回目で▽開始、2回目で送り仮名位置を指定
  ・送り仮名の最初の文字（子音）を大文字にする
  ・母音で始まる送り仮名の場合もその母音を大文字にする"
     :exercises
     ((:instruction "「書く」と変換してください。"
       :hint "K a K u"
       :expected "書く"
       :validator nil)
      (:instruction "「読む」と変換してください。"
       :hint "Y o M u"
       :expected "読む"
       :validator nil)
      (:instruction "「見る」と変換してください。"
       :hint "M i R u"
       :expected "見る"
       :validator nil)))

    ;; Lesson 7
    (:title "辞書登録"
     :explanation
     "変換候補がすべて尽きると、辞書登録モードに入ります。
ミニバッファに「[辞書登録]」プロンプトが表示されます。

■ 辞書登録の流れ

  ステップ1: 候補が尽きると自動で登録モードへ
    ▽つくえ → SPC → （候補なし）→ [辞書登録] つくえ:

  ステップ2: 登録したい漢字を入力
    ミニバッファ内でもSKK変換が使えます。
    例: [辞書登録] つくえ: 机

  ステップ3: RET で確定・登録
    RET → 「机」がバッファに挿入され、辞書に追加

  ステップ4: 次回から使える
    ▽つくえ → SPC → ▼机（登録した候補が出る）

■ 登録をキャンセルする

  C-g で辞書登録をキャンセルして▽モードに戻れます。
  何も登録されません。

■ 再帰的な辞書登録

  ミニバッファ内でも SKK による変換が可能なため、
  登録語の入力中にさらに辞書登録に入ることがあります。
  nskk-max-registration-depth で最大ネスト深度を制御できます。

■ 注意事項

  ※ このチュートリアルでは、登録した内容はチュートリアル
    終了時に破棄されます。あなたの個人辞書には影響しません。

このレッスンでは、辞書登録の流れを説明しました。
実際の登録操作は、通常の使用時に体験してみてください。"
     :exercises
     ((:instruction "「学生」と変換して確定してください。"
       :hint "G a k u s e i SPC C-j"
       :expected "学生"
       :validator nil)))

    ;; Lesson 8
    (:title "モード切替とまとめ"
     :explanation
     "NSKKの全モードを復習しましょう。

  ■ かなモード（ひらがな入力）
    → C-j で他モードから戻れます

  ■ カナモード（カタカナ入力）
    → q でかなモードからトグル

  ■ ASCIIモード（半角英数）
    → l で切り替え
    → C-j でかなモードに戻る

  ■ 全角英数モード（JIS X 0208）
    → L（大文字）で切り替え
    → C-j でかなモードに戻る

  ■ Abbrevモード（英字略語変換）
    → / で切り替え
    → 英字を入力して SPC で辞書検索

以上で基本操作は完了です。レッスン9以降では、
SKKの応用的な機能を紹介します。"
     :exercises
     ((:instruction "「桜」と変換して確定してください。"
       :hint "S a k u r a SPC C-j"
       :expected "桜"
       :validator nil)
      (:instruction "「海」と変換して確定してください。"
       :hint "U m i SPC C-j"
       :expected "海"
       :validator nil)))

    ;; Lesson 9: Abbrev mode
    (:title "Abbrevモード ── 英字略語変換"
     :explanation
     "Abbrevモードは、英字の略語や英単語を辞書で検索して
変換する機能です。

使い方：
  1. / キーを押してAbbrevモードに入る
     → ▽ マークが表示されます
  2. 英字をそのまま入力する
     → ローマ字→ひらがな変換は行われません
     → 例: ▽file, ▽TCP, ▽http
  3. SPC で辞書検索して変換
  4. C-j で確定

Abbrevモードが便利な場面：
  ・「file」→「ファイル」のような英和辞書変換
  ・「TCP」→「伝送制御プロトコル」のような略語展開
  ・プログラミング用語の日本語変換

Abbrevモードでは、l や q などのモード切替キーも
通常の文字として入力されます。

C-g を押すと変換をキャンセルして元に戻れます。
C-j を押すとAbbrevモードを抜けてひらがなに戻ります。"
     :exercises
     ((:instruction "/ でAbbrevモードに入り、C-j でひらがなに戻ってください。"
       :hint "/ → C-j"
       :expected nil
       :validator nskk-tutorial--validate-hiragana-mode)
      (:instruction "「漢字」と変換して確定してください。"
       :hint "K a n z i SPC C-j"
       :expected "漢字"
       :validator nil)))

    ;; Lesson 10: Dynamic completion (dcomp)
    (:title "動的補完（TABキー）"
     :explanation
     "▽モード（見出し語入力中）で TAB キーを押すと、
入力途中の読みから候補を自動補完できます。
これを「動的補完（dcomp）」と呼びます。

例：「かん」まで入力した状態で TAB を押すと、
  ▽かん → TAB → ▽かんじ（辞書の最初の一致）
  → TAB → ▽かんけい（次の一致）
  → TAB → ▽かんきょう（さらに次）

使い方：
  1. 大文字で変換を開始（▽モードに入る）
  2. 読みの先頭部分を入力
  3. TAB を押して補完候補を表示
  4. TAB を繰り返して候補を巡回
  5. 目的の読みが出たら SPC で変換、C-j で確定

dcompには2つのスタイルがあります（nskk-dcomp-style）：
  capf  ── completion-at-point と連携（corfu/company対応）
  cycle ── 従来のTABサイクル（DDSKK互換）

長い読みを毎回フルタイプする手間が省けるため、
日常的な入力効率が大きく向上します。"
     :exercises
     ((:instruction "「漢字」と変換して確定してください。"
       :hint "K a n z i SPC C-j"
       :expected "漢字"
       :validator nil)
      (:instruction "「関係」と変換して確定してください。"
       :hint "K a n k e i SPC C-j"
       :expected "関係"
       :validator nil)
      (:instruction "「環境」と変換して確定してください。"
       :hint "K a n k y o u SPC C-j"
       :expected "環境"
       :validator nil)))

    ;; Lesson 11: Candidate list selection
    (:title "候補リスト表示（A/S/D/F/J/K/L）"
     :explanation
     "候補が多い語では、SPC を何度も押すのは面倒です。
NSKKでは、一定回数SPC を押すと候補リストが
自動的に表示されます。

動作の流れ：
  1. SPC を数回押す → 1つずつ候補が表示される
  2. nskk-henkan-show-candidates-nth 回目以降 →
     候補リストが一覧表示される
  3. リスト内のキーで直接選択：
     a → 1番目    s → 2番目    d → 3番目
     f → 4番目    j → 5番目    k → 6番目
     l → 7番目

候補リスト表示中の操作：
  SPC      → 次のページへ
  x        → 前のページへ
  C-g      → 候補リストを閉じて▽に戻る
  a〜l     → 対応する候補を確定

デフォルトでは、5回SPC を押すと候補リストに
切り替わります。この回数は
nskk-henkan-show-candidates-nth で変更できます。

1ページに表示する候補数は
nskk-henkan-number-to-display-candidates（デフォルト7）
で変更でき、選択キーも
nskk-henkan-show-candidates-keys でカスタマイズ可能です。"
     :exercises
     ((:instruction "「公園」と変換して確定してください。"
       :hint "K o u e n n SPC C-j"
       :expected "公園"
       :validator nil)
      (:instruction "「花」と変換して確定してください。"
       :hint "H a n a SPC C-j"
       :expected "花"
       :validator nil)))

    ;; Lesson 12: Sticky shift
    (:title "Stickyシフト（;キー）"
     :explanation
     "SKKでは大文字キーが重要な役割を持ちますが、
Shiftキーを頻繁に押すのは指への負担になります。

Stickyシフトを使うと、; キーが「次の1文字を大文字にする」
疑似Shiftとして機能します。

通常のSKK入力：
  Kanji      → ▽かんじ    （Shiftを押しながらK）
  KaKu       → 書く        （Shiftを2回）

Stickyシフト使用時：
  ;kanji     → ▽かんじ    （;を押してからk）
  ;ka;ku     → 書く        （;を2回使用）

利点：
  ・Shiftキーを押す必要がなくなる
  ・ホームポジションから手を動かさずに入力可能
  ・特に送りあり変換で効果を発揮

設定方法：
  DDSKKでは (setq skk-sticky-key \";\") で有効化します。
  NSKKでも同様の設定が可能です。

注意：; 自体を入力したい場合は ;; と2回押します。"
     :exercises
     ((:instruction "「電話」と変換して確定してください。"
       :hint "D e n w a を大文字開始で入力 → SPC C-j"
       :expected "電話"
       :validator nil)
      (:instruction "「学校」と変換して確定してください。"
       :hint "G a k k o u を大文字開始で入力 → SPC C-j"
       :expected "学校"
       :validator nil)))

    ;; Lesson 13: Numeric conversion
    (:title "数値変換（#）"
     :explanation
     "SKKには数値を含む変換を行う機能があります。
読みに # を含む辞書エントリと、入力中の数字が
組み合わさって変換されます。

数値変換の型コード：
  #0 ── 無変換（そのまま）        1 → 1
  #1 ── 全角数字                  1 → １
  #2 ── 漢数字（桁ごと）          1024 → 一〇二四
  #3 ── 漢数字（位取り）          1024 → 千二十四
  #8 ── カンマ区切り              1024 → 1,024

使い方の例：
  辞書に「#にち」→「第#3日」がある場合：
    入力: Q3Niti → ▽3にち → SPC → 第三日

  辞書に「#こ」→「#0個」がある場合：
    入力: Q5Ko → ▽5こ → SPC → 5個

数値部分は入力した数字に応じて自動的に変換されます。
型コード #3 は位取りで「千」「万」「億」等を付けるため、
年号や金額の入力に特に便利です。

  例: 2026nen → 二千二十六年（辞書に #nen→#3年）

この機能を使うことで、数字を含む定型表現を
効率的に入力できます。"
     :exercises
     ((:instruction "「漢字」と変換して確定してください。"
       :hint "K a n z i SPC C-j"
       :expected "漢字"
       :validator nil)
      (:instruction "「日本語」と変換して確定してください。"
       :hint "N i h o n g o SPC C-j"
       :expected "日本語"
       :validator nil)))

    ;; Lesson 14: AZIK extended romaji
    (:title "AZIK拡張ローマ字"
     :explanation
     "AZIK（エイズィック）は、ローマ字入力のキーストローク数を
削減する拡張ローマ字方式です。標準SKKローマ字と互換性を
保ちつつ、頻出パターンを少ないキーで入力できます。

有効化：
  (setq nskk-converter-romaji-style 'azik)

■ 撥音（ん）拡張 ── 子音の後に特定キーで「ん」を付加

  標準: k a n n → かん（4打鍵）
  AZIK: k a z   → かん（3打鍵）

  サフィックスキーとその効果：
    z → ん       例: kaz → かん、taz → たん
    k → んk      例: kak → かんか（「ん」+次のカ行子音）
    j → んj      例: kaj → かんじゃ
    d → んd      例: kad → かんだ
    l → んl      例: kal → かんら

■ 二重母音拡張 ── 母音の連続を1キーで

  h → 母音直後で「う」付加
    kah → かう    toh → とう    suh → すう
  ; → っ（促音）
    ka; → かっ

■ 拗音の簡略化

  標準: s h a → しゃ（3打鍵）
  AZIK: x a   → しゃ（2打鍵）

  x 系:  xa→しゃ  xi→し  xu→しゅ  xe→しぇ  xo→しょ
  c 系:  ca→ちゃ  ci→ち  cu→ちゅ  ce→ちぇ  co→ちょ

■ JP106キーボード固有

  + キー（Shift+;）→ っ＋送り仮名トリガー
  @ キー         → AZIKモード切替

■ キーボードタイプ

  nskk-azik-keyboard-type で jp106/us101 を選択できます。
  キーボードによって一部のキー割り当てが変わります。

AZIKは慣れが必要ですが、マスターすると日本語入力の
打鍵数を約20%削減できると言われています。"
     :exercises
     ((:instruction "「漢字」と変換して確定してください。"
       :hint "K a n z i SPC C-j"
       :expected "漢字"
       :validator nil)
      (:instruction "「日本語」と変換して確定してください。"
       :hint "N i h o n g o SPC C-j"
       :expected "日本語"
       :validator nil)))

    ;; Lesson 15: Comprehensive practice and tips
    (:title "総合練習とヒント"
     :explanation
     "最後に、これまで学んだ機能を組み合わせて練習しましょう。

■ 効率的な入力のコツ

1. 暗黙の確定を活用する
   変換後に次の文字を打ち始めると自動確定されます。
   いちいち C-j を押す必要はありません。
     ▼漢字K → 「漢字」確定 → ▽ 新規変換開始

2. ▽モードで q を押すとカタカナ確定
   カタカナ語は変換不要です：
     Katakana → ▽かたかな → q → カタカナ（確定）

3. x で戻る、C-g でキャンセル
   ▼モードで x → 前の候補
   ▼モードで C-g → ▽モードに戻る
   ▽モードで C-g → 入力前に戻る

4. C-j は万能な「戻る」キー
   どのモードでも C-j でひらがなモードに戻れます。

■ カスタマイズ

  nskk-henkan-show-candidates-nth  候補リスト表示までのSPC回数
  nskk-dcomp-style                 動的補完スタイル（capf/cycle）
  nskk-converter-romaji-style      AZIK等の拡張ローマ字
  nskk-state-default-mode          起動時のデフォルトモード
  nskk-azik-keyboard-type          AZIKのキーボードタイプ

■ 関連パッケージとの連携

  corfu / company     → dcomp の capf スタイルで連携
  ddskk-posframe     → 候補表示をposframeで表示
  orderless          → 補完スタイルの拡張

おめでとうございます！NSKKの基本から応用までを一通り
学びました。SKKは使い込むほど手に馴染む入力方式です。
実際のファイル編集で使ってみてください。"
     :exercises
     ((:instruction "「文章」と変換して確定してください。"
       :hint "B u n s h o u SPC C-j"
       :expected "文章"
       :validator nil)
      (:instruction "「作成」と変換して確定してください。"
       :hint "S a k u s e i SPC C-j"
       :expected "作成"
       :validator nil)
      (:instruction "「完成」と変換して確定してください。"
       :hint "K a n s e i SPC C-j"
       :expected "完成"
       :validator nil))))
  "List of tutorial lessons.
Each lesson is a plist with :title, :explanation, and :exercises.
Each exercise is a plist with :instruction, :hint, :expected, and :validator.")


;;;;
;;;; Buffer-local State
;;;;

(defvar-local nskk-tutorial--current-lesson 0
  "Index of the currently displayed lesson (0-based).")

(defvar-local nskk-tutorial--exercise-markers nil
  "List of (BEG-MARKER . END-MARKER) pairs for exercise input areas.")

(defvar-local nskk-tutorial--exercise-states nil
  "Vector of completion states for exercises in the current lesson.
Each element is t (completed) or nil (pending).")

(defvar-local nskk-tutorial--result-markers nil
  "List of (BEG-MARKER . END-MARKER) pairs for result display areas.")

(defvar-local nskk-tutorial--saved-prolog-db nil
  "Saved Prolog database hash table for restoration on exit.")

(defvar-local nskk-tutorial--saved-prolog-idx nil
  "Saved Prolog index config for restoration on exit.")

(defvar-local nskk-tutorial--saved-prolog-hash nil
  "Saved Prolog hash indices for restoration on exit.")

(defvar-local nskk-tutorial--saved-prolog-trie nil
  "Saved Prolog trie indices for restoration on exit.")

(defvar-local nskk-tutorial--saved-prolog-counter nil
  "Saved Prolog variable counter for restoration on exit.")

(defvar-local nskk-tutorial--saved-user-dict nil
  "Saved user dict index for restoration on exit.")

(defvar-local nskk-tutorial--saved-system-dict nil
  "Saved system dict index for restoration on exit.")

(defvar-local nskk-tutorial--saved-init-flags nil
  "Alist of saved initialization flags for restoration on exit.")


;;;;
;;;; Prolog DB Isolation
;;;;

(defun nskk-tutorial--copy-hash-table (ht)
  "Deep-copy hash table HT for Prolog DB isolation.
Lists are shallow-copied.  Trie structures and nested hash tables
are recursively deep-copied."
  (let ((new (make-hash-table :test (hash-table-test ht)
                              :size (hash-table-size ht))))
    (maphash (lambda (k v)
               (puthash k (pcase v
                            ((pred nskk-trie-p)
                             (nskk-tutorial--copy-trie v))
                            ((pred hash-table-p)
                             (nskk-tutorial--copy-hash-table v))
                            ((pred sequencep)
                             (copy-sequence v))
                            (_ v))
                        new))
             ht)
    new))

(defun nskk-tutorial--copy-trie-node (node)
  "Return a deep copy of trie NODE and all descendants."
  (let ((new (nskk-trie-node--create
              :is-end  (nskk-trie-node-is-end node)
              :value   (nskk-trie-node-value node)
              :count   (nskk-trie-node-count node))))
    (when-let* ((children (nskk-trie-node-children node)))
      (let ((new-children (make-hash-table :test 'eq
                                           :size (hash-table-count children))))
        (maphash (lambda (ch child)
                   (puthash ch (nskk-tutorial--copy-trie-node child)
                            new-children))
                 children)
        (setf (nskk-trie-node-children new) new-children)))
    new))

(defun nskk-tutorial--copy-trie (trie)
  "Return a deep copy of TRIE."
  (nskk-trie--create-internal
   :root (nskk-tutorial--copy-trie-node (nskk-trie-root trie))
   :size (nskk-trie-size trie)))

(defun nskk-tutorial--save-prolog-state ()
  "Save current Prolog database state for later restoration."
  (setq nskk-tutorial--saved-prolog-db
        (nskk-tutorial--copy-hash-table nskk--prolog-database)
        nskk-tutorial--saved-prolog-idx
        (nskk-tutorial--copy-hash-table nskk--prolog-index-config)
        nskk-tutorial--saved-prolog-hash
        (nskk-tutorial--copy-hash-table nskk--prolog-hash-indices)
        nskk-tutorial--saved-prolog-trie
        (nskk-tutorial--copy-hash-table nskk--prolog-trie-indices)
        nskk-tutorial--saved-prolog-counter
        nskk--prolog-var-counter
        nskk-tutorial--saved-user-dict
        (and (boundp 'nskk--user-dict-index) nskk--user-dict-index)
        nskk-tutorial--saved-system-dict
        (and (boundp 'nskk--system-dict-index) nskk--system-dict-index)
        nskk-tutorial--saved-init-flags
        (mapcar (lambda (sym)
                  (cons sym (and (boundp sym) (symbol-value sym))))
                '(nskk--input-initialized
                  nskk--state-prolog-initialized
                  nskk--henkan-initialized
                  nskk--kana-initialized
                  nskk--converter-initialized
                  nskk--candidate-key-facts-initialized
                  nskk--annotation-initialized))))

(defun nskk-tutorial--restore-prolog-state ()
  "Restore previously saved Prolog database state."
  (when nskk-tutorial--saved-prolog-db
    (setq nskk--prolog-database    nskk-tutorial--saved-prolog-db
          nskk--prolog-index-config nskk-tutorial--saved-prolog-idx
          nskk--prolog-hash-indices nskk-tutorial--saved-prolog-hash
          nskk--prolog-trie-indices nskk-tutorial--saved-prolog-trie
          nskk--prolog-var-counter  nskk-tutorial--saved-prolog-counter)
    (when (boundp 'nskk--user-dict-index)
      (setq nskk--user-dict-index nskk-tutorial--saved-user-dict))
    (when (boundp 'nskk--system-dict-index)
      (setq nskk--system-dict-index nskk-tutorial--saved-system-dict))
    ;; Restore initialization flags
    (pcase-dolist (`(,sym . ,val) nskk-tutorial--saved-init-flags)
      (when (boundp sym)
        (set sym val)))
    ;; Re-derive database-tails from the restored database
    (let ((new-tails (make-hash-table :test 'equal :size 128)))
      (maphash (lambda (k v)
                 (when v (puthash k (last v) new-tails)))
               nskk--prolog-database)
      (setq nskk--prolog-database-tails new-tails))
    (setq nskk-tutorial--saved-prolog-db nil)))

(defun nskk-tutorial--install-mini-dict ()
  "Install the mini dictionary for tutorial exercises.
Asserts `dict-initialized' first to prevent real dictionary loading,
then asserts tutorial entries as `user-dict-entry/2' facts."
  (nskk-prolog-assert '((dict-initialized)))
  (nskk-prolog-retract-all 'user-dict-entry 2)
  (nskk-prolog-set-index 'user-dict-entry 2 :trie)
  (pcase-dolist (`(,reading . ,candidates) nskk-tutorial--mini-dict)
    (nskk-prolog-assert
     `((user-dict-entry ,reading ,candidates)))))


;;;;
;;;; Exercise Validators
;;;;

(defun nskk-tutorial--validate-hiragana-mode ()
  "Validate that the current mode is hiragana.
Returns t if nskk-mode is active and the current mode is hiragana."
  (and nskk-mode
       (boundp 'nskk-current-state)
       nskk-current-state
       (eq (nskk-state-mode nskk-current-state) 'hiragana)))


;;;;
;;;; Buffer Rendering
;;;;

(defconst nskk-tutorial--buffer-name "*NSKK Tutorial*"
  "Name of the tutorial buffer.")

(defun nskk-tutorial--header-line ()
  "Return the header-line string for the tutorial buffer."
  (let* ((lesson (nth nskk-tutorial--current-lesson nskk-tutorial--lessons))
         (title (plist-get lesson :title))
         (n (1+ nskk-tutorial--current-lesson))
         (total (length nskk-tutorial--lessons)))
    (format "  NSKK チュートリアル  [レッスン %d/%d]  ─  %s" n total title)))

(defun nskk-tutorial--render-lesson ()
  "Render the current lesson in the tutorial buffer."
  (let* ((lesson (nth nskk-tutorial--current-lesson nskk-tutorial--lessons))
         (title (plist-get lesson :title))
         (explanation (plist-get lesson :explanation))
         (exercises (plist-get lesson :exercises))
         (inhibit-read-only t))
    ;; Clear buffer
    (erase-buffer)
    (setq nskk-tutorial--exercise-markers nil
          nskk-tutorial--result-markers nil
          nskk-tutorial--exercise-states
          (make-vector (length exercises) nil))
    ;; Title
    (let ((start (point)))
      (insert (format "\n━━━ レッスン %d: %s ━━━\n\n"
                      (1+ nskk-tutorial--current-lesson) title))
      (add-text-properties start (point)
                           `(face nskk-tutorial-header-face read-only t)))
    ;; Explanation
    (let ((start (point)))
      (insert explanation "\n\n")
      (add-text-properties start (point) '(read-only t)))
    ;; Exercises
    (let ((start (point)))
      (insert "━━━ 練習 ━━━\n\n")
      (add-text-properties start (point)
                           `(face nskk-tutorial-header-face read-only t)))
    (dotimes (i (length exercises))
      (let ((ex (nth i exercises)))
        ;; Instruction
        (let ((start (point)))
          (insert (format "  練習%d: " (1+ i)))
          (let ((instr-start (point)))
            (insert (plist-get ex :instruction) "\n")
            (add-text-properties instr-start (point)
                                 '(face nskk-tutorial-instruction-face)))
          (when-let* ((hint (plist-get ex :hint)))
            (let ((hint-start (point)))
              (insert "  ヒント: " hint "\n")
              (add-text-properties hint-start (point)
                                   '(face nskk-tutorial-hint-face))))
          (add-text-properties start (point) '(read-only t)))
        ;; Input area
        (let ((start (point)))
          (insert "  入力欄: ")
          (add-text-properties start (point) '(read-only t)))
        (let ((input-beg (point-marker)))
          (insert "                                        ")
          (let ((input-end (point-marker)))
            (set-marker-insertion-type input-end t)
            (add-text-properties (marker-position input-beg)
                                 (marker-position input-end)
                                 '(face nskk-tutorial-input-area-face))
            (push (cons input-beg input-end) nskk-tutorial--exercise-markers)))
        (let ((start (point)))
          (insert "\n")
          (add-text-properties start (point) '(read-only t)))
        ;; Result area
        (let ((start (point)))
          (insert "  結果:   ")
          (add-text-properties start (point) '(read-only t)))
        (let ((result-beg (point-marker)))
          (insert "          ")
          (let ((result-end (point-marker)))
            (set-marker-insertion-type result-end t)
            (push (cons result-beg result-end) nskk-tutorial--result-markers)))
        (let ((start (point)))
          (insert "\n\n")
          (add-text-properties start (point) '(read-only t)))))
    ;; Reverse marker lists so index 0 = first exercise
    (setq nskk-tutorial--exercise-markers (nreverse nskk-tutorial--exercise-markers))
    (setq nskk-tutorial--result-markers (nreverse nskk-tutorial--result-markers))
    ;; Footer
    (let ((start (point)))
      (insert "──────────────────────────────────────\n"
              "  [M-p] 前のレッスン  [M-n] 次のレッスン  "
              "[r] リセット  [q] 終了\n")
      (add-text-properties start (point) '(read-only t)))
    ;; Position cursor at first input area
    (when nskk-tutorial--exercise-markers
      (goto-char (marker-position (caar nskk-tutorial--exercise-markers))))))


;;;;
;;;; Validation Engine
;;;;

(defun nskk-tutorial--get-input-text (index)
  "Get the text content of exercise input area at INDEX."
  (when-let* ((markers (nth index nskk-tutorial--exercise-markers)))
    (let ((text (buffer-substring-no-properties
                 (marker-position (car markers))
                 (marker-position (cdr markers)))))
      (string-trim text))))

(defun nskk-tutorial--set-result (index text face)
  "Set the result display for exercise INDEX to TEXT with FACE."
  (when-let* ((markers (nth index nskk-tutorial--result-markers)))
    (let ((inhibit-read-only t))
      (delete-region (marker-position (car markers))
                     (marker-position (cdr markers)))
      (goto-char (marker-position (car markers)))
      (insert (propertize text 'face face)))))

(defun nskk-tutorial--validate-exercises ()
  "Check all exercises in the current lesson and update results.
Called from `post-command-hook'."
  (when (and nskk-tutorial--exercise-states
             nskk-tutorial--exercise-markers)
    (let* ((lesson (nth nskk-tutorial--current-lesson nskk-tutorial--lessons))
           (exercises (plist-get lesson :exercises))
           (all-done t)
           (save-point (point)))
      (dotimes (i (length exercises))
        (unless (aref nskk-tutorial--exercise-states i)
          (let* ((ex (nth i exercises))
                 (validator (plist-get ex :validator))
                 (expected (plist-get ex :expected))
                 (input (nskk-tutorial--get-input-text i))
                 (passed (if validator
                             (funcall validator)
                           (and expected
                                (not (string-empty-p input))
                                (string= input expected)))))
            (if passed
                (progn
                  (aset nskk-tutorial--exercise-states i t)
                  (nskk-tutorial--set-result i "✓ 正解！" 'nskk-tutorial-success-face))
              (setq all-done nil)))))
      (when all-done
        (nskk-tutorial--set-result
         (1- (length exercises))
         "✓ 正解！ すべての練習が完了しました！ [M-n] で次のレッスンへ"
         'nskk-tutorial-success-face))
      (goto-char save-point))))


;;;;
;;;; Navigation Commands
;;;;

(defun nskk-tutorial-next-lesson ()
  "Go to the next lesson."
  (interactive)
  (if (< nskk-tutorial--current-lesson (1- (length nskk-tutorial--lessons)))
      (progn
        (cl-incf nskk-tutorial--current-lesson)
        (nskk-tutorial--render-lesson)
        (nskk-tutorial--reset-mode))
    (message "最後のレッスンです。")))

(defun nskk-tutorial-prev-lesson ()
  "Go to the previous lesson."
  (interactive)
  (if (> nskk-tutorial--current-lesson 0)
      (progn
        (cl-decf nskk-tutorial--current-lesson)
        (nskk-tutorial--render-lesson)
        (nskk-tutorial--reset-mode))
    (message "最初のレッスンです。")))

(defun nskk-tutorial-goto-lesson (n)
  "Jump to lesson N (1-based)."
  (interactive "nレッスン番号: ")
  (let ((idx (1- n)))
    (if (and (>= idx 0) (< idx (length nskk-tutorial--lessons)))
        (progn
          (setq nskk-tutorial--current-lesson idx)
          (nskk-tutorial--render-lesson)
          (nskk-tutorial--reset-mode))
      (message "レッスン%dは存在しません。（1〜%d）" n (length nskk-tutorial--lessons)))))

(defun nskk-tutorial-reset-lesson ()
  "Reset the current lesson: clear all input areas and results."
  (interactive)
  (nskk-tutorial--render-lesson)
  (nskk-tutorial--reset-mode)
  (message "レッスンをリセットしました。"))

(defun nskk-tutorial-quit ()
  "Quit the tutorial and restore Prolog state."
  (interactive)
  (when (yes-or-no-p "チュートリアルを終了しますか？ ")
    (kill-buffer (current-buffer))))

(defun nskk-tutorial--reset-mode ()
  "Reset NSKK to hiragana mode in the tutorial buffer."
  (when (and nskk-mode (boundp 'nskk-current-state) nskk-current-state)
    (nskk--set-mode 'hiragana)
    (when (boundp 'nskk--romaji-buffer)
      (setq nskk--romaji-buffer ""))
    (nskk-modeline-update)))

(defun nskk-tutorial--maybe-navigate (direction)
  "Navigate if point is in a read-only region, else self-insert.
DIRECTION is `next' or `prev'."
  (if (get-text-property (point) 'read-only)
      (pcase direction
        ('next (nskk-tutorial-next-lesson))
        ('prev (nskk-tutorial-prev-lesson)))
    (self-insert-command 1)))

(defun nskk-tutorial-n-or-self-insert ()
  "Navigate to next lesson or self-insert depending on context."
  (interactive)
  (nskk-tutorial--maybe-navigate 'next))

(defun nskk-tutorial-p-or-self-insert ()
  "Navigate to previous lesson or self-insert depending on context."
  (interactive)
  (nskk-tutorial--maybe-navigate 'prev))


;;;;
;;;; Major Mode
;;;;

(defvar-keymap nskk-tutorial-mode-map
  :doc "Keymap for `nskk-tutorial-mode'."
  :parent special-mode-map
  "M-n" #'nskk-tutorial-next-lesson
  "M-p" #'nskk-tutorial-prev-lesson
  "n"   #'nskk-tutorial-n-or-self-insert
  "p"   #'nskk-tutorial-p-or-self-insert
  "g"   #'nskk-tutorial-goto-lesson
  "r"   #'nskk-tutorial-reset-lesson
  "q"   #'nskk-tutorial-quit)

(define-derived-mode nskk-tutorial-mode special-mode "NSKK-Tutorial"
  "Major mode for the NSKK interactive tutorial.

Provides a hybrid learning environment with read-only explanations
and interactive exercise sections.  NSKK minor mode is activated
within this buffer for practicing SKK operations.

\\{nskk-tutorial-mode-map}"
  :group 'nskk
  (setq header-line-format '(:eval (nskk-tutorial--header-line)))
  ;; Disable buffer-read-only inherited from special-mode.
  ;; We rely on text property `read-only' for per-region protection instead,
  ;; because nskk-mode needs to write into editable input areas.
  (setq buffer-read-only nil)
  (add-hook 'post-command-hook #'nskk-tutorial--validate-exercises nil t)
  (add-hook 'kill-buffer-hook #'nskk-tutorial--on-kill nil t)
  ;; Prevent dict save during tutorial
  (remove-hook 'kill-emacs-hook #'nskk--dict-maybe-save))

(defun nskk-tutorial--on-kill ()
  "Buffer kill hook: restore Prolog state and re-register dict save hook."
  (when nskk-mode
    (ignore-errors (nskk-mode -1)))
  (nskk-tutorial--restore-prolog-state)
  (add-hook 'kill-emacs-hook #'nskk--dict-maybe-save))


;;;;
;;;; Entry Point
;;;;

;;;###autoload
(defun nskk-tutorial ()
  "Start the NSKK interactive tutorial.
Opens a dedicated buffer with lessons teaching core SKK operations.
A bundled mini dictionary ensures predictable conversion results.

The user's personal dictionary is not affected; the Prolog database
is saved on entry and restored when the tutorial buffer is killed."
  (interactive)
  (if-let* ((buf (get-buffer nskk-tutorial--buffer-name)))
      (pop-to-buffer buf)
    (let ((buf (get-buffer-create nskk-tutorial--buffer-name)))
      (with-current-buffer buf
        (nskk-tutorial--save-prolog-state)
        (nskk-tutorial--install-mini-dict)
        (nskk-tutorial-mode)
        (nskk-mode 1)
        (nskk-tutorial--reset-mode)
        (nskk-tutorial--render-lesson))
      (pop-to-buffer buf))))

(provide 'nskk-tutorial)

;;; nskk-tutorial.el ends here
