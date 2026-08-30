# FR-001 Phase 1a 独立検証報告

status: error

summary: FR-001は現状「不合格」です。機能ゲートはすべて通過しましたが、henkan層にinline固有知識がdocstringとして残り、benchでも主要E2E項目に最大39.4%の悪化が観測されました。

## evidence

### ① 各ゲート結果とテスト数

- [verified] `make compile` → exit 0
- [verified] `make test` → exit 0
  - selector: `t`
  - 6,087件選択、6,087件実行
  - 6,087 expected、0 unexpected
  - ベースライン6,086件から1件増加
- [verified] `make lint` → exit 0
- [verified] `make package-lint` → exit 0
  - repository対象の指摘なし
  - Nix store内依存パッケージ由来のlexical-binding警告2件のみ
- [verified] `make bench` → exit 0、Samples=3
- [verified] 検証前後で28個の `src/*.el` のmtime・sizeに変化なし

### ② grep結果

- [verified] `grep -cE "nskk-inline|nskk--inline" src/nskk-henkan.el` → `0`

実装シンボルとしてのhenkan→inline直接依存は除去されています。

### ③ bench比較

Phase 0 baseline → current のmedian ms/op:

- hash-hit: 0.0101 → 0.0091（-9.9%）
- hash-miss: 0.0067 → 0.0050（-25.4%）
- input-route query: 0.0032 → 0.0025（-21.9%）
- core-search exact: 0.1108 → 0.1124（+1.4%）
- E2E `a`: 0.0133 → 0.0157（+18.0%）
- E2E `ka`: 0.0235 → 0.0261（+11.1%）
- E2E `shi`: 0.0329 → 0.0436（+32.5%）
- E2E `nihongo`: 0.0885 → 0.1234（+39.4%）
- E2E `kka`: 0.0394 → 0.0403（+2.3%）

Prolog系は改善し、core-searchも横ばいです。一方、`shi` と `nihongo` は「大きな退行がない」とは現時点で判定できません。Samples=3の単回比較であるため、再現性とFR-001起因性は未確定です。

### ④ diffレビュー所見

- [verified] 登録・列挙は `nskk-prolog-register-presentation-action` / `nskk-prolog-presentation-actions` に集約されています。henkanからinline実装シンボルへの参照はありません。
- [verified] cleanup callbackが例外を投げても処理を継続し、finalize後にhenkan所有状態を再消去してから最初の例外を再送出する構造です。finalizeアクションはcleanupとは独立してoverlayを削除します。
- [verified] 新設異常系テストは4種類のoverlayについて、変数のnil化とoverlay objectのbufferからの切断を検証しています。対象テスト単独は1件選択・1件成功です。
- [verified] 既存テストの弱体化・skip追加はありません。ただし新設テストは未追跡ファイルのため、`git diff -- test/` だけでは表示されません。
- [verified] inline未ロード専用の既存テストはありません。fresh Emacsによる簡易再現では、inlineをロードせずに確定フローとterminal cleanupがともに完走しました。

要修正項目:

- 中: `src/nskk-henkan.el:761` のdocstringに “inline overlay” が残っています。FR-001の「henkan側にinline知識を残さない」に反するため、一般的なpresentation action/finalizationの記述へ変更が必要です。
- 中: E2E benchmarkの`shi`（+32.5%）と`nihongo`（+39.4%）を再測定し、再現する場合は原因特定が必要です。
- 低: `src/nskk-prolog.el:1722` 周辺の新APIが `DSL Macros` 節に置かれ、private helperにdocstringがありません。冒頭のKey public API一覧にも新しい2 APIが未掲載です。
- 低: `src/nskk-inline.el:143` のprivate helperが `Public API` 節内にあります。
- 低: 異常系テストは例外種別のみ確認しており、「最初の例外がpayloadを含めunchangedで再送出される」ことまでは検証していません。

### ⑤ 受け入れ判定

不合格。

理由は、機能ゲートおよびoverlay消去不変条件は通過しているものの、次の受け入れ条件が未達または未確定だからです。

1. henkan側のdocstringにinline固有知識が残り、レイヤー分離要件を完全には満たしていません。
2. benchmarkで`shi`と`nihongo`に大きなE2E悪化が観測され、「大きな退行がない」ことを確認できていません。

## verification

- `make compile` — exit 0
- `make test` — exit 0、6,087/6,087成功
- `make lint` — exit 0
- `make package-lint` — exit 0
- `make bench` — exit 0、Samples=3
- 対象異常系ERT — exit 0、1/1成功
- inline未ロードbatch再現2件 — exit 0
- `git diff --check -- src/nskk-prolog.el src/nskk-inline.el src/nskk-henkan.el test/unit/nskk-presentation-action-test.el` — exit 0
- 検証前後で28個の `src/*.el` のmtime・sizeに変化なし
- `src/`・`test/` の編集、git add、commit、pushは実施していません

## gaps

- benchmark悪化の再現性およびFR-001との因果関係は未確定です。
- inline未ロード動作は簡易再現のみで、恒久的な回帰テストはありません。
