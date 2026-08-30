# Phase 0 baseline gates

実行環境: Emacs GNU Emacs 31.1 / aarch64-apple-darwin25.5.0

| gate | command | exit | duration | result |
|---|---|---:|---:|---|
| compile | `make compile` | 0 | 0.82s | verified: pass |
| test | `make test` | 0 | 未保存 | verified: 6,086 tests selected (`selector ‘t’`), pass |
| lint | `make lint` | 0 | 1.48s | verified: pass |
| package-lint | `make package-lint` | 0 | 3.09s | verified: pass; warnings are from installed external packages |

README.org の記載は 5,000+ tests で、実測の 6,086 tests はこれを満たす。

注: 全ゲートの連続再実行では `make test` が途中まで進んだ時点で再測定を中断したため、test の経過時間は保存できなかった。上表の pass は先行実行の完走ログ（`Running 6086 tests ...`）に基づく。

再現コマンド:

```sh
make compile && make test && make lint && make package-lint
```
