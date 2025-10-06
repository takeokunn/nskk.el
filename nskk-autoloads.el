;;; nskk-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-

;; Copyright (C) 2025 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKの自動ロード定義を含みます。
;; パッケージマネージャー (use-package, straight.el, package.el等) から
;; NSKKをインストールする際に自動的に読み込まれます。
;;
;; ddskk互換性のため、グローバルキーバインドも自動設定されます。

;;; Code:

;;;### (autoloads nil "nskk" "nskk.el" (0 0 0 0))

;;;###autoload
(autoload 'nskk-mode "nskk" "\
NSKK (Next-generation SKK) 日本語入力モード。

\(fn &optional ARG)" t nil)

;;;###autoload
(autoload 'nskk-auto-fill-mode "nskk" "\
NSKKモードとauto-fill-modeを同時にトグルする。
ddskkの `skk-auto-fill-mode' 互換。

\(fn &optional ARG)" t nil)

;;;###autoload
(autoload 'nskk-tutorial "nskk" "\
NSKKのチュートリアルを起動する。
ddskkの `skk-tutorial' 互換。

\(fn)" t nil)

;;;###autoload
(autoload 'nskk-setup-global-keys "nskk" "\
NSKKのグローバルキーバインドを設定する。
ddskkと互換性のあるキーバインドを設定する。

\(fn)" t nil)

;;;###autoload
(autoload 'nskk-activate "nskk" "\
NSKKを有効化する。

\(fn)" t nil)

;;;###autoload
(autoload 'nskk-deactivate "nskk" "\
NSKKを無効化する。

\(fn)" t nil)

;;;###autoload
(autoload 'nskk-toggle "nskk" "\
NSKKモードをトグルする。

\(fn)" t nil)

;;;###autoload
(autoload 'nskk-setup "nskk" "\
NSKKの初期セットアップを実行する。

\(fn)" t nil)

;;;###autoload
(register-input-method
 "nskk"
 "Japanese"
 'nskk-input-method
 "NSKK"
 "Next-generation SKK input method")

;;;***

;; ddskk互換: パッケージロード時にグローバルキーを自動設定
;; ユーザーが `nskk-setup-global-keys-on-load' を nil に設定している場合は無視
(with-eval-after-load 'nskk
  (when (and (boundp 'nskk-setup-global-keys-on-load)
             nskk-setup-global-keys-on-load)
    (nskk-setup-global-keys)))

(provide 'nskk-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:

;;; nskk-autoloads.el ends here
