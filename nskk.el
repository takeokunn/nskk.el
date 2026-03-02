;;; nskk.el --- NSKK main entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: i18n japanese input method skk convenience

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

;; NSKK is a next-generation SKK (Simple Kana to Kanji) Japanese input
;; method for Emacs.  SKK converts romaji keystrokes to kana/kanji using
;; a dictionary, with the user explicitly marking word boundaries.
;;
;; Features:
;; - Zero external dependencies (Emacs 29.1+ only)
;; - < 1ms key response, < 10ms dictionary search
;; - Modular architecture with clean layer separation
;; - AZIK extended romaji support (optional, via nskk-azik.el)
;;
;; Quick start:
;;   (require 'nskk)
;;   (nskk-global-mode 1)
;;
;; Key bindings (in nskk-mode):
;;   C-x C-j  Toggle NSKK on/off in current buffer
;;   C-j      Kakutei (commit) / enter hiragana from ASCII / newline in Japanese mode
;;   SPC      Start conversion (▽→▼) / cycle next candidate
;;   x        Previous candidate
;;   RET      Commit candidate and insert newline
;;   q        Toggle hiragana ↔ katakana
;;   l        Switch to ASCII (latin) mode
;;   L        Switch to full-width latin (JIS X 0208) mode
;;   /        Enter abbrev mode (▽word SPC → dictionary lookup, DDSKK-compatible)
;;   DEL      Delete last preedit char / cancel empty abbrev preedit
;;   C-g      Cancel conversion or preedit (abbrev: restores previous mode)
;;   C-n      Next candidate (in conversion mode) / next-line
;;   C-p      Previous candidate (in conversion mode) / previous-line
;;
;; Input modes: ascii (default), hiragana, katakana, latin,
;;   abbrev, jisx0208-latin.
;;
;; Dictionary configuration:
;;   (setq nskk-jisyo-files
;;         (list "/path/to/SKK-JISYO.L" "/path/to/SKK-JISYO.jinmei"))
;;
;; SKK server (skkserv) configuration (optional):
;;   (setq nskk-server-enable t)
;;   (setq nskk-server-host "localhost")

;;; Code:

;; Foundation modules
(require 'nskk-custom)
(require 'nskk-prolog)
(require 'nskk-state)

;; Core modules
(require 'nskk-henkan)
(require 'nskk-input)

;; UI modules
(require 'nskk-keymap)
(require 'nskk-candidate-window)
(require 'nskk-modeline)

;; Optional
(require 'nskk-debug nil t)
(require 'nskk-server nil t)

(declare-function nskk-set-mode-hiragana "nskk-input")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk--clear-pending-romaji "nskk-henkan" ())
(declare-function nskk--current-kakutei-state "nskk-keymap")
(declare-function nskk--maybe-load-azik-style "nskk-input")
(declare-function nskk-dict--maybe-save "nskk-dictionary")
(declare-function nskk-handle-ctrl-a "nskk-keymap")
(declare-function nskk-handle-ctrl-e "nskk-keymap")
(declare-function nskk-handle-tab "nskk-keymap")
(declare-function nskk-handle-hash "nskk-keymap")
(declare-function nskk-handle-semicolon-key "nskk-input")
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--get-conversion-start "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")

(defvar nskk-mode-hook nil
  "Hook run when NSKK mode is enabled.
DDSKK equivalent: skk-mode-hook")

(defvar nskk-mode-off-hook nil
  "Hook run when NSKK mode is disabled.
DDSKK equivalent: skk-mode-off-hook")

(defvar nskk-input-mode-hook nil
  "Hook run when input mode changes.
DDSKK equivalent: skk-input-mode-hook")

(defvar nskk--system-dict-index)

;; Define the minor mode
(defvar-keymap nskk-mode-map
  :doc "Keymap for NSKK minor mode."
  ;; Remap self-insert for romaji->kana conversion
  "<remap> <self-insert-command>" #'nskk-self-insert
  ;; Mode switching
  "C-x C-j" #'nskk-toggle-mode
  "C-j"     #'nskk-kakutei
  ;; State-aware special key dispatch (see nskk-keymap.el)
  "q"       #'nskk-handle-q
  "l"       #'nskk-handle-l
  "SPC"     #'nskk-handle-space
  "RET"     #'nskk-handle-return
  ;; Additional ddskk-compatible bindings
  "L"       #'nskk-handle-upper-l
  "/"       #'nskk-handle-slash
  "x"       #'nskk-handle-x
  "C-n"     #'nskk-handle-ctrl-n
  "C-p"     #'nskk-handle-ctrl-p
  "C-f"     #'nskk-handle-ctrl-f
  "<right>" #'nskk-handle-ctrl-f
  "C-b"     #'nskk-handle-ctrl-b
  "<left>"  #'nskk-handle-ctrl-b
  "<down>"  #'nskk-handle-ctrl-n
  "<up>"    #'nskk-handle-ctrl-p
  "C-a"     #'nskk-handle-ctrl-a
  "<home>"  #'nskk-handle-ctrl-a
  "C-e"     #'nskk-handle-ctrl-e
  "<end>"   #'nskk-handle-ctrl-e
  "C-g"     #'nskk-handle-cancel
  "DEL"     #'nskk-handle-backspace
  ;; Sticky shift, dynamic completion, numeric input
  ";"       #'nskk-handle-semicolon-key
  "TAB"     #'nskk-handle-tab
  "#"       #'nskk-handle-hash)

;;;###autoload
(define-minor-mode nskk-mode
  "Minor mode for NSKK (Next-generation SKK) input method.

The mode-line indicator is generated dynamically by `nskk-modeline-indicator'.
Use `nskk-modeline-format' to customize its format, and `nskk-use-color-cursor'
to control per-mode cursor color changes.

\\{nskk-mode-map}"
  :init-value nil
  :lighter '(:eval (nskk-modeline-indicator))
  :keymap nskk-mode-map
  :group 'nskk
  (if nskk-mode
      (nskk--enable)
    (nskk--disable)))

(defvar-keymap nskk-global-mode-map
  :doc "Global keymap for `nskk-global-mode'.
This provides global bindings that work even when nskk-mode is not yet active."
  "C-x C-j" #'nskk-toggle-mode)

;;;###autoload
(define-globalized-minor-mode nskk-global-mode
  nskk-mode
  nskk--turn-on-mode
  :global t
  :keymap nskk-global-mode-map
  :group 'nskk)

;; Internal implementation functions

(defun nskk--enable ()
  "Enable NSKK in current buffer."
  (nskk-debug-message "NSKK enabled in buffer: %s" (buffer-name))
  ;; Initialize global Prolog predicates (idempotent, guard-protected)
  (nskk-state-initialize-prolog)
  (nskk-kana-initialize)
  (nskk-henkan-initialize)
  (nskk-input-initialize)
  (nskk-converter-initialize)
  (unless (nskk-prolog-holds-p '(dict-initialized))
    (nskk-dict-initialize))
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create nskk-state-default-mode))
    (nskk-debug-message "Created initial state: mode=%s" nskk-state-default-mode))
  ;; Wire candidate display hooks
  (add-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
  (add-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
  (setq nskk-henkan-select-candidate-by-key-function #'nskk-candidate-list-select-by-key)
  ;; Load AZIK style if configured
  (nskk--maybe-load-azik-style)
  ;; Register save-on-exit hook; add-hook deduplicates same symbol safely
  (add-hook 'kill-emacs-hook #'nskk-dict--maybe-save)
  (nskk--setup-buffer)
  (nskk-modeline-update))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  (run-hooks 'nskk-mode-off-hook)
  (nskk--cleanup-buffer)
  (remove-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
  (remove-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
  (setq nskk-henkan-select-candidate-by-key-function nil)
  (setq nskk-current-state nil))

(defun nskk--turn-on-mode ()
  "Turn on nskk-mode in appropriate buffers."
  (nskk-debug-message "Turning on NSKK mode in buffer: %s" (buffer-name))
  (unless (minibufferp)
    (nskk-mode 1)))

(defun nskk--setup-buffer ()
  "Setup buffer-local NSKK state."
  (add-hook 'post-command-hook #'nskk--post-command-handler nil t))

(defun nskk--cleanup-buffer ()
  "Cleanup buffer-local NSKK state."
  (remove-hook 'post-command-hook #'nskk--post-command-handler t))

(defun nskk--post-command-handler ()
  "Handle post-command hook for NSKK state update.
Also guards against point escaping the conversion region (▼) due to
unmapped cursor-movement commands (mouse clicks, C-a, scroll, etc.).
When point drifts outside [conversion-start .. overlay-end] while
converting, performs an implicit kakutei (確定) identical to DDSKK
behaviour.  Named nskk handlers (C-f, C-b, etc.) already call
`nskk-commit-current' explicitly before moving, so by the time this
hook fires for them, `nskk-converting-p' is already nil and this guard
is a no-op."
  (when (and nskk-mode nskk-current-state)
    ;; Point-escape guard: if converting and point moved outside the
    ;; conversion region, commit the current candidate (implicit kakutei).
    (when (nskk-converting-p)
      (let* ((conv-start (nskk--get-conversion-start))
             (overlay-end (when (and (boundp 'nskk--conversion-overlay)
                                     (overlayp nskk--conversion-overlay))
                            (overlay-end nskk--conversion-overlay))))
        (when (and conv-start overlay-end
                   (or (< (point) conv-start)
                       (> (point) overlay-end)))
          (nskk-commit-current))))
    (nskk-modeline-update)))

;; User commands

;;;###autoload
(defun nskk-toggle-mode ()
  "Toggle NSKK mode on/off in current buffer."
  (interactive)
  (if nskk-mode
      (nskk-mode 0)
    (nskk-mode 1))
  (message "NSKK mode %s" (if nskk-mode "enabled" "disabled")))

;;;###autoload
(defun nskk-kakutei ()
  "Commit conversion or switch to hiragana mode (確定).
Dispatches via the `kakutei-action/2' Prolog predicate based on state:
- converting (▼): commit current candidate
- preedit (▽): commit preedit text as-is
- romaji-pending: flush incomplete romaji buffer
- hiragana-idle (hiragana): insert newline
- katakana-idle (katakana/katakana-半角): switch to hiragana
- direct-idle (ascii/latin/jisx0208-latin/abbrev): switch to hiragana"
  (interactive)
  (let* ((state (nskk--current-kakutei-state))
         (action (nskk-prolog-query-value
                  `(kakutei-action ,state ,'\?a) '\?a)))
    (pcase action
      ('commit-candidate (nskk-commit-current))
      ('commit-preedit   (nskk-henkan-kakutei))
      ('clear-romaji     (nskk--clear-pending-romaji) (setq nskk--romaji-buffer ""))
      ('enter-hiragana   (nskk-set-mode-hiragana))
      ('insert-newline   (newline))
      (_                 nil))))

(provide 'nskk)

;;; nskk.el ends here
