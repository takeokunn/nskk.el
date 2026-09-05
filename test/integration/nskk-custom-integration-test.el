;;; nskk-custom-integration-test.el --- Integration tests for NSKK options  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the NSKK customization surface.

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-state)
(require 'nskk-modeline)
(require 'nskk-search)
(require 'nskk-converter)
(require 'nskk-debug)
(require 'nskk-test-framework)
(require 'nskk-test-macros)


;;;;
;;;; nskk-state-default-mode
;;;;

(nskk-describe "nskk-state-default-mode: initial mode for new state"

  (nskk-it "nskk-state-create uses nskk-state-default-mode when called without argument"
    (let ((nskk-state-default-mode 'hiragana))
      (let ((state (nskk-state-create)))
        (should (eq (nskk-state-mode state) 'hiragana)))))

  (nskk-it "binding nskk-state-default-mode to katakana gives katakana state"
    (let ((nskk-state-default-mode 'katakana))
      (let ((state (nskk-state-create)))
        (should (eq (nskk-state-mode state) 'katakana)))))

  (nskk-it "binding nskk-state-default-mode to ascii gives ascii state"
    (let ((nskk-state-default-mode 'ascii))
      (let ((state (nskk-state-create)))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "explicit mode argument overrides nskk-state-default-mode"
    (let ((nskk-state-default-mode 'hiragana))
      (let ((state (nskk-state-create 'ascii)))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "invalid nskk-state-default-mode falls back to ascii"
    (let ((nskk-state-default-mode 'nonexistent-mode))
      (let ((state (nskk-state-create)))
        (should (eq (nskk-state-mode state) 'ascii))))))


;;;;
;;;; nskk-converter-auto-start-henkan
;;;;

(nskk-describe "nskk-converter-auto-start-henkan: uppercase triggers conversion"

  (nskk-it "default value is t"
    (should (eq (default-value 'nskk-converter-auto-start-henkan) t)))

  (nskk-it ":safe predicate accepts both t and nil"
    (let ((pred (get 'nskk-converter-auto-start-henkan 'safe-local-variable)))
      (should (funcall pred t))
      (should (funcall pred nil)))))


;;;;
;;;; nskk-converter-romaji-style
;;;;

(nskk-describe "nskk-converter-romaji-style: romaji input style selection"

  (nskk-it "default value is standard"
    (should (eq (default-value 'nskk-converter-romaji-style) 'standard)))

  (nskk-it "nskk-converter-load-style standard succeeds (style is registered)"
    (nskk-prolog-test-with-isolated-db
      (should (nskk-converter-load-style 'standard)))))


;;;;
;;;; nskk-search-sort-method
;;;;

(nskk-describe "nskk-search-sort-method: result ordering for search"

  (nskk-it "default value is frequency"
    (should (eq (default-value 'nskk-search-sort-method) 'frequency)))

  (nskk-it "kana sorting orders all matching readings lexically"
    (nskk-with-mock-dict '(("かさ" . ("傘")) ("かい" . ("貝")) ("かき" . ("柿")))
      (let ((nskk-search-sort-method 'kana)
            (idx (nskk-dict-system-index)))
        (should (equal (mapcar #'car (nskk-search-prefix idx "か" nil nil))
                       '("かい" "かき" "かさ"))))))

  (nskk-it "frequency sorting orders all matching readings by learned usage"
    (nskk-with-mock-dict '(("かさ" . ("傘")) ("かい" . ("貝")) ("かき" . ("柿")))
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-search-learn "かさ" "傘")
      (nskk-search-learn "かさ" "傘")
      (nskk-search-learn "かき" "柿")
      (let ((nskk-search-sort-method 'frequency)
            (idx (nskk-dict-system-index)))
        (should (equal (mapcar #'car (nskk-search-prefix idx "か" nil nil))
                       '("かさ" "かき" "かい")))))))


;;;;
;;;; nskk-modeline-format
;;;;

(nskk-describe "nskk-modeline-format: modeline indicator format string"

  (nskk-it "default value is \" %m\""
    (should (equal (default-value 'nskk-modeline-format) " %m")))

  (nskk-it "nskk-modeline-indicator uses the format string for hiragana mode"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil)
            (nskk-modeline-format " [%m]"))
        (let ((indicator (nskk-modeline-indicator)))
          (should (stringp indicator))
          (should (string-prefix-p " [" indicator))))))

  (nskk-it "changing format to %m-only omits the leading space"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil)
            (nskk-modeline-format "%m"))
        (let ((indicator (nskk-modeline-indicator)))
          (should (stringp indicator))
          (should-not (string-prefix-p " " indicator))))))

  (nskk-it "indicator with nil state returns empty string regardless of format"
    (nskk-with-state nil
      (let ((nskk-modeline-format " [%m]"))
        (should (string= "" (nskk-modeline-indicator)))))))


;;;;
;;;; nskk-use-color-cursor
;;;;

(nskk-describe "nskk-use-color-cursor: cursor color enable/disable"

  (nskk-it "default value is t"
    (should (eq (default-value 'nskk-use-color-cursor) t)))

  (nskk-it "nskk-cursor-update is a no-op when nskk-use-color-cursor is nil"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (should-not (condition-case nil
                        (progn (nskk-cursor-update) nil)
                      (error t))))))

  (nskk-it "nskk-cursor-update does not signal when state is nil regardless of flag"
    (nskk-with-state nil
      (let ((nskk-use-color-cursor t))
        (should-not (condition-case nil
                        (progn (nskk-cursor-update) nil)
                      (error t))))))

  (nskk-it "nskk-modeline-update does not signal when color cursor is nil"
    (with-temp-buffer
      (nskk-with-state 'ascii
        (let ((nskk-use-color-cursor nil))
          (should-not (condition-case nil
                          (progn (nskk-modeline-update) nil)
                        (error t))))))))


;;;;
;;;; nskk-henkan-show-candidates-nth
;;;;

(nskk-describe "nskk-henkan-show-candidates-nth: inline-before-list threshold"

  (nskk-it "default value is 5"
    (should (= (default-value 'nskk-henkan-show-candidates-nth) 5)))

  (nskk-it ":safe predicate accepts zero (show list immediately)"
    (let ((pred (get 'nskk-henkan-show-candidates-nth 'safe-local-variable)))
      (should (funcall pred 0)))))


;;;;
;;;; nskk-henkan-number-to-display-candidates
;;;;

(nskk-describe "nskk-henkan-number-to-display-candidates: candidate page size"

  (nskk-it "default value is 7"
    (should (= (default-value 'nskk-henkan-number-to-display-candidates) 7)))

  (nskk-it "value is always a natural number"
    (should (natnump nskk-henkan-number-to-display-candidates))))


;;;;
;;;; nskk-henkan-show-candidates-keys
;;;;

(nskk-describe "nskk-henkan-show-candidates-keys: selection key list"

  (nskk-it "default value is a list of 7 characters"
    (let ((keys (default-value 'nskk-henkan-show-candidates-keys)))
      (should (listp keys))
      (should (= (length keys) 7))
      (should (cl-every #'characterp keys))))

  (nskk-it "default keys include a s d f j k l"
    (should (equal (default-value 'nskk-henkan-show-candidates-keys)
                   '(?a ?s ?d ?f ?j ?k ?l))))

  (nskk-it "each default key is a character that satisfies characterp"
    (dolist (ch nskk-henkan-show-candidates-keys)
      (should (characterp ch)))))


;;;;
;;;; nskk-max-registration-depth
;;;;

(nskk-describe "nskk-max-registration-depth: recursive registration nesting limit"

  (nskk-it "default value is 3"
    (should (= (default-value 'nskk-max-registration-depth) 3)))

  (nskk-it "value is a natural number"
    (should (natnump nskk-max-registration-depth))))


;;;;
;;;; nskk-debug-enabled
;;;;

(nskk-describe "nskk-debug-enabled: debug logging gate"

  (nskk-it "default value is nil"
    (should (null (default-value 'nskk-debug-enabled))))

  (nskk-it "nskk-debug-message does not write to buffer when nil"
    (let ((nskk-debug-enabled nil))
      (let ((buf-before (and (get-buffer "*NSKK Debug*")
                             (with-current-buffer "*NSKK Debug*"
                               (buffer-size)))))
        (nskk-debug-message "integration test probe: disabled")
        (let ((buf-after (and (get-buffer "*NSKK Debug*")
                              (with-current-buffer "*NSKK Debug*"
                                (buffer-size)))))
          (should (equal buf-before buf-after))))))

  (nskk-it "nskk-debug-message writes to buffer when t"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-message "integration test probe: enabled")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (should buf)
        (should (> (with-current-buffer buf (buffer-size)) 0)))))

  (nskk-it "disabling after enabling suppresses further logging"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-message "first entry"))
    (let ((nskk-debug-enabled nil))
      (let ((size-before (and (get-buffer "*NSKK Debug*")
                              (with-current-buffer "*NSKK Debug*"
                                (buffer-size)))))
        (nskk-debug-message "should not appear")
        (let ((size-after (and (get-buffer "*NSKK Debug*")
                               (with-current-buffer "*NSKK Debug*"
                                 (buffer-size)))))
          (should (equal size-before size-after)))))))


;;;;
;;;; nskk-debug-max-entries
;;;;

(nskk-describe "nskk-debug-max-entries: debug buffer trim threshold"

  (nskk-it "default value is 1000"
    (should (= (default-value 'nskk-debug-max-entries) 1000)))

  (nskk-it "with max-entries 1, buffer retains only the latest line"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 1))
      (nskk-debug-clear)
      (nskk-debug-message "line one")
      (nskk-debug-message "line two")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (should buf)
        (with-current-buffer buf
          (should (= (count-lines (point-min) (point-max)) 1))
          (should (string-match-p "\\`\\[[^]\n]+\\] line two\n\\'"
                                  (buffer-string)))))))

  (nskk-it "with max-entries 0, buffer is cleared on every append"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 0))
      (nskk-debug-clear)
      (nskk-debug-message "zero-max probe")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (should buf)
        (with-current-buffer buf
          (should (= (count-lines (point-min) (point-max)) 0))
          (should (equal (buffer-string) ""))))))

  (nskk-it "value is always a natural number"
    (should (natnump nskk-debug-max-entries))))


;;;;
;;;; Cross-variable interaction: modeline format + color cursor
;;;;

(nskk-describe "cross-variable: modeline-format and use-color-cursor interaction"

  (nskk-it "modeline indicator honours format even when color cursor is disabled"
    (nskk-with-state 'katakana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil)
            (nskk-modeline-format "(%m)"))
        (let ((indicator (nskk-modeline-indicator)))
          (should (stringp indicator))
          (should (string-prefix-p "(" indicator))))))

  (nskk-it "two different format strings produce two different indicator strings"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (let ((ind1 (let ((nskk-modeline-format " %m")
                          (nskk--modeline-indicator-cache nil))
                      (nskk-modeline-indicator)))
              (ind2 (let ((nskk-modeline-format "[%m]")
                          (nskk--modeline-indicator-cache nil))
                      (nskk-modeline-indicator))))
          (should-not (string= ind1 ind2)))))))


;;;;
;;;; Cross-variable interaction: state default mode + modeline
;;;;

(nskk-describe "cross-variable: state-default-mode and modeline indicator"

  (nskk-it "modeline indicator for the default mode is non-empty"
    (let ((nskk-use-color-cursor nil)
          (nskk--modeline-indicator-cache nil))
      (nskk-with-state nskk-state-default-mode
        (let ((indicator (nskk-modeline-indicator)))
          (should (stringp indicator))
          (should (not (string-empty-p indicator)))))))

  (nskk-it "modeline differs between hiragana and katakana default modes"
    (let ((nskk-use-color-cursor nil))
      (let ((ind-hira
             (let ((nskk-state-default-mode 'hiragana)
                   (nskk--modeline-indicator-cache nil))
               (nskk-with-state 'hiragana
                 (nskk-modeline-indicator))))
            (ind-kata
             (let ((nskk-state-default-mode 'katakana)
                   (nskk--modeline-indicator-cache nil))
               (nskk-with-state 'katakana
                 (nskk-modeline-indicator)))))
        (should-not (string= ind-hira ind-kata))))))


(provide 'nskk-custom-integration-test)

;;; nskk-custom-integration-test.el ends here
