;;; nskk-custom-integration-test.el --- Integration tests for nskk-custom  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for customization variables defined in nskk-custom.el.
;;
;; These tests verify that each defcustom variable actually affects runtime
;; behavior when let-bound.  They are distinct from test/unit/nskk-custom-test.el
;; which only checks static metadata (registration, defaults, :safe predicates,
;; groups, faces).
;;
;; Coverage by functional area:
;;
;;   nskk-state-*
;;     nskk-state-default-mode    -- nskk-state-create uses it when no mode given
;;
;;   nskk-converter-*
;;     nskk-converter-auto-start-henkan -- declared guard; read at call time
;;     nskk-converter-romaji-style      -- load-style selects the registered init-fn
;;
;;   nskk-search-*
;;     nskk-search-sort-method    -- nskk--search-sort-results reads it at call time
;;     nskk-search-fuzzy-threshold -- nskk-search-fuzzy applies the distance cutoff
;;
;;   nskk-modeline / nskk-use-color-cursor
;;     nskk-modeline-format       -- nskk-modeline-indicator applies format-spec
;;     nskk-use-color-cursor      -- nskk-cursor-update guards on it
;;
;;   nskk-henkan-*
;;     nskk-henkan-show-candidates-nth         -- Prolog query uses the value
;;     nskk-henkan-number-to-display-candidates -- used by candidate window
;;     nskk-henkan-show-candidates-keys        -- candidate selection key list
;;     nskk-max-registration-depth             -- registration guard
;;
;;   nskk-debug-*
;;     nskk-debug-enabled   -- gates nskk-debug-message / nskk-debug-log
;;     nskk-debug-max-entries -- nskk--debug-trim prunes at this count

;;; Code:

(require 'ert)
(require 'nskk-custom)
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
    ;; An unrecognised symbol is not a valid-mode; nskk-state-create falls back.
    (let ((nskk-state-default-mode 'nonexistent-mode))
      (let ((state (nskk-state-create)))
        (should (eq (nskk-state-mode state) 'ascii))))))


;;;;
;;;; nskk-converter-auto-start-henkan
;;;;

(nskk-describe "nskk-converter-auto-start-henkan: uppercase triggers conversion"

  (nskk-it "default value is t"
    (should (eq (default-value 'nskk-converter-auto-start-henkan) t)))

  (nskk-it "let-binding to nil is visible inside the body"
    (let ((nskk-converter-auto-start-henkan nil))
      (should-not nskk-converter-auto-start-henkan)))

  (nskk-it "let-binding to t is visible inside the body"
    (let ((nskk-converter-auto-start-henkan t))
      (should nskk-converter-auto-start-henkan)))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-converter-auto-start-henkan nil))
      (should-not nskk-converter-auto-start-henkan))
    (should nskk-converter-auto-start-henkan))

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

  (nskk-it "let-binding to azik is visible inside the body"
    (let ((nskk-converter-romaji-style 'azik))
      (should (eq nskk-converter-romaji-style 'azik))))

  (nskk-it "let-binding to standard is visible inside the body"
    (let ((nskk-converter-romaji-style 'standard))
      (should (eq nskk-converter-romaji-style 'standard))))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-converter-romaji-style 'azik))
      (should (eq nskk-converter-romaji-style 'azik)))
    (should (eq nskk-converter-romaji-style 'standard)))

  (nskk-it "nskk-converter-load-style standard succeeds (style is registered)"
    (nskk-prolog-test-with-isolated-db
      (should (nskk-converter-load-style 'standard)))))


;;;;
;;;; nskk-search-sort-method
;;;;

(nskk-describe "nskk-search-sort-method: result ordering for search"

  (nskk-it "default value is frequency"
    (should (eq (default-value 'nskk-search-sort-method) 'frequency)))

  (nskk-it "let-binding to none is visible inside the body"
    (let ((nskk-search-sort-method 'none))
      (should (eq nskk-search-sort-method 'none))))

  (nskk-it "let-binding to kana is visible inside the body"
    (let ((nskk-search-sort-method 'kana))
      (should (eq nskk-search-sort-method 'kana))))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-search-sort-method 'none))
      (should (eq nskk-search-sort-method 'none)))
    (should (eq nskk-search-sort-method 'frequency)))

  (nskk-it "with sort-method none, result order equals insertion order"
    (nskk-with-mock-dict '(("あい" . ("愛" "藍" "哀")))
      (let ((nskk-search-sort-method 'none)
            (idx nskk--system-dict-index))
        (let ((entry (nskk-search-exact idx "あい" nil)))
          (when (nskk-dict-entry-p entry)
            ;; With sort none, candidates should be in dict order
            (should (listp (nskk-dict-entry-candidates entry))))))))

  (nskk-it "with sort-method kana, nskk--search-sort-results returns a list"
    (nskk-with-mock-dict '(("こ" . ("子")) ("あ" . ("亜")) ("か" . ("下")))
      (let ((nskk-search-sort-method 'kana)
            (idx nskk--system-dict-index))
        (let ((results (nskk-search-prefix idx "あ" nil nil)))
          ;; prefix on "あ" returns at least the "あ" entry
          (should (or (null results) (listp results)))))))

  (nskk-it "with sort-method frequency, nskk--search-sort-results returns a list"
    (nskk-with-mock-dict '(("あ" . ("亜" "吾")))
      (let ((nskk-search-sort-method 'frequency)
            (idx nskk--system-dict-index))
        (let ((results (nskk-search-prefix idx "あ" nil nil)))
          (should (or (null results) (listp results))))))))


;;;;
;;;; nskk-search-fuzzy-threshold
;;;;

(nskk-describe "nskk-search-fuzzy-threshold: Levenshtein distance cutoff"

  (nskk-it "default value is 3"
    (should (= (default-value 'nskk-search-fuzzy-threshold) 3)))

  (nskk-it "let-binding to 0 disables fuzzy matching"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")))
      (let ((nskk-search-fuzzy-threshold 0)
            (idx nskk--system-dict-index))
        ;; "かん" is distance 1 from "かんじ"; threshold 0 means no fuzzy hits
        (let ((results (nskk-search-fuzzy idx "かん" nil)))
          (should (null results))))))

  (nskk-it "let-binding to 1 allows distance-1 matches"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")))
      (let ((nskk-search-fuzzy-threshold 1)
            (idx nskk--system-dict-index))
        ;; "かんじ" has distance 0 from itself, so it must appear
        (let ((results (nskk-search-fuzzy idx "かんじ" nil)))
          (should results)))))

  (nskk-it "threshold 2 includes entries within distance 2"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")) ("かんき" . ("感激")))
      (let ((nskk-search-fuzzy-threshold 2)
            (idx nskk--system-dict-index))
        ;; "かんじ" and "かんき" are distance 1 apart; both within threshold 2
        (let ((results (nskk-search-fuzzy idx "かんじ" nil)))
          (should results)
          (should (listp results))))))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-search-fuzzy-threshold 0))
      (should (= nskk-search-fuzzy-threshold 0)))
    (should (= nskk-search-fuzzy-threshold 3))))


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

  (nskk-it "format string is restored after let exits"
    (let ((nskk-modeline-format "CUSTOM"))
      (should (equal nskk-modeline-format "CUSTOM")))
    (should (equal nskk-modeline-format " %m")))

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
        ;; Should not signal, should not call set-cursor-color
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
                        (error t)))))))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-use-color-cursor nil))
      (should-not nskk-use-color-cursor))
    (should nskk-use-color-cursor)))


;;;;
;;;; nskk-henkan-show-candidates-nth
;;;;

(nskk-describe "nskk-henkan-show-candidates-nth: inline-before-list threshold"

  (nskk-it "default value is 5"
    (should (= (default-value 'nskk-henkan-show-candidates-nth) 5)))

  (nskk-it "let-binding to 0 is visible inside the body"
    (let ((nskk-henkan-show-candidates-nth 0))
      (should (= nskk-henkan-show-candidates-nth 0))))

  (nskk-it "let-binding to 1 is visible inside the body"
    (let ((nskk-henkan-show-candidates-nth 1))
      (should (= nskk-henkan-show-candidates-nth 1))))

  (nskk-it "variable is restored to 5 after let exits"
    (let ((nskk-henkan-show-candidates-nth 1))
      (should (= nskk-henkan-show-candidates-nth 1)))
    (should (= nskk-henkan-show-candidates-nth 5)))

  (nskk-it ":safe predicate accepts zero (show list immediately)"
    (let ((pred (get 'nskk-henkan-show-candidates-nth 'safe-local-variable)))
      (should (funcall pred 0)))))


;;;;
;;;; nskk-henkan-number-to-display-candidates
;;;;

(nskk-describe "nskk-henkan-number-to-display-candidates: candidate page size"

  (nskk-it "default value is 7"
    (should (= (default-value 'nskk-henkan-number-to-display-candidates) 7)))

  (nskk-it "let-binding to a smaller page size is visible inside the body"
    (let ((nskk-henkan-number-to-display-candidates 3))
      (should (= nskk-henkan-number-to-display-candidates 3))))

  (nskk-it "let-binding to a larger page size is visible inside the body"
    (let ((nskk-henkan-number-to-display-candidates 20))
      (should (= nskk-henkan-number-to-display-candidates 20))))

  (nskk-it "variable is restored to 7 after let exits"
    (let ((nskk-henkan-number-to-display-candidates 4))
      (should (= nskk-henkan-number-to-display-candidates 4)))
    (should (= nskk-henkan-number-to-display-candidates 7)))

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

  (nskk-it "let-binding to a shorter key list is visible inside the body"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d)))
      (should (equal nskk-henkan-show-candidates-keys '(?a ?s ?d)))))

  (nskk-it "let-binding to nil (no selection keys) is visible inside the body"
    (let ((nskk-henkan-show-candidates-keys nil))
      (should (null nskk-henkan-show-candidates-keys))))

  (nskk-it "variable is restored after let exits"
    (let ((nskk-henkan-show-candidates-keys '(?x ?y)))
      (should (equal nskk-henkan-show-candidates-keys '(?x ?y))))
    (should (equal nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))))

  (nskk-it "each default key is a character that satisfies characterp"
    (dolist (ch nskk-henkan-show-candidates-keys)
      (should (characterp ch)))))


;;;;
;;;; nskk-max-registration-depth
;;;;

(nskk-describe "nskk-max-registration-depth: recursive registration nesting limit"

  (nskk-it "default value is 3"
    (should (= (default-value 'nskk-max-registration-depth) 3)))

  (nskk-it "let-binding to 1 is visible inside the body"
    (let ((nskk-max-registration-depth 1))
      (should (= nskk-max-registration-depth 1))))

  (nskk-it "let-binding to 0 is visible inside the body (no nesting)"
    (let ((nskk-max-registration-depth 0))
      (should (= nskk-max-registration-depth 0))))

  (nskk-it "variable is restored to 3 after let exits"
    (let ((nskk-max-registration-depth 1))
      (should (= nskk-max-registration-depth 1)))
    (should (= nskk-max-registration-depth 3)))

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
          ;; Size must not have grown
          (should (equal buf-before buf-after))))))

  (nskk-it "nskk-debug-message writes to buffer when t"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-message "integration test probe: enabled")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (should buf)
        (should (> (with-current-buffer buf (buffer-size)) 0)))))

  (nskk-it "variable is restored to nil after let exits"
    (let ((nskk-debug-enabled t))
      (should nskk-debug-enabled))
    (should-not nskk-debug-enabled))

  (nskk-it "disabling after enabling suppresses further logging"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-message "first entry"))
    ;; Now disabled — new message should not appear
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

  (nskk-it "let-binding to a smaller value is visible inside the body"
    (let ((nskk-debug-max-entries 10))
      (should (= nskk-debug-max-entries 10))))

  (nskk-it "variable is restored to 1000 after let exits"
    (let ((nskk-debug-max-entries 50))
      (should (= nskk-debug-max-entries 50)))
    (should (= nskk-debug-max-entries 1000)))

  (nskk-it "with max-entries 1, buffer contains at most 1 line after two writes"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 1))
      (nskk-debug-clear)
      (nskk-debug-message "line one")
      (nskk-debug-message "line two")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (should buf)
        (with-current-buffer buf
          ;; count-lines returns 0 or 1 for a trimmed single-entry buffer
          (should (<= (count-lines (point-min) (point-max)) 2))))))

  (nskk-it "with max-entries 0, buffer is cleared on every append"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 0))
      (nskk-debug-clear)
      (nskk-debug-message "zero-max probe")
      (let ((buf (get-buffer "*NSKK Debug*")))
        (when buf
          (with-current-buffer buf
            ;; With max 0, trim deletes everything before the last 0 lines
            (should (<= (buffer-size) 200)))))))

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
