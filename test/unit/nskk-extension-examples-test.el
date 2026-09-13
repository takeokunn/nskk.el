;;; nskk-extension-examples-test.el --- Executable README examples -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;;; Commentary:

;; Evaluate the README blocks themselves to detect documentation drift.

;;; Code:

(require 'nskk-test-framework)
(require 'nskk)

(defconst nskk-extension-examples--readme
  (expand-file-name "../../README.org" (file-name-directory load-file-name)))

(defun nskk-extension-examples--eval (name)
  "Evaluate README block NAME instead of maintaining a copy in the test."
  (with-temp-buffer
    (insert-file-contents nskk-extension-examples--readme)
    (goto-char (point-min))
    (search-forward (concat "#+name: " name "\n"))
    (search-forward "#+begin_src elisp\n")
    (let ((start (point)))
      (search-forward "#+end_src")
      (narrow-to-region start (match-beginning 0)))
    (goto-char (point-min))
    (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
      (eval (read (current-buffer)) t))))

(nskk-describe "extension examples"
  (nskk-it "binds a guarded mode command"
    (let ((nskk-mode-map (copy-keymap nskk-mode-map)))
      (unwind-protect
          (progn
            (nskk-extension-examples--eval "nskk-extension-keys")
            (should (eq (lookup-key nskk-mode-map (kbd "C-c j"))
                        'my-nskk-hiragana))
            (with-temp-buffer
              (let ((nskk-current-state (nskk-state-create 'ascii)))
                (should (eq (nskk-current-key-state) 'normal))
                (call-interactively #'my-nskk-hiragana)
                (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))
        (fmakunbound 'my-nskk-hiragana))))

  (nskk-it "retains user rules across reloads"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-converter-user-rules nil)
            (nskk-converter-romaji-style 'standard))
        (nskk-converter-initialize)
        (nskk-extension-examples--eval "nskk-extension-rules")
        (should (equal (nskk-converter-lookup "qa") "くぁ"))
        (should (equal (nskk-converter-lookup "ka") "ヵ"))
        (nskk-converter-reload-style nskk-converter-romaji-style)
        (should (equal (nskk-converter-lookup "qa") "くぁ"))
        (should (equal (nskk-converter-lookup "ka") "ヵ")))))

  (nskk-it "shows and selects candidates within the current page"
    (let ((nskk-henkan-show-candidates-functions nil)
          (nskk-henkan-hide-candidates-functions nil)
          (nskk-henkan-select-candidate-by-key-function nil)
          (nskk-henkan-show-candidates-keys '(?a ?s ?d))
          displayed)
      (unwind-protect
          (progn
            (nskk-extension-examples--eval "nskk-extension-candidate-ui")
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq displayed
                               (and format-string
                                    (apply #'format format-string args))))))
              (run-hook-with-args 'nskk-henkan-show-candidates-functions
                                  '("甲" "乙" "丙" "丁") 2)
              (should (equal displayed "a:丙  s:丁"))
              (should (= (funcall nskk-henkan-select-candidate-by-key-function
                                  ?s '("甲" "乙" "丙" "丁") 2)
                         3))
              (let ((nskk-henkan-number-to-display-candidates 1))
                (run-hook-with-args 'nskk-henkan-show-candidates-functions
                                    '("甲" "乙" "丙" "丁") 2)
                (should (equal displayed "a:丙"))
                (should-not (funcall nskk-henkan-select-candidate-by-key-function
                                     ?s '("甲" "乙" "丙" "丁") 2)))
              (run-hooks 'nskk-henkan-hide-candidates-functions)
              (should-not displayed)))
        (fmakunbound 'my-nskk-show-candidates)
        (fmakunbound 'my-nskk-hide-candidates))))

  (nskk-it "formats candidate text"
    (let ((nskk-candidate-format-function #'nskk-candidate-format))
      (unwind-protect
          (progn
            (nskk-extension-examples--eval "nskk-extension-candidate-format")
            (let ((text (funcall nskk-candidate-format-function '("甲") '(?a) 2)))
              (should (string-prefix-p "\na:甲" text))
              (should (string-suffix-p "  [選択]" text))))
        (fmakunbound 'my-nskk-candidate-text))))

  (nskk-it "customizes mode display metadata"
    (let ((nskk-mode-display-alist (copy-tree nskk-mode-display-alist))
          (nskk-use-color-cursor nil)
          (nskk-show-mode-show nil))
      (nskk-extension-examples--eval "nskk-extension-mode-display")
      (should (equal (nskk-mode-display-properties 'hiragana)
                     '("あ" nskk-modeline-hiragana-face "ひらがな"
                       nskk-cursor-hiragana)))
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (should (equal (substring-no-properties (nskk-modeline-indicator))
                         " あ"))))))

  (nskk-it "formats horizontal and vertical inline candidates"
    (let ((nskk-inline-format-function #'nskk-inline-format-candidate)
          (nskk-show-inline nil))
      (unwind-protect
          (progn
            (nskk-extension-examples--eval "nskk-extension-inline")
            (should nskk-show-inline)
            (should (equal (funcall nskk-inline-format-function "候補" t)
                           " [候補]"))
            (should (equal (funcall nskk-inline-format-function "候補" 'vertical)
                           "\n[候補]")))
        (fmakunbound 'my-nskk-inline-text)))))

(provide 'nskk-extension-examples-test)
;;; nskk-extension-examples-test.el ends here
