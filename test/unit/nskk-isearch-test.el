;;; nskk-isearch-test.el --- Tests for nskk-isearch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-isearch.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-isearch)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Customization

(nskk-describe "nskk-isearch-mode-string-alist"
  (nskk-it "maps every valid NSKK mode to an indicator string"
    (dolist (mode nskk--valid-modes)
      (should (stringp (cdr (assq mode nskk-isearch-mode-string-alist))))))
  (nskk-it "maps no key that is not a valid NSKK mode"
    (dolist (entry nskk-isearch-mode-string-alist)
      (should (memq (car entry) nskk--valid-modes)))))

(nskk-describe "nskk-isearch-mode-string-alist safety predicate"
  (nskk-it "accepts the default value"
    (should (funcall (get 'nskk-isearch-mode-string-alist 'safe-local-variable)
                     nskk-isearch-mode-string-alist)))
  (nskk-it "rejects an indicator that is not a string"
    (should-not
     (funcall (get 'nskk-isearch-mode-string-alist 'safe-local-variable)
              '((hiragana . 12)))))
  (nskk-it "rejects a key that is not a symbol"
    (should-not
     (funcall (get 'nskk-isearch-mode-string-alist 'safe-local-variable)
              '(("hiragana" . "[か]")))))
  (nskk-it "rejects an entry that is not a cons"
    (should-not
     (funcall (get 'nskk-isearch-mode-string-alist 'safe-local-variable)
              '(hiragana)))))

(nskk-describe "nskk-isearch-enable safety predicate"
  (nskk-it "accepts a boolean"
    (should (funcall (get 'nskk-isearch-enable 'safe-local-variable) t))
    (should (funcall (get 'nskk-isearch-enable 'safe-local-variable) nil)))
  (nskk-it "rejects a non-boolean"
    (should-not
     (funcall (get 'nskk-isearch-enable 'safe-local-variable) "yes"))))

;;;; Mode String Lookup

(nskk-describe "nskk--isearch-mode-string with no orig-buffer"
  (nskk-it "returns nil when orig-buffer is nil"
    (let ((nskk--isearch-orig-buffer nil))
      (should (null (nskk--isearch-mode-string))))))

(nskk-describe "nskk--isearch-mode-string with dead buffer"
  (nskk-it "returns nil for dead buffer"
    (let ((buf (generate-new-buffer " *nskk-test-dead*")))
      (kill-buffer buf)
      (let ((nskk--isearch-orig-buffer buf))
        (should (null (nskk--isearch-mode-string)))))))

(nskk-describe "nskk--isearch-mode-string with live buffer"
  (nskk-it "returns nil when nskk-current-state is nil in orig-buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (let ((nskk-current-state nil)
              (nskk--isearch-orig-buffer (current-buffer)))
          (should (null (nskk--isearch-mode-string)))))))

  (nskk-it "returns nil when the buffer's mode has no alist entry"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk--isearch-orig-buffer (current-buffer))
              (nskk-isearch-mode-string-alist nil))
          (should (null (nskk--isearch-mode-string))))))))

(nskk-deftest-table isearch-mode-string
  :columns (mode expected)
  :rows ((hiragana       "[か]")
         (katakana       "[ア]")
         (katakana-半角   "[ｱ]")
         (jisx0208-latin "[英]")
         (ascii          "[aa]")
         (latin          "[aa]")
         (abbrev         "[aあ]"))
  :description "nskk--isearch-mode-string returns this mode's indicator"
  :body (nskk-prolog-test-with-isolated-db
          (nskk-state-initialize-prolog)
          (with-temp-buffer
            (setq nskk-current-state (nskk-state-create mode))
            (let ((nskk--isearch-orig-buffer (current-buffer)))
              (should (equal (nskk--isearch-mode-string) expected))))))

(nskk-describe "nskk--isearch-mode-string/k"
  (nskk-it "calls the success continuation with the indicator"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (should (equal (nskk--isearch-mode-string/k
                          (lambda (indicator) (list :found indicator))
                          (lambda () :missing))
                         '(:found "[か]")))))))
  (nskk-it "calls the failure continuation when no origin buffer is recorded"
    (let ((nskk--isearch-orig-buffer nil))
      (should (eq (nskk--isearch-mode-string/k
                   (lambda (indicator) indicator)
                   (lambda () :missing))
                  :missing)))))

;;;; Prompt Advice

(nskk-describe "nskk--isearch-prompt-advice"
  (nskk-it "returns orig-prompt unchanged when nskk-isearch-enable is nil"
    (let ((nskk-isearch-enable nil)
          (nskk--isearch-orig-buffer nil))
      (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
        (should (equal result "I-search: ")))))

  (nskk-it "returns orig-prompt unchanged when no mode string available"
    (let ((nskk-isearch-enable t)
          (nskk--isearch-orig-buffer nil))
      (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
        (should (equal result "I-search: ")))))

  (nskk-it "prepends mode string to orig-prompt when mode is active"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-isearch-enable t)
              (nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
            (should (stringp result))
            (should (string-prefix-p "[か]" result))
            (should (string-suffix-p "I-search: " result))))))))

;;;; Setup / Teardown

(nskk-describe "nskk-isearch-setup"
  (nskk-it "adds hook to isearch-mode-hook"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (memq #'nskk--isearch-setup isearch-mode-hook)))
      (nskk-isearch-teardown)))
  (nskk-it "adds hook to isearch-mode-end-hook"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (memq #'nskk--isearch-teardown isearch-mode-end-hook)))
      (nskk-isearch-teardown)))
  (nskk-it "installs advice on isearch-message-prefix"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (advice-member-p #'nskk--isearch-prompt-advice
                                   'isearch-message-prefix)))
      (nskk-isearch-teardown)))
  (nskk-it "rolls back every acquired resource across all initial states"
    (let ((case-count 0))
      (nskk-isearch-test--call-with-isolated-lifecycle
       (lambda ()
         (dolist (initial-state
                  '((nil nil nil) (nil nil t) (nil t nil) (nil t t)
                    (t nil nil) (t nil t) (t t nil) (t t t)))
           (dolist (condition '(error quit))
             (dotimes (failure-offset (cl-count nil initial-state))
               (cl-incf case-count)
               (nskk--isearch-restore-resource-state '(nil nil nil))
               (nskk--isearch-restore-resource-state initial-state)
               (nskk--isearch-set-ownership-state '(nil nil nil))
               (let ((original-add-hook (symbol-function 'add-hook))
                     (original-advice-add (symbol-function 'advice-add))
                     (payload (list 'setup-payload initial-state condition
                                    failure-offset))
                     (acquisition-count 0)
                     condition-data
                     data-cons)
                 (setq data-cons (list payload))
                 (cl-letf
                     (((symbol-function 'add-hook)
                       (lambda (hook function &rest arguments)
                         (prog1
                             (apply original-add-hook hook function arguments)
                           (cl-incf acquisition-count)
                           (when (= acquisition-count (1+ failure-offset))
                             (signal condition data-cons)))))
                      ((symbol-function 'advice-add)
                       (lambda (symbol where function &rest properties)
                         (prog1
                             (apply original-advice-add
                                    symbol where function properties)
                           (cl-incf acquisition-count)
                           (when (= acquisition-count (1+ failure-offset))
                             (signal condition data-cons))))))
                   (setq condition-data
                         (condition-case data
                             (progn (nskk-isearch-setup) nil)
                           ((error quit) data))))
                 (should (eq (car condition-data) condition))
                 (should (eq (cdr condition-data) data-cons))
                 (should (eq (cadr condition-data) payload))
                 (should (equal (nskk-isearch-resource-state)
                                initial-state))
                 (should (equal
                          (nskk--isearch-ownership-state)
                          '(nil nil nil))))
               (nskk-isearch-setup)
               (nskk-isearch-setup)
               (should (equal (nskk-isearch-resource-state) '(t t t)))
               (should (equal
                        (nskk--isearch-ownership-state)
                        (mapcar #'not initial-state)))
               (nskk-isearch-teardown)
               (nskk-isearch-teardown)
               (should (equal (nskk-isearch-resource-state)
                              initial-state))
               (should (equal
                        (nskk--isearch-ownership-state)
                        '(nil nil nil))))))))
      (should (= case-count 24))))
  (nskk-it "preserves the original condition when rollback fails"
    (let ((case-count 0))
      (nskk-isearch-test--call-with-isolated-lifecycle
       (lambda ()
         (dolist (original-condition '(error quit))
           (cl-incf case-count)
           (nskk--isearch-restore-resource-state '(nil nil nil))
           (let ((original-add-hook (symbol-function 'add-hook))
                 (original-remove-hook (symbol-function 'remove-hook))
                 (acquisition-count 0)
                 (rollback-signaled nil)
                 condition-data)
             (cl-letf
                 (((symbol-function 'add-hook)
                   (lambda (hook function &rest arguments)
                     (prog1
                         (apply original-add-hook hook function arguments)
                       (cl-incf acquisition-count)
                       (when (= acquisition-count 2)
                         (signal original-condition
                                 '(original-payload))))))
                  ((symbol-function 'remove-hook)
                   (lambda (hook function &rest arguments)
                     (prog1
                         (apply original-remove-hook hook function arguments)
                       (unless rollback-signaled
                         (setq rollback-signaled t)
                         (signal (if (eq original-condition 'error)
                                     'quit
                                   'error)
                                 '(rollback-payload)))))))
               (setq condition-data
                     (condition-case data
                         (progn (nskk-isearch-setup) nil)
                       ((error quit) data))))
             (should (equal condition-data
                            (list original-condition 'original-payload)))
             (should (equal (nskk-isearch-resource-state)
                            '(nil nil nil)))))))
      (should (= case-count 2)))))

(nskk-describe "nskk-isearch-teardown"
  (nskk-it "removes isearch-mode-hook"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (memq #'nskk--isearch-setup isearch-mode-hook)))
  (nskk-it "removes isearch-mode-end-hook"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (memq #'nskk--isearch-teardown isearch-mode-end-hook)))
  (nskk-it "removes advice from isearch-message-prefix"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (advice-member-p #'nskk--isearch-prompt-advice 'isearch-message-prefix))))

;;;; Internal Hooks

(nskk-describe "nskk--isearch-setup hook"
  (nskk-it "records current buffer as orig-buffer"
    (with-temp-buffer
      (let ((nskk--isearch-orig-buffer nil))
        (nskk--isearch-setup)
        (should (eq nskk--isearch-orig-buffer (current-buffer)))))))

(nskk-describe "nskk--isearch-teardown hook"
  (nskk-it "clears orig-buffer"
    (with-temp-buffer
      (setq nskk--isearch-orig-buffer (current-buffer))
      (nskk--isearch-teardown)
      (should (null nskk--isearch-orig-buffer)))))

(defun nskk-isearch-test--call-with-isolated-lifecycle (function)
  "Call FUNCTION with isolated physical and ownership lifecycle state."
  (let ((saved-enable nskk-isearch-enable)
        (saved-orig-buffer nskk--isearch-orig-buffer)
        (saved-orig-buffer-stack nskk--isearch-orig-buffer-stack)
        (saved-resource-state (nskk-isearch-resource-state))
        (saved-watcher-present (nskk--isearch-watcher-present-p))
        (nskk--isearch-mode-hook-owned nil)
        (nskk--isearch-mode-end-hook-owned nil)
        (nskk--isearch-prompt-advice-owned nil)
        (nskk--isearch-enable-watcher-owned nil))
    (unwind-protect
        (progn
          (nskk--isearch-restore-watcher-presence nil)
          (nskk--isearch-restore-resource-state '(nil nil nil))
          (setq nskk-isearch-enable nil
                nskk--isearch-orig-buffer nil
                nskk--isearch-orig-buffer-stack nil)
          (nskk--isearch-register-enable-watcher)
          (funcall function))
      (nskk--isearch-restore-watcher-presence nil)
      (nskk--isearch-restore-resource-state '(nil nil nil))
      (setq nskk-isearch-enable saved-enable
            nskk--isearch-orig-buffer saved-orig-buffer
            nskk--isearch-orig-buffer-stack saved-orig-buffer-stack)
      (nskk--isearch-restore-resource-state saved-resource-state)
      (nskk--isearch-restore-watcher-presence saved-watcher-present))))

(nskk-describe "nskk-isearch watcher lifecycle"
  (nskk-it "keeps one callback after repeated registration evaluation"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (let ((setup-count 0)
             (teardown-count 0))
         (cl-letf (((symbol-function 'nskk-isearch-setup)
                    (lambda () (cl-incf setup-count)))
                   ((symbol-function 'nskk-isearch-teardown)
                    (lambda () (cl-incf teardown-count))))
           (dotimes (_ 3)
             (eval '(nskk--isearch-register-enable-watcher)))
           (should (= 1 (cl-count #'nskk--isearch-enable-watcher
                                  (get-variable-watchers
                                   'nskk-isearch-enable)
                                  :test #'eq)))
           (setq nskk-isearch-enable t)
           (setq nskk-isearch-enable nil)
           (should (= setup-count 1))
           (should (= teardown-count 1)))))))

  (nskk-it "sets up and tears down integration when the option changes"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (setq nskk-isearch-enable t)
       (should (memq #'nskk--isearch-setup isearch-mode-hook))
       (should (memq #'nskk--isearch-teardown isearch-mode-end-hook))
       (should (advice-member-p #'nskk--isearch-prompt-advice
                                'isearch-message-prefix))
       (setq nskk--isearch-orig-buffer (current-buffer))
       (setq nskk-isearch-enable nil)
       (should-not (memq #'nskk--isearch-setup isearch-mode-hook))
       (should-not (memq #'nskk--isearch-teardown isearch-mode-end-hook))
       (should-not (advice-member-p #'nskk--isearch-prompt-advice
                                    'isearch-message-prefix))
       (should-not nskk--isearch-orig-buffer)
       (should (= 1 (cl-count #'nskk--isearch-enable-watcher
                              (get-variable-watchers 'nskk-isearch-enable)
                              :test #'eq))))))

  (nskk-it "unload removes the watcher and disables future option effects"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (setq nskk-isearch-enable t)
       (setq nskk--isearch-orig-buffer (current-buffer))
       (should-not (nskk-isearch-unload-function))
       (should-not (memq #'nskk--isearch-setup isearch-mode-hook))
       (should-not (memq #'nskk--isearch-teardown isearch-mode-end-hook))
       (should-not (advice-member-p #'nskk--isearch-prompt-advice
                                    'isearch-message-prefix))
       (should-not nskk--isearch-orig-buffer)
       (should-not (memq #'nskk--isearch-enable-watcher
                         (get-variable-watchers 'nskk-isearch-enable)))
       (let ((setup-count 0)
             (teardown-count 0))
         (cl-letf (((symbol-function 'nskk-isearch-setup)
                    (lambda () (cl-incf setup-count)))
                   ((symbol-function 'nskk-isearch-teardown)
                    (lambda () (cl-incf teardown-count))))
           (setq nskk-isearch-enable nil)
           (setq nskk-isearch-enable t)
           (should (= setup-count 0))
           (should (= teardown-count 0))))))))

(defun nskk-isearch-test--all-resource-states ()
  "Return every physical or ownership state for the three resources."
  '((nil nil nil)
    (nil nil t)
    (nil t nil)
    (nil t t)
    (t nil nil)
    (t nil t)
    (t t nil)
    (t t t)))

(defun nskk-isearch-test--stage-state (stage)
  "Return a resource state with only STAGE present."
  (pcase stage
    ('mode-hook '(t nil nil))
    ('mode-end-hook '(nil t nil))
    ('advice '(nil nil t))
    (_ '(nil nil nil))))

(defun nskk-isearch-test--mutate-with-fault
    (target timing condition condition-data mutation)
  "Run MUTATION, signaling CONDITION at TIMING when TARGET is non-nil."
  (if (not target)
      (funcall mutation)
    (when (eq timing 'before)
      (signal condition condition-data))
    (prog1 (funcall mutation)
      (signal condition condition-data))))

(defun nskk-isearch-test--call-with-removal-fault
    (stage timing condition condition-data function)
  "Call FUNCTION while faulting removal of STAGE at TIMING."
  (let ((original-remove-hook (symbol-function 'remove-hook))
        (original-advice-remove (symbol-function 'advice-remove))
        (original-remove-watcher
         (symbol-function 'remove-variable-watcher)))
    (cl-letf
        (((symbol-function 'remove-hook)
          (lambda (hook callback &optional local)
            (nskk-isearch-test--mutate-with-fault
             (or (and (eq stage 'mode-hook)
                      (eq hook 'isearch-mode-hook)
                      (eq callback #'nskk--isearch-setup))
                 (and (eq stage 'mode-end-hook)
                      (eq hook 'isearch-mode-end-hook)
                      (eq callback #'nskk--isearch-teardown)))
             timing condition condition-data
             (lambda ()
               (funcall original-remove-hook hook callback local)))))
         ((symbol-function 'advice-remove)
          (lambda (symbol callback)
            (nskk-isearch-test--mutate-with-fault
             (and (eq stage 'advice)
                  (eq symbol 'isearch-message-prefix)
                  (eq callback #'nskk--isearch-prompt-advice))
             timing condition condition-data
             (lambda ()
               (funcall original-advice-remove symbol callback)))))
         ((symbol-function 'remove-variable-watcher)
          (lambda (symbol callback)
            (nskk-isearch-test--mutate-with-fault
             (and (eq stage 'watcher)
                  (eq symbol 'nskk-isearch-enable)
                  (eq callback #'nskk--isearch-enable-watcher))
             timing condition condition-data
             (lambda ()
               (funcall original-remove-watcher symbol callback))))))
      (funcall function))))

(nskk-describe "nskk-isearch ownership matrices"
  (nskk-it "round-trips setup and teardown for all eight initial states"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (dolist (initial-state (nskk-isearch-test--all-resource-states))
         (nskk--isearch-restore-resource-state '(nil nil nil))
         (nskk--isearch-set-ownership-state '(nil nil nil))
         (nskk--isearch-restore-resource-state initial-state)
         (nskk-isearch-setup)
         (nskk-isearch-setup)
         (should (equal (nskk-isearch-resource-state) '(t t t)))
         (should
          (equal (nskk--isearch-ownership-state)
                 (mapcar #'not initial-state)))
         (setq nskk--isearch-orig-buffer (current-buffer))
         (nskk-isearch-teardown)
         (nskk-isearch-teardown)
         (should (equal (nskk-isearch-resource-state) initial-state))
         (should (equal (nskk--isearch-ownership-state)
                        '(nil nil nil)))
         (should-not nskk--isearch-orig-buffer)))))

  (nskk-it "tears down all 64 physical and ownership combinations"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (dolist (physical-state (nskk-isearch-test--all-resource-states))
         (dolist (ownership-state
                  (nskk-isearch-test--all-resource-states))
           (nskk--isearch-restore-resource-state '(nil nil nil))
           (nskk--isearch-set-ownership-state '(nil nil nil))
           (nskk--isearch-restore-resource-state physical-state)
           (nskk--isearch-set-ownership-state ownership-state)
           (setq nskk--isearch-orig-buffer (current-buffer))
           (let ((expected
                  (cl-mapcar (lambda (present owned)
                               (and present (not owned)))
                             physical-state ownership-state)))
             (nskk-isearch-teardown)
             (nskk-isearch-teardown)
             (should (equal (nskk-isearch-resource-state) expected))
             (should (equal (nskk--isearch-ownership-state)
                            '(nil nil nil)))
             (should-not nskk--isearch-orig-buffer)))))))

  (nskk-it "preserves preexisting nonowned watcher and resources on unload"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (nskk--isearch-restore-resource-state '(t t t))
       (nskk--isearch-set-ownership-state '(nil nil nil))
       (nskk--isearch-restore-watcher-presence nil)
       (setq nskk--isearch-enable-watcher-owned nil)
       (add-variable-watcher 'nskk-isearch-enable
                             #'nskk--isearch-enable-watcher)
       (nskk--isearch-register-enable-watcher)
       (nskk--isearch-register-enable-watcher)
       (should-not nskk--isearch-enable-watcher-owned)
       (should (= 1
                  (cl-count #'nskk--isearch-enable-watcher
                            (get-variable-watchers 'nskk-isearch-enable)
                            :test #'eq)))
       (should-not (nskk-isearch-unload-function))
       (should (equal (nskk-isearch-resource-state) '(t t t)))
       (should (nskk--isearch-watcher-present-p))
       (should-not nskk--isearch-enable-watcher-owned)
       (should (= 1
                  (cl-count #'nskk--isearch-enable-watcher
                            (get-variable-watchers 'nskk-isearch-enable)
                            :test #'eq)))))))

(nskk-describe "nskk-isearch adversarial cleanup"
  (nskk-it "continues teardown after every before or after removal fault"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (dolist (stage '(mode-hook mode-end-hook advice))
         (dolist (timing '(before after))
           (dolist (condition '(error quit))
             (nskk--isearch-restore-resource-state '(nil nil nil))
             (nskk--isearch-set-ownership-state '(nil nil nil))
             (nskk-isearch-setup)
             (setq nskk--isearch-orig-buffer (current-buffer))
             (let* ((payload (list stage timing condition))
                    (data (list payload))
                    (caught
                     (nskk-isearch-test--call-with-removal-fault
                      stage timing condition data
                      (lambda ()
                        (condition-case condition-data
                            (progn (nskk-isearch-teardown) nil)
                          ((error quit) condition-data))))))
               (should (eq (car caught) condition))
               (should (eq (cdr caught) data))
               (should (eq (car (cdr caught)) payload))
               (should-not nskk--isearch-orig-buffer)
               (let ((expected
                      (if (eq timing 'before)
                          (nskk-isearch-test--stage-state stage)
                        '(nil nil nil))))
                 (should (equal (nskk-isearch-resource-state) expected))
                 (should (equal (nskk--isearch-ownership-state)
                                expected)))
               (nskk-isearch-teardown)
               (nskk-isearch-teardown)
               (should (equal (nskk-isearch-resource-state)
                              '(nil nil nil)))
               (should (equal (nskk--isearch-ownership-state)
                              '(nil nil nil))))))))))

  (nskk-it "continues unload after every before or after removal fault"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (dolist (stage '(mode-hook mode-end-hook advice watcher))
         (dolist (timing '(before after))
           (dolist (condition '(error quit))
             (nskk--isearch-restore-resource-state '(nil nil nil))
             (nskk--isearch-set-ownership-state '(nil nil nil))
             (nskk--isearch-restore-watcher-presence nil)
             (setq nskk--isearch-enable-watcher-owned nil)
             (nskk-isearch-setup)
             (nskk--isearch-register-enable-watcher)
             (setq nskk--isearch-orig-buffer (current-buffer))
             (let* ((payload (list stage timing condition))
                    (data (list payload))
                    (caught
                     (nskk-isearch-test--call-with-removal-fault
                      stage timing condition data
                      (lambda ()
                        (condition-case condition-data
                            (progn (nskk-isearch-unload-function) nil)
                          ((error quit) condition-data))))))
               (should (eq (car caught) condition))
               (should (eq (cdr caught) data))
               (should (eq (car (cdr caught)) payload))
               (should-not nskk--isearch-orig-buffer)
               (let ((expected
                      (if (and (eq timing 'before)
                               (not (eq stage 'watcher)))
                          (nskk-isearch-test--stage-state stage)
                        '(nil nil nil))))
                 (should (equal (nskk-isearch-resource-state) expected))
                 (should (equal (nskk--isearch-ownership-state)
                                expected)))
               (should (eq (nskk--isearch-watcher-present-p)
                           (and (eq timing 'before)
                                (eq stage 'watcher))))
               (should (eq nskk--isearch-enable-watcher-owned
                           (and (eq timing 'before)
                                (eq stage 'watcher))))
               (nskk-isearch-unload-function)
               (nskk-isearch-unload-function)
               (should (equal (nskk-isearch-resource-state)
                              '(nil nil nil)))
               (should (equal (nskk--isearch-ownership-state)
                              '(nil nil nil)))
               (should-not (nskk--isearch-watcher-present-p))
               (should-not nskk--isearch-enable-watcher-owned)))))))))

(nskk-describe "nskk-isearch cleanup convergence"
  (nskk-it "rolls back watcher registration faults with condition identity"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (dolist (timing '(before after))
         (dolist (condition '(error quit))
           (nskk--isearch-restore-watcher-presence nil)
           (setq nskk--isearch-enable-watcher-owned nil)
           (let* ((payload (list 'register timing condition))
                  (data (list payload))
                  (original-add
                   (symbol-function 'add-variable-watcher))
                  caught)
             (cl-letf (((symbol-function 'add-variable-watcher)
                        (lambda (symbol function)
                          (if (and (eq symbol 'nskk-isearch-enable)
                                   (eq function
                                       #'nskk--isearch-enable-watcher))
                              (nskk-isearch-test--mutate-with-fault
                               'watcher timing condition data
                               (lambda ()
                                 (funcall original-add symbol function)))
                            (funcall original-add symbol function)))))
               (setq caught
                     (condition-case condition-data
                         (progn
                           (nskk--isearch-register-enable-watcher)
                           nil)
                       ((error quit) condition-data))))
             (should (eq (car caught) condition))
             (should (eq (cdr caught) data))
             (should (eq (car (cdr caught)) payload))
             (should-not (nskk--isearch-watcher-present-p))
             (should-not nskk--isearch-enable-watcher-owned))
           (nskk--isearch-register-enable-watcher)
           (nskk--isearch-register-enable-watcher)
           (should (nskk--isearch-watcher-present-p))
           (should nskk--isearch-enable-watcher-owned)
           (should (= (cl-count #'nskk--isearch-enable-watcher
                                (get-variable-watchers
                                 'nskk-isearch-enable))
                      1)))))))

  (nskk-it "keeps the first cleanup condition while attempting later cleanup"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (nskk--isearch-restore-transaction-state '((t t t) (t t t)))
       (setq nskk--isearch-orig-buffer (current-buffer))
       (let* ((first-payload (list 'first-removal))
              (first-data (list first-payload))
              (later-payload (list 'later-removal))
              (later-data (list later-payload))
              (later-called nil)
              (original-remove-hook
               (symbol-function 'remove-hook))
              caught)
         (cl-letf (((symbol-function 'remove-hook)
                    (lambda (hook function &optional local)
                      (cond
                       ((and (eq hook 'isearch-mode-hook)
                             (eq function #'nskk--isearch-setup))
                        (signal 'error first-data))
                       ((and (eq hook 'isearch-mode-end-hook)
                             (eq function #'nskk--isearch-teardown))
                        (setq later-called t)
                        (signal 'quit later-data))
                       (t
                        (funcall original-remove-hook
                                 hook function local))))))
           (setq caught
                 (condition-case condition-data
                     (progn (nskk-isearch-teardown) nil)
                   ((error quit) condition-data))))
         (should later-called)
         (should (eq (car caught) 'error))
         (should (eq (cdr caught) first-data))
         (should (eq (car (cdr caught)) first-payload))
         (should-not nskk--isearch-orig-buffer)
         (nskk-isearch-teardown)
         (should (equal (nskk-isearch-resource-state)
                        '(nil nil nil)))
         (should (equal (nskk--isearch-ownership-state)
                        '(nil nil nil)))))))

  (nskk-it "removes teardown recontamination in final reconciliation"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (nskk--isearch-restore-transaction-state '((t t t) (t t t)))
       (let ((original-remove-hook
              (symbol-function 'remove-hook))
             (recontaminated nil))
         (cl-letf (((symbol-function 'remove-hook)
                    (lambda (hook function &optional local)
                      (funcall original-remove-hook hook function local)
                      (when (and (not recontaminated)
                                 (eq hook 'isearch-mode-end-hook)
                                 (eq function
                                     #'nskk--isearch-teardown))
                        (setq recontaminated t)
                        (add-hook 'isearch-mode-hook
                                  #'nskk--isearch-setup)))))
           (nskk-isearch-teardown))
         (should recontaminated)
         (should (equal (nskk-isearch-resource-state)
                        '(nil nil nil)))
         (should (equal (nskk--isearch-ownership-state)
                        '(nil nil nil)))))))

  (nskk-it "removes unload recontamination after watcher cleanup"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (nskk-isearch-setup)
       (nskk--isearch-register-enable-watcher)
       (let ((original-remove-watcher
              (symbol-function 'remove-variable-watcher))
             (recontaminated nil))
         (cl-letf (((symbol-function 'remove-variable-watcher)
                    (lambda (symbol function)
                      (funcall original-remove-watcher symbol function)
                      (when (and (not recontaminated)
                                 (eq symbol 'nskk-isearch-enable)
                                 (eq function
                                     #'nskk--isearch-enable-watcher))
                        (setq recontaminated t)
                        (advice-add 'isearch-message-prefix
                                    :around
                                    #'nskk--isearch-prompt-advice)))))
           (nskk-isearch-unload-function))
         (should recontaminated)
         (should (equal (nskk-isearch-resource-state)
                        '(nil nil nil)))
         (should (equal (nskk--isearch-ownership-state)
                        '(nil nil nil)))
         (should-not (nskk--isearch-watcher-present-p))
         (should-not nskk--isearch-enable-watcher-owned)))))

  (nskk-it "converges watcher registration after adversarial removal"
    (nskk-isearch-test--call-with-isolated-lifecycle
     (lambda ()
       (nskk--isearch-restore-watcher-presence nil)
       (setq nskk--isearch-enable-watcher-owned nil)
       (let ((original-add
              (symbol-function 'add-variable-watcher))
             (original-remove
              (symbol-function 'remove-variable-watcher))
             (add-count 0))
         (cl-letf (((symbol-function 'add-variable-watcher)
                    (lambda (symbol function)
                      (funcall original-add symbol function)
                      (when (and (= add-count 0)
                                 (eq symbol 'nskk-isearch-enable)
                                 (eq function
                                     #'nskk--isearch-enable-watcher))
                        (funcall original-remove symbol function))
                      (setq add-count (1+ add-count)))))
           (nskk--isearch-register-enable-watcher))
         (should (= add-count 2))
         (should (nskk--isearch-watcher-present-p))
         (should nskk--isearch-enable-watcher-owned)
         (should (= (cl-count #'nskk--isearch-enable-watcher
                              (get-variable-watchers
                               'nskk-isearch-enable))
                    1)))))))

(nskk-describe "nskk isearch nested origins"
  (nskk-it "restores nested origins in LIFO order without lifecycle drift"
    (let ((outer (generate-new-buffer " *nskk-isearch-outer*"))
          (inner (generate-new-buffer " *nskk-isearch-inner*"))
          (nskk--isearch-orig-buffer nil)
          (nskk--isearch-orig-buffer-stack nil)
          (setup-function (symbol-function 'nskk--isearch-setup))
          (teardown-function (symbol-function 'nskk--isearch-teardown))
          (resource-state (nskk-isearch-resource-state))
          (watcher-state (nskk--isearch-watcher-state)))
      (unwind-protect
          (progn
            (with-current-buffer outer
              (nskk--isearch-setup))
            (should (eq nskk--isearch-orig-buffer outer))
            (with-current-buffer inner
              (nskk--isearch-setup))
            (should (eq nskk--isearch-orig-buffer inner))
            (should (equal nskk--isearch-orig-buffer-stack
                           (list outer nil)))
            (kill-buffer inner)
            (nskk--isearch-teardown)
            (should (eq nskk--isearch-orig-buffer outer))
            (should (equal nskk--isearch-orig-buffer-stack '(nil)))
            (nskk--isearch-teardown)
            (should-not nskk--isearch-orig-buffer)
            (should-not nskk--isearch-orig-buffer-stack)
            (nskk--isearch-teardown)
            (should-not nskk--isearch-orig-buffer)
            (should-not nskk--isearch-orig-buffer-stack)
            (should (eq setup-function
                        (symbol-function 'nskk--isearch-setup)))
            (should (eq teardown-function
                        (symbol-function 'nskk--isearch-teardown)))
            (should (equal resource-state
                           (nskk-isearch-resource-state)))
            (should (equal watcher-state
                           (nskk--isearch-watcher-state))))
        (when (buffer-live-p inner)
          (kill-buffer inner))
        (when (buffer-live-p outer)
          (kill-buffer outer)))))

  (nskk-it "clears all origin frames on public teardown and unload"
    (dolist (cleanup '(nskk-isearch-teardown
                       nskk-isearch-unload-function))
      (nskk-isearch-test--call-with-isolated-lifecycle
       (lambda ()
         (setq nskk--isearch-orig-buffer (current-buffer)
               nskk--isearch-orig-buffer-stack
               (list (current-buffer) nil))
         (funcall cleanup)
         (should-not nskk--isearch-orig-buffer)
         (should-not nskk--isearch-orig-buffer-stack))))))

(ert-deftest nskk-isearch-prompt-forwards-arguments ()
  (let ((nskk-isearch-enable nil)
        received)
    (should (equal "prompt"
                   (nskk--isearch-prompt-advice
                    (lambda (&rest args) (setq received args) "prompt")
                    'ellipsis 'nonincremental)))
    (should (equal received '(ellipsis nonincremental)))))

(ert-deftest nskk-isearch-teardown-releases-input-buffer-and-owned-map ()
  (nskk-isearch-test--call-with-isolated-lifecycle
   (lambda ()
     (let* ((input (generate-new-buffer " *nskk-isearch-test-input*"))
            (previous (make-sparse-keymap))
            (owned (make-sparse-keymap))
            (overriding-terminal-local-map owned)
            (nskk--isearch-input-sessions (list (list input owned previous))))
       (unwind-protect
           (progn
             (nskk-isearch-teardown)
             (should-not (buffer-live-p input))
             (should-not nskk--isearch-input-sessions)
             (should (eq overriding-terminal-local-map previous)))
         (when (buffer-live-p input) (kill-buffer input)))))))

(ert-deftest nskk-isearch-end-preserves-already-restored-map ()
  (let* ((input (generate-new-buffer " *nskk-isearch-test-input*"))
         (restored (make-sparse-keymap))
         (overriding-terminal-local-map restored)
         (nskk--isearch-orig-buffer nil)
         (nskk--isearch-orig-buffer-stack nil)
         (nskk--isearch-input-sessions
          (list (list input (make-sparse-keymap) (make-sparse-keymap)))))
    (unwind-protect
        (progn
          (nskk--isearch-teardown)
          (should-not (buffer-live-p input))
          (should (eq overriding-terminal-local-map restored)))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest nskk-isearch-setup-failure-releases-partial-session ()
  (require 'nskk)
  (let ((nskk--isearch-input-sessions nil)
        (nskk--isearch-orig-buffer nil)
        (nskk--isearch-orig-buffer-stack nil)
        (overriding-terminal-local-map (make-sparse-keymap))
        (nskk-mode t)
        input)
    (let ((previous overriding-terminal-local-map))
      (cl-letf (((symbol-function 'nskk--isearch-origin-mode)
                 (lambda (_) 'hiragana))
                ((symbol-function 'nskk-mode)
                 (lambda (_) (setq input (current-buffer))
                   (error "Injected setup failure"))))
        (unwind-protect
            (progn
              (should-error (nskk--isearch-setup))
              (should-not (buffer-live-p input))
              (should-not nskk--isearch-input-sessions)
              (should-not nskk--isearch-orig-buffer)
              (should-not nskk--isearch-orig-buffer-stack)
              (should (eq previous overriding-terminal-local-map)))
          (when (buffer-live-p input) (kill-buffer input)))))))

(ert-deftest nskk-isearch-origin-failure-preserves-session-state ()
  (require 'nskk)
  (let* ((nskk-mode t)
         (nskk--isearch-orig-buffer (current-buffer))
         (nskk--isearch-orig-buffer-stack '(outer))
         (nskk--isearch-input-sessions '(outer))
         (overriding-terminal-local-map (make-sparse-keymap))
         (previous overriding-terminal-local-map))
    (cl-letf (((symbol-function 'nskk--isearch-origin-mode)
               (lambda (_) (error "Injected origin failure"))))
      (should-error (nskk--isearch-setup)))
    (should (eq nskk--isearch-orig-buffer (current-buffer)))
    (should (equal nskk--isearch-orig-buffer-stack '(outer)))
    (should (equal nskk--isearch-input-sessions '(outer)))
    (should (eq overriding-terminal-local-map previous))))

(ert-deftest nskk-isearch-pending-message-preserves-query ()
  (require 'nskk)
  (with-temp-buffer
    (insert "▼かな")
    (let* ((overlay (make-overlay 2 (point-max)))
           (nskk--romaji-buffer "")
           (nskk--isearch-input-sessions (list (list (current-buffer))))
           (isearch-message "prefix")
           observed)
      (overlay-put overlay 'display "仮名")
      (cl-letf (((symbol-function 'nskk-state-conversion-overlay)
                 (lambda (&rest _) overlay))
                ((symbol-function 'isearch-message)
                 (lambda (&rest _) (setq observed isearch-message))))
        (nskk--isearch-pending-message))
      (should (equal observed "prefix▼仮名"))
      (should (equal isearch-message "prefix"))
      (should (equal (buffer-string) "▼かな")))))

(ert-deftest nskk-isearch-cleanup-hook-failure-restores-session ()
  (require 'nskk)
  (dolist (phase '(setup teardown))
    (let ((nskk--isearch-input-sessions nil)
          (nskk--isearch-orig-buffer nil)
          (nskk--isearch-orig-buffer-stack nil)
          (overriding-terminal-local-map (make-sparse-keymap))
          (nskk-mode t)
          input)
      (let ((previous overriding-terminal-local-map))
        (cl-letf (((symbol-function 'nskk--isearch-origin-mode)
                   (lambda (_) 'hiragana))
                  ((symbol-function 'nskk-set-mode) #'ignore)
                  ((symbol-function 'nskk-mode)
                   (lambda (_) (setq input (current-buffer))
                     (add-hook 'kill-buffer-hook
                               (lambda () (error "cleanup failure")) nil t)
                     (when (eq phase 'setup) (error "setup failure")))))
          (unwind-protect
              (let ((condition
                     (should-error
                      (progn (nskk--isearch-setup)
                             (nskk--isearch-teardown)))))
                (should (equal (cadr condition)
                               (if (eq phase 'setup) "setup failure"
                                 "cleanup failure")))
                (when (eq phase 'setup)
                  (should (equal (plist-get (cddr condition) :cleanup-errors)
                                 '((error "cleanup failure")))))
                (should-not (buffer-live-p input))
                (should-not nskk--isearch-input-sessions)
                (should-not nskk--isearch-orig-buffer)
                (should-not nskk--isearch-orig-buffer-stack)
                (should (eq previous overriding-terminal-local-map)))
            (when (buffer-live-p input)
              (with-current-buffer input (setq kill-buffer-hook nil))
              (kill-buffer input))))))))

(ert-deftest nskk-isearch-cleanup-retains-failed-buffer-for-public-teardown ()
  (let ((nskk--isearch-input-sessions nil)
        (nskk--isearch-pending-buffers nil)
        (nskk--isearch-orig-buffer nil)
        (nskk--isearch-orig-buffer-stack nil)
        (input (generate-new-buffer " *isearch-disposal-test*"))
        (attempts 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'kill-buffer)
                     (lambda (buffer)
                       (should (eq buffer input))
                       (cl-incf attempts)
                       (error "injected disposal failure"))))
            (should-error (nskk--isearch-dispose-input input)))
          (should (= attempts 2))
          (should (equal nskk--isearch-pending-buffers (list input)))
          (nskk-isearch-teardown)
          (should-not (buffer-live-p input))
          (should-not nskk--isearch-pending-buffers))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest nskk-isearch-cleanup-does-not-change-origin-hooks ()
  (with-temp-buffer
    (let* ((origin (current-buffer))
           (hook (lambda () (error "origin hook must remain")))
           (input (generate-new-buffer " *isearch-owned-test*"))
           (nskk--isearch-pending-buffers nil))
      (setq-local kill-buffer-hook (list hook))
      (unwind-protect
          (progn
            (with-current-buffer input
              (setq-local kill-buffer-query-functions (list (lambda () nil))))
            (should-error (nskk--isearch-dispose-input input))
            (should-not (buffer-live-p input))
            (should (buffer-live-p origin))
            (should (equal kill-buffer-hook (list hook))))
        (setq kill-buffer-hook nil)
        (when (buffer-live-p input) (kill-buffer input))))))

(provide 'nskk-isearch-test)

;;; nskk-isearch-test.el ends here
