;;; nskk-candidate-backend-test.el --- Live customization tests -*- lexical-binding: t; -*-

(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'cl-lib)
(require 'nskk)

(defmacro nskk-test-with-candidate-backend (&rest body)
  "Run BODY with buffer-local candidate resources and no display callbacks."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((nskk-current-state (nskk-state-create 'hiragana))
           (nskk-henkan-show-candidates-functions nil)
           (nskk-henkan-hide-candidates-functions nil)
           (nskk-henkan-select-candidate-by-key-function nil))
       ,@body)))

(nskk-describe "candidate backend customization"
  (nskk-it "replaces the live backend before selecting a candidate"
    (nskk-test-with-candidate-backend
     (let (events)
       (setq nskk-henkan-show-candidates-functions
             (list (lambda (candidates index) (push (list 'old-show candidates index) events)))
             nskk-henkan-hide-candidates-functions
             (list (lambda () (push 'old-hide events))))
       (nskk--run-candidate-show-transaction '("a" "b") 1 1 nil 0)
       (setq nskk-henkan-show-candidates-functions
             (list (lambda (candidates index) (push (list 'new-show candidates index) events)))
             nskk-henkan-hide-candidates-functions
             (list (lambda () (push 'new-hide events)))
             nskk-henkan-select-candidate-by-key-function
             (lambda (key candidates index)
               (push (list 'select key candidates index) events)
               index))
       (should (= 1 (nskk--candidate-select ?a '("a" "b") 1)))
       (nskk--dismiss-candidate-list)
       (should (equal (nreverse events)
                      '((old-show ("a" "b") 1) old-hide
			(new-show ("a" "b") 1) (select 97 ("a" "b") 1) new-hide))))))

  (nskk-it "retains old cleanup functions when callback symbols are redefined"
    (nskk-test-with-candidate-backend
     (let ((show (make-symbol "show"))
           (hide (make-symbol "hide")) events)
       (fset show (lambda (&rest _) (push 'old-show events)))
       (fset hide (lambda () (push 'old-hide events)))
       (setq nskk-henkan-show-candidates-functions (list show)
             nskk-henkan-hide-candidates-functions (list hide))
       (nskk--run-candidate-show-transaction '("a") 0 0 nil 0)
       (fset show (lambda (&rest _) (push 'new-show events)))
       (fset hide (lambda () (push 'new-hide events)))
       (nskk--candidate-select ?a '("a") 0)
       (nskk--dismiss-candidate-list)
       (should (equal (nreverse events) '(old-show old-hide new-show new-hide))))))

  (nskk-it "preserves show errors and quits while running every cleanup"
    (dolist (kind '(error quit))
      (nskk-test-with-candidate-backend
       (let ((cleaned nil))
         (setq nskk-henkan-show-candidates-functions
               (list (lambda (&rest _) (signal kind '(original))))
               nskk-henkan-hide-candidates-functions
               (list (lambda () (signal 'error '(cleanup)))
                     (lambda () (setq cleaned t))))
         (should (equal (condition-case condition
                            (nskk--run-candidate-show-transaction '("a") 0 0 nil 0)
                          ((error quit) condition))
			(list kind 'original)))
         (should cleaned)
         (should-not nskk--candidate-backend)
         (should-not nskk--henkan-candidate-list-active)))))

  (nskk-it "leaves the new UI untouched when old cleanup fails"
    (nskk-test-with-candidate-backend
     (let (events)
       (setq nskk-henkan-hide-candidates-functions
             (list (lambda () (push 'old-hide events) (error "old"))))
       (nskk--run-candidate-show-transaction '("a") 0 0 nil 0)
       (setq nskk-henkan-hide-candidates-functions
             (list (lambda () (push 'new-hide events))))
       (should-error (nskk--candidate-select ?a '("a") 0))
       (should (equal events '(old-hide)))
       (should-not nskk--candidate-backend))))

  (nskk-it "captures global callbacks spliced into buffer-local hooks"
    (let ((saved (default-value 'nskk-henkan-hide-candidates-functions)) events)
      (unwind-protect
          (progn
            (set-default 'nskk-henkan-hide-candidates-functions
			 (list (lambda () (push 'global events))))
            (with-temp-buffer
              (setq-local nskk-henkan-hide-candidates-functions
                          (list (lambda () (push 'local events)) t))
              (let ((nskk--candidate-backend (nskk--candidate-current-backend)))
		(set-default 'nskk-henkan-hide-candidates-functions nil)
		(nskk--dismiss-candidate-list)))
            (should (equal (nreverse events) '(local global))))
	(set-default 'nskk-henkan-hide-candidates-functions saved))))

  (nskk-it "uses live selection keys for both drawing and selection"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?z ?y))
            (nskk-henkan-number-to-display-candidates 2)
            (nskk-candidate-format-function
             (lambda (candidates keys remaining)
               (should (equal candidates '("a" "b")))
               (should (equal keys '(?z ?y)))
               (should (= remaining 0))
               "custom")))
	(unwind-protect
            (progn
              (nskk-candidate-show-list '("a" "b") 0)
              (should (equal (overlay-get (nskk-state-candidate-overlay) 'after-string) "custom"))
              (should (= 1 (nskk-candidate-list-select-by-key ?y '("a" "b") 0)))
              (should-not (nskk-candidate-list-select-by-key ?s '("a" "b") 0)))
          (nskk-candidate-hide-list)))))

  (nskk-it "recognizes added and removed custom keymap commands"
    (with-temp-buffer
      (let ((nskk-mode-map (make-sparse-keymap))
            (command (lambda () (interactive))))
	(nskk--setup-buffer)
	(should-not (nskk--bound-command-p command))
	(define-key nskk-mode-map (kbd "C-c x") command)
	(should (nskk--bound-command-p command))
	(define-key nskk-mode-map (kbd "C-c x") nil)
	(should-not (nskk--bound-command-p command))
	(should (nskk--bound-command-p 'nskk-next-candidate))
	(should (nskk--bound-command-p 'completion-at-point)))))

  (nskk-it "preserves selector errors and quits while running every cleanup"
    (dolist (kind '(error quit))
      (nskk-test-with-candidate-backend
       (let (cleaned)
         (setq nskk-henkan-select-candidate-by-key-function
               (lambda (&rest _) (signal kind '(original)))
               nskk-henkan-hide-candidates-functions
               (list (lambda () (signal 'error '(cleanup)))
                     (lambda () (setq cleaned t))))
         (nskk--run-candidate-show-transaction '("a") 0 0 nil 0)
         (should (equal (condition-case condition
                            (nskk--candidate-select ?a '("a") 0)
                          ((error quit) condition))
			(list kind 'original)))
         (should cleaned)
         (should-not nskk--candidate-backend)
         (should-not nskk--henkan-candidate-list-active)))))

  (nskk-it "dismisses each buffer with its own captured backend"
    (let ((first (generate-new-buffer " *nskk-first*"))
          (second (generate-new-buffer " *nskk-second*"))
          (nskk-henkan-show-candidates-functions nil)
          (nskk-henkan-hide-candidates-functions nil)
          events)
      (unwind-protect
          (progn
            (dolist (buffer (list first second))
              (with-current-buffer buffer
		(setq-local nskk-henkan-hide-candidates-functions
                            (list (lambda () (push buffer events))))
		(nskk--run-candidate-show-hooks-once '("a") 0)))
            (with-current-buffer first (nskk--dismiss-candidate-list))
            (should (equal events (list first)))
            (with-current-buffer second
              (should nskk--candidate-backend)
              (nskk--dismiss-candidate-list))
            (should (equal events (list second first))))
	(kill-buffer first)
	(kill-buffer second))))

  (nskk-it "maps internal classifications to public key states"
    (dolist (classification '(converting preedit-japanese idle-direct))
      (cl-letf (((symbol-function 'nskk-classify-state) (lambda () classification)))
	(should (eq (nskk-current-key-state)
                    (pcase classification
                      ('converting 'converting)
                      ('preedit-japanese 'preedit)
                      (_ 'normal)))))))

  (nskk-it "restores the key cache together with an isolated database"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 7)
            (candidates '("a" "b" "c" "d")))
	(should (= 2 (nskk-candidate-list-select-by-key ?d candidates 0)))
	(nskk-prolog-test-with-isolated-db
          (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f)))
            (should (= 3 (nskk-candidate-list-select-by-key ?f candidates 0)))))
	(let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f)))
          (should (= 3 (nskk-candidate-list-select-by-key ?f candidates 0))))))))

(provide 'nskk-candidate-backend-test)
;;; nskk-candidate-backend-test.el ends here
