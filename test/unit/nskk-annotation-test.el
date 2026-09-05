;;; nskk-annotation-test.el --- Tests for nskk-annotation.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-annotation.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-annotation)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-annotation function existence"
  (nskk-it "nskk-annotation-initialize is defined"
    (should (fboundp 'nskk-annotation-initialize)))
  (nskk-it "nskk-annotation-register is defined"
    (should (fboundp 'nskk-annotation-register)))
  (nskk-it "nskk-annotation-lookup is defined"
    (should (fboundp 'nskk-annotation-lookup)))
  (nskk-it "nskk-annotation-clear is defined"
    (should (fboundp 'nskk-annotation-clear)))
  (nskk-it "nskk-annotation-toggle-display is defined"
    (should (fboundp 'nskk-annotation-toggle-display)))
  (nskk-it "nskk-annotation-show-for-candidate is defined"
    (should (fboundp 'nskk-annotation-show-for-candidate))))

;;;; Initialization

(nskk-describe "nskk-annotation-initialize"
  (nskk-it "is idempotent"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-initialize)
        (should nskk--annotation-initialized)))))

;;;; Register and Lookup

(nskk-describe "nskk-annotation-register and lookup"
  (nskk-it "registers and retrieves an annotation"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "common kanji")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "common kanji")))))

  (nskk-it "returns nil for unregistered reading+candidate"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (should (null (nskk-annotation-lookup "unknown" "候補"))))))

  (nskk-it "returns nil when not initialized"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (should (null (nskk-annotation-lookup "かんじ" "漢字"))))))

  (nskk-it "distinguishes different readings for same candidate"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "Chinese character")
        (nskk-annotation-register "かんじる" "感じる" "to feel")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "Chinese character"))
        (should (equal (nskk-annotation-lookup "かんじる" "感じる") "to feel")))))

  (nskk-it "distinguishes different candidates for same reading"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "Chinese character")
        (nskk-annotation-register "かんじ" "感じ" "feeling")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "Chinese character"))
        (should (equal (nskk-annotation-lookup "かんじ" "感じ") "feeling")))))

  (nskk-it "keeps the first annotation when the same pair is registered twice"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "first")
        (nskk-annotation-register "かんじ" "漢字" "second")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "first")))))

  (nskk-it "returns an empty annotation registered directly"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "")
        (should (equal (nskk-annotation-lookup "よみ" "候補") ""))))))

;;;; CPS Variant

(nskk-describe "nskk-annotation-lookup/k"
  (nskk-it "invokes on-found with the registered annotation"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil)
            found called)
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "note text")
        (nskk-annotation-lookup/k "よみ" "候補"
                                  (lambda (value) (setq called t found value))
                                  (lambda () (ert-fail "expected on-found")))
        (should called)
        (should (equal found "note text")))))

  (nskk-it "invokes on-not-found when nothing is registered"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil)
            missed)
        (nskk-annotation-initialize)
        (nskk-annotation-lookup/k "よみ" "未登録"
                                  (lambda (_value) (ert-fail "expected on-not-found"))
                                  (lambda () (setq missed t)))
        (should missed))))

  (nskk-it "invokes on-not-found before initialization"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil)
            missed)
        (nskk-annotation-lookup/k "よみ" "候補"
                                  (lambda (_value) (ert-fail "expected on-not-found"))
                                  (lambda () (setq missed t)))
        (should missed)))))

;;;; Format Helper

(nskk-describe "nskk--annotation-format"
  (nskk-it "returns nil for nil annotation"
    (should (null (nskk--annotation-format nil))))
  (nskk-it "returns nil for empty string annotation"
    (should (null (nskk--annotation-format ""))))
  (nskk-it "returns a propertized string for non-empty annotation"
    (let ((result (nskk--annotation-format "test annotation")))
      (should (stringp result))
      (should (string-match-p "test annotation" result))
      (should (string-prefix-p " [" result))
      (should (string-suffix-p "]" result))))
  (nskk-it "applies nskk-annotation-face"
    (let ((result (nskk--annotation-format "hello")))
      (should (eq (get-text-property 0 'face result) 'nskk-annotation-face))))
  (nskk-it "strips input properties and applies only the annotation face"
    (let* ((source (propertize "unsafe"
                              'display "spoofed"
                              'face 'error
                              'keymap (make-sparse-keymap)
                              'help-echo "untrusted"))
           (result (nskk--annotation-format source)))
      (should (equal (substring-no-properties result) " [unsafe]"))
      (dotimes (index (length result))
        (should (equal (text-properties-at index result)
                       '(face nskk-annotation-face)))))))

;;;; Clear

(nskk-describe "nskk-annotation-clear"
  (nskk-it "clears nskk--annotation-current"
    (with-temp-buffer
      (setq nskk--annotation-current "some annotation")
      (nskk-annotation-clear)
      (should (null nskk--annotation-current)))))

;;;; Show for Candidate Guards

(nskk-describe "nskk-annotation-show-for-candidate"
  (nskk-it "clears stale state without lookup or display when disabled"
    (with-temp-buffer
      (let ((nskk-show-annotation nil)
            (nskk--annotation-current "stale")
            (nskk--annotation-visible nil)
            (lookup-calls 0)
            (message-calls 0)
            displayed)
        (cl-letf (((symbol-function 'nskk-annotation-lookup)
                   (lambda (&rest _args)
                     (cl-incf lookup-calls)
                     "new"))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (cl-incf message-calls)
                     (when format-string
                       (setq displayed
                             (apply #'format format-string args))))))
          (nskk-annotation-show-for-candidate "よみ" "候補")
          (should (null nskk--annotation-current))
          (should (= lookup-calls 0))
          (should (= message-calls 0))
          (nskk-annotation-toggle-display)
          (should (= message-calls 1))
          (should-not displayed)))))

  (nskk-it "sets annotation-current when nskk-show-annotation is t"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "note text")
        (with-temp-buffer
          (let ((nskk-show-annotation t)
                (message-log-max nil))
            (nskk-annotation-show-for-candidate "よみ" "候補")
            (should (equal nskk--annotation-current "note text")))))))

  (nskk-it "sets annotation-current to nil for candidate without annotation"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (with-temp-buffer
          (setq nskk--annotation-current "stale")
          (let ((nskk-show-annotation t)
                (message-log-max nil))
            (nskk-annotation-show-for-candidate "よみ" "無注釈")
            (should (null nskk--annotation-current)))))))

  (nskk-it "records the annotation without echoing it when display is toggled off"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "note text")
        (with-temp-buffer
          (let ((nskk-show-annotation t)
                (nskk--annotation-visible nil)
                (message-calls 0))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _args) (cl-incf message-calls))))
              (nskk-annotation-show-for-candidate "よみ" "候補"))
            (should (equal nskk--annotation-current "note text"))
            (should (= message-calls 0)))))))

  (nskk-it "clears the stale annotation even when lookup signals"
    (with-temp-buffer
      (let ((nskk-show-annotation t)
            (nskk--annotation-current "stale"))
        (cl-letf (((symbol-function 'nskk-annotation-lookup)
                   (lambda (&rest _args) (error "lookup failed"))))
          (should-error (nskk-annotation-show-for-candidate "よみ" "候補")))
        (should (null nskk--annotation-current))))))

;;;; Toggle Display

(nskk-describe "nskk-annotation-toggle-display"
  (nskk-it "toggles nskk--annotation-visible"
    (with-temp-buffer
      (let ((nskk--annotation-visible t))
        (nskk-annotation-toggle-display)
        (should (null nskk--annotation-visible))
        (nskk-annotation-toggle-display)
        (should nskk--annotation-visible))))

  (nskk-it "echoes the formatted annotation when toggled on"
    (with-temp-buffer
      (let ((nskk--annotation-visible nil)
            (nskk--annotation-current "test note")
            displayed)
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq displayed
                           (and format-string
                                (apply #'format format-string args))))))
          (nskk-annotation-toggle-display))
        (should nskk--annotation-visible)
        (should (equal (substring-no-properties displayed) " [test note]")))))

  (nskk-it "clears the echo area when toggled off"
    (with-temp-buffer
      (let ((nskk--annotation-visible t)
            (nskk--annotation-current "test note")
            (message-calls 0)
            displayed)
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (cl-incf message-calls)
                     (setq displayed
                           (and format-string
                                (apply #'format format-string args))))))
          (nskk-annotation-toggle-display))
        (should (null nskk--annotation-visible))
        (should (= message-calls 1))
        (should-not displayed))))

  (nskk-it "keeps the echoed annotation out of the message log"
    (with-temp-buffer
      (let ((nskk--annotation-visible nil)
            (nskk--annotation-current "test note")
            (message-log-max 100)
            called
            log-max-during)
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest _args)
                     (setq called t
                           log-max-during message-log-max))))
          (nskk-annotation-toggle-display))
        (should called)
        (should (null log-max-during)))))

  (nskk-it "echoes nothing when the current annotation is empty"
    (with-temp-buffer
      (let ((nskk--annotation-visible nil)
            (nskk--annotation-current "")
            (message-calls 0))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest _args) (cl-incf message-calls))))
          (nskk-annotation-toggle-display))
        (should nskk--annotation-visible)
        (should (= message-calls 0))))))

;;;; Load from Candidates Helper

(nskk-describe "nskk-annotation-load-from-candidates"
  (nskk-it "registers annotations from pair list"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-load-from-candidates
         "よみ"
         '(("候補1" . "annotation1") ("候補2" . nil) ("候補3" . "annotation3")))
        (should (equal (nskk-annotation-lookup "よみ" "候補1") "annotation1"))
        (should (null  (nskk-annotation-lookup "よみ" "候補2")))
        (should (equal (nskk-annotation-lookup "よみ" "候補3") "annotation3")))))

  (nskk-it "skips empty annotation strings"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-load-from-candidates
         "よみ"
         '(("候補" . "")))
        (should (null (nskk-annotation-lookup "よみ" "候補"))))))

  (nskk-it "registers nothing for an empty candidate list"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-load-from-candidates "よみ" nil)
        (should (null (nskk-annotation-lookup "よみ" "候補")))))))

(nskk-describe "untrusted annotation display properties"
    (nskk-it "strips attack properties before applying the annotation face"
      (let* ((source (propertize "unsafe"
                                'display "spoofed"
                                'keymap (make-sparse-keymap)
                                'local-map (make-sparse-keymap)
                                'mouse-face 'highlight
                                'help-echo "untrusted"
                                'face 'error
                                'nskk-no-learn t))
             (source-copy (copy-sequence source))
             (rendered (nskk--annotation-format source)))
        (should (equal (substring-no-properties rendered) " [unsafe]"))
        (dolist (property '(display keymap local-map mouse-face help-echo))
          (should-not
           (text-property-not-all 0 (length rendered) property nil rendered)))
        (dotimes (index (length rendered))
          (should (eq (get-text-property index 'face rendered)
                      'nskk-annotation-face)))
        (should (equal-including-properties source source-copy))
        (should (eq (get-text-property 0 'face source) 'error))
        (should (eq (get-text-property 0 'nskk-no-learn source) t))))
    (nskk-it "sanitizes candidate and annotation copies in echo-area output"
      (let* ((reading (propertize "よみ" 'nskk-no-learn t))
             (candidate (propertize "候補"
                                   'display "spoofed candidate"
                                   'keymap (make-sparse-keymap)
                                   'local-map (make-sparse-keymap)
                                   'mouse-face 'highlight
                                   'help-echo "candidate help"
                                   'face 'error
                                   'nskk-no-learn t))
             (annotation (propertize "注釈"
                                    'display "spoofed annotation"
                                    'keymap (make-sparse-keymap)
                                    'local-map (make-sparse-keymap)
                                    'mouse-face 'highlight
                                    'help-echo "annotation help"
                                    'face 'error
                                    'nskk-no-learn t))
             (candidate-copy (copy-sequence candidate))
             (annotation-copy (copy-sequence annotation))
             lookup-reading lookup-candidate rendered)
        (cl-letf (((symbol-function 'nskk-annotation-lookup)
                   (lambda (actual-reading actual-candidate)
                     (setq lookup-reading actual-reading
                           lookup-candidate actual-candidate)
                     annotation))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq rendered (apply #'format format-string args)))))
          (let ((nskk-show-annotation t)
                (nskk--annotation-visible t))
            (nskk-annotation-show-for-candidate reading candidate)))
        (should (eq lookup-reading reading))
        (should (eq lookup-candidate candidate))
        (should (equal (substring-no-properties rendered) "候補 [注釈]"))
        (dolist (property '(display keymap local-map mouse-face help-echo))
          (should-not
           (text-property-not-all 0 (length rendered) property nil rendered)))
        (dotimes (index (length candidate))
          (should-not (get-text-property index 'face rendered)))
        (let ((index (length candidate)))
          (while (< index (length rendered))
            (should (eq (get-text-property index 'face rendered)
                        'nskk-annotation-face))
            (setq index (1+ index))))
        (should (equal-including-properties candidate candidate-copy))
        (should (equal-including-properties annotation annotation-copy))
        (should (eq (get-text-property 0 'nskk-no-learn candidate) t))
        (should (eq (get-text-property 0 'nskk-no-learn annotation) t)))))
(ert-deftest nskk-annotation-toggle-retains-echo-ownership ()
  (with-temp-buffer
    (let ((nskk--annotation-visible t)
          (nskk--annotation-current "note")
          (nskk--annotation-displayed t))
      (nskk-annotation-toggle-display)
      (should-not nskk--annotation-displayed)
      (should (equal nskk--annotation-current "note"))
      (nskk-annotation-toggle-display)
      (should nskk--annotation-displayed)
      (nskk-annotation-clear)
      (should-not nskk--annotation-displayed)
      (should-not nskk--annotation-current))))

(ert-deftest nskk-annotation-clears-before-candidate-list-hooks ()
  (require 'nskk-henkan)
  (with-temp-buffer
    (let ((nskk--annotation-current "note")
          (nskk--annotation-displayed t)
          (nskk-henkan-show-candidates-functions
           (list (lambda (_candidates _index)
                   (should-not nskk--annotation-current)
                   (should-not nskk--annotation-displayed)
                   (message "candidate list")))))
      (nskk--run-candidate-show-hooks-once '("候補") 0)
      (should nskk--henkan-candidate-list-active))))

(ert-deftest nskk-annotation-context-preserves-equal-candidate-identities ()
  (let* ((first (copy-sequence "12個"))
         (second (copy-sequence "12個"))
         (nskk-current-state (nskk-state-create)))
    (nskk-state-put-metadata nskk-current-state 'annotation-reading "#こ")
    (nskk-state-put-metadata nskk-current-state 'annotation-candidates
                             (list (cons first "#0個") (cons second "12個")))
    (should (equal (mapcar #'nskk--annotation-candidate-context
                           (list second first))
                   '(("#こ" . "12個") ("#こ" . "#0個"))))))

(ert-deftest nskk-annotation-list-suffix-is-pure-and-sanitized ()
  (let* ((nskk-current-state (nskk-state-create))
         (nskk-show-annotation t)
         (nskk--annotation-visible t)
         (nskk--annotation-current "previous")
         (nskk--annotation-displayed t)
         (note (propertize "tab\tinside" 'display "unsafe" 'keymap 'unsafe))
         (original (copy-sequence note)))
    (nskk-state-put-metadata nskk-current-state 'henkan-reading "たぶ")
    (cl-letf (((symbol-function 'nskk-annotation-lookup)
               (lambda (reading candidate)
                 (should (equal reading "たぶ"))
                 (should (equal candidate "次"))
                 note))
              ((symbol-function 'nskk-annotation-show-for-candidate)
               (lambda (&rest _) (ert-fail "List lookup must not display")))
              ((symbol-function 'message)
               (lambda (&rest _) (ert-fail "List lookup must not echo"))))
      (let ((suffix (nskk--annotation-candidate-list-suffix "次")))
        (should (equal (substring-no-properties suffix) ";tab\tinside"))
        (dolist (property '(display keymap))
          (should-not (text-property-not-all 0 (length suffix) property nil suffix))))
      (should (equal nskk--annotation-current "previous"))
      (should nskk--annotation-displayed)
      (should (equal-including-properties note original)))))

(ert-deftest nskk-annotation-list-suffix-honors-both-display-switches ()
  (let ((nskk-current-state (nskk-state-create)))
    (nskk-state-put-metadata nskk-current-state 'henkan-reading "か")
    (cl-letf (((symbol-function 'nskk-annotation-lookup)
               (lambda (&rest _) (ert-fail "Disabled annotations must not look up"))))
      (let ((nskk-show-annotation nil) (nskk--annotation-visible t))
        (should-not (nskk--annotation-candidate-list-suffix "蚊")))
      (let ((nskk-show-annotation t) (nskk--annotation-visible nil))
        (should-not (nskk--annotation-candidate-list-suffix "蚊"))))))

(provide 'nskk-annotation-test)

;;; nskk-annotation-test.el ends here
