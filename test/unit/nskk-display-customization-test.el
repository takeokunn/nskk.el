;;; nskk-display-customization-test.el --- Lisp display customization tests -*- lexical-binding: t; -*-

(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'cl-lib)
(require 'nskk-modeline)
(require 'nskk-show-mode)
(require 'nskk-inline)

(nskk-describe "display customization"
  (nskk-it "refreshes modeline text face and tooltip without changing mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-mode-display-alist
             (list (list 'hiragana (copy-sequence "AA") 'bold "First" nil))))
        (should (equal (substring-no-properties (nskk-modeline-indicator)) " AA"))
        (aset (cadar nskk-mode-display-alist) 0 ?B)
        (setf (nth 2 (car nskk-mode-display-alist)) 'italic
              (nth 3 (car nskk-mode-display-alist)) "Second")
        (let ((actual (nskk-modeline-indicator)))
          (should (equal (substring-no-properties actual) " BA"))
          (should (eq (get-text-property 0 'face actual) 'italic))
          (should (equal (get-text-property 0 'help-echo actual) "Second")))
        (setq nskk-mode-display-alist nil)
        (should (equal (substring-no-properties (nskk-modeline-indicator)) " NSKK")))))

  (nskk-it "shares mode metadata between cursor color and indicator text"
    (let ((nskk-mode-display-alist
           '((hiragana "あ" bold "Kana" nskk-cursor-katakana))))
      (should (equal (nskk--cursor-with-color 'hiragana)
                     (face-attribute 'nskk-cursor-katakana :background nil t)))
      (should (equal (substring-no-properties
                      (nskk--show-mode-indicator-string 'hiragana)) "[あ]"))
      (should-not (nskk-mode-display-properties 'unknown))))

  (nskk-it "refreshes the same-mode indicator when text or duration changes"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-show-mode-show t)
            (nskk-show-mode-style 'inline)
            (nskk-show-mode-duration 60)
            (nskk-mode-display-alist
             (list (list 'hiragana (copy-sequence "AA") 'bold "Kana" nil)))
            (calls nil))
        (cl-letf (((symbol-function 'nskk--show-mode-display-inline)
                   (lambda (text) (push (substring-no-properties text) calls) t)))
          (nskk-show-mode-display)
          (nskk-show-mode-display)
          (should (equal calls '("[AA]")))
          (aset (cadar nskk-mode-display-alist) 0 ?B)
          (nskk-show-mode-display)
          (should (equal calls '("[BA]" "[AA]")))
          (setq nskk-show-mode-duration 30)
          (nskk-show-mode-display)
          (should (equal calls '("[BA]" "[BA]" "[AA]")))))))

  (nskk-it "strips candidate properties before formatting and preserves output faces"
    (with-temp-buffer
      (let ((nskk-show-inline 'vertical)
            (nskk-inline-format-function
             (lambda (candidate style)
               (should (eq style 'vertical))
               (should-not (text-properties-at 0 candidate))
               (should-not (get-text-property 0 'display candidate))
               (should-not (get-text-property 0 'keymap candidate))
               (propertize (concat "<" candidate ">") 'face 'bold))))
        (unwind-protect
            (progn
              (nskk-inline-show-candidate
               (propertize "候補" 'display "bad" 'keymap '(keymap)))
              (let ((actual (overlay-get nskk--inline-overlay 'after-string)))
                (should (equal (substring-no-properties actual) "<候補>"))
                (should (eq (get-text-property 0 'face actual) 'bold)))
              (should (equal (buffer-string) "")))
          (nskk-inline-hide)))))

  (nskk-it "rejects non-string inline formatter results"
    (let ((nskk-inline-format-function (lambda (_candidate _style) nil)))
      (should-error (nskk--inline-render "候補" t) :type 'wrong-type-argument)))

  (nskk-it "keeps buffer-local display settings out of other buffers"
    (let ((nskk-mode-display-alist '((hiragana "global" bold "Global" nil))))
      (with-temp-buffer
        (setq-local nskk-mode-display-alist '((hiragana "local" italic "Local" nil)))
        (should (equal (car (nskk-mode-display-properties 'hiragana)) "local")))
      (with-temp-buffer
        (should (equal (car (nskk-mode-display-properties 'hiragana)) "global")))))

  (nskk-it "removes the active indicator and timer when mode metadata is removed"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-show-mode-show t)
            (nskk-show-mode-style 'inline)
            (nskk-show-mode-duration 60)
            (nskk-mode-display-alist '((hiragana "AA" bold "Kana" nil))))
        (unwind-protect
            (progn
              (nskk-show-mode-display)
              (let ((overlay nskk--show-mode-overlay))
                (should (eq (overlay-buffer overlay) (current-buffer)))
                (should nskk--show-mode-timer)
                (setq nskk-mode-display-alist nil)
                (nskk-show-mode-display)
                (should-not (overlay-buffer overlay))
                (should-not nskk--show-mode-overlay)
                (should-not nskk--show-mode-timer)
                (should-not nskk--show-mode-last-mode)))
          (nskk-show-mode-hide)))))

  (nskk-it "restores the original cursor when its custom color is removed"
    (dolist (settings '(nil ((hiragana "AA" bold "Kana" nil))))
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-use-color-cursor t)
              (nskk-mode-display-alist
               '((hiragana "AA" bold "Kana" nskk-cursor-hiragana)))
              (parameters (make-hash-table :test 'eq)))
          (puthash 'cursor-color "original" parameters)
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (_frame key) (gethash key parameters)))
                    ((symbol-function 'set-frame-parameter)
                     (lambda (_frame key value) (puthash key value parameters)))
                    ((symbol-function 'set-cursor-color)
                     (lambda (color) (puthash 'cursor-color color parameters)))
                    ((symbol-function 'nskk--other-nskk-buffers-active-p)
                     (lambda (&optional _frame) t)))
            (nskk-cursor-update)
            (should (equal (gethash 'cursor-color parameters)
                           (nskk--cursor-with-color 'hiragana)))
            (should (equal (gethash nskk--saved-cursor-color-parameter parameters)
                           "original"))
            (setq nskk-mode-display-alist settings)
            (nskk-cursor-update)
            (should (equal (gethash 'cursor-color parameters) "original"))
            (should-not (gethash nskk--saved-cursor-color-parameter parameters))
            (should-not (gethash nskk--last-cursor-color-parameter parameters))))))))

(provide 'nskk-display-customization-test)
;;; nskk-display-customization-test.el ends here
