;;; nskk-layer-infrastructure-test.el --- Tests for nskk-layer-infrastructure -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nskk-layer-infrastructure)

(ert-deftest nskk-layer-infrastructure-test-read-file-error ()
  "読み込み不能なパスでエラーが発生することを確認。"
  (let ((unreadable (expand-file-name (format "nskk-no-file-%s" (make-temp-name ""))
                                      temporary-file-directory)))
    (should-error (nskk-infrastructure-read-file unreadable) :type 'file-error)))

(ert-deftest nskk-layer-infrastructure-test-read-file-async-error ()
  "非同期読み込みでもエラーハンドラが呼ばれることを確認。"
  (let ((unreadable (expand-file-name (format "nskk-no-file-%s" (make-temp-name ""))
                                      temporary-file-directory))
        (captured nil))
    (nskk-infrastructure-read-file-async
     unreadable
     (lambda (_content)
       (setq captured 'unexpected-success))
     (lambda (err)
       (setq captured err)))
    (should (and captured (not (eq captured 'unexpected-success))))
    (should (eq (car captured) 'file-error))))

(ert-deftest nskk-layer-infrastructure-test-write-file-stats ()
  "書き込み後に統計情報が更新される。"
  (let ((test-stats (make-hash-table :test 'eq))
        (temp (make-temp-file "nskk-infra")))
    (unwind-protect
        (cl-letf (((symbol-value 'nskk-infrastructure--statistics) test-stats))
          (nskk-infrastructure--initialize-statistics)
          (nskk-infrastructure-write-file temp "data")
          (should (= (gethash :files-written test-stats) 1)))
      (when (file-exists-p temp)
        (delete-file temp)))))

(provide 'nskk-layer-infrastructure-test)
;;; nskk-layer-infrastructure-test.el ends here
