;;; nskk-layer-data-test.el --- Tests for nskk-layer-data -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nskk-layer-data)
(require 'nskk-dict-struct)

(ert-deftest nskk-layer-data-test-prefix-dedup ()
  "前方一致検索で辞書の重複結果が除外されることを確認する。"
  (let* ((entry1 (nskk-dict-entry-create "かんじ"
                                         (list (nskk-dict-candidate-create "漢字"))
                                         'okuri-nasi))
         (entry2 (nskk-dict-entry-create "かんじ"
                                         (list (nskk-dict-candidate-create "感じ"))
                                         'okuri-nasi))
         (dicts '(:dict1 :dict2))
         (orig-search (symbol-function 'nskk-search)))
    (cl-letf (((symbol-function 'nskk-search)
               (lambda (index query type okuri limit)
                 (ignore query okuri limit)
                 (cond
                  ((and (eq type 'prefix) (eq index :dict1))
                   (list (cons "かんじ" entry1)))
                  ((and (eq type 'prefix) (eq index :dict2))
                   (list (cons "かんじ" entry2)))
                  (t
                   (funcall orig-search index query type okuri limit)))))
              ((symbol-value 'nskk-data--dictionaries) dicts)
              ((symbol-value 'nskk-data--cache) (make-hash-table :test 'equal))
              ((symbol-value 'nskk-data--learning-data) (make-hash-table :test 'equal)))
      (let ((results (nskk-data-prefix-search "かん" nil 'okuri-nasi)))
        (should (= (length results) 1))
        (should (eq (cdr (car results)) entry1))))))

(ert-deftest nskk-layer-data-test-exact-learning-order ()
  "完全一致検索で学習スコアが候補の並び順に反映される。"
  (let* ((entry (nskk-dict-entry-create
                 "かんじ"
                 (list (nskk-dict-candidate-create "漢字")
                       (nskk-dict-candidate-create "感じ"))
                 'okuri-nasi))
         (table (make-hash-table :test 'equal))
         (index (nskk-dict-index--create
                 :okuri-ari-table (make-hash-table :test 'equal)
                 :okuri-nasi-table (prog1 table
                                     (puthash "かんじ" entry table)))))
    (cl-letf (((symbol-value 'nskk-data--dictionaries) (list index))
              ((symbol-value 'nskk-data--cache) (make-hash-table :test 'equal))
              ((symbol-value 'nskk-data--learning-data)
                (let ((store (make-hash-table :test 'equal))
                      (scores (make-hash-table :test 'equal)))
                  (puthash "感じ" 5 scores)
                  (puthash "漢字" 1 scores)
                  (puthash "かんじ" scores store)
                  store)))
      (let* ((result (nskk-data-search "かんじ" '(:type exact)))
             (cands (nskk-dict-entry-candidates result)))
        (should (string= (nskk-dict-candidate-word (car cands)) "感じ"))
        (should (string= (nskk-dict-candidate-word (cadr cands)) "漢字"))))))

(provide 'nskk-layer-data-test)
;;; nskk-layer-data-test.el ends here
