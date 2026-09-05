;;; nskk-search-cache-integration-test.el --- Search+learning-persistence integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The search-result cache subsystem this file used to exercise
;; (`nskk-search-with-cache' and its `nskk--search-cache-*' glue in
;; nskk-search.el) has been deleted; there is no longer a cache sitting
;; between search and the dictionary.  Generic `nskk-cache.el' LRU/LFU
;; behaviour (put/get/eviction/stats/invalidate) is unit-tested on its own
;; in nskk-cache-test.el and does not belong here.
;;
;; What remains is the genuinely cross-module territory no unit test
;; owns: learning scores recorded via `nskk-search-learn' reordering
;; `nskk-search-prefix' results, and that ordering surviving a
;; `nskk-search-save-learning-data' / `nskk-search-load-learning-data'
;; round trip through the filesystem.

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-test-framework)
(require 'nskk-test-macros)


;;;
;;; Search Learning
;;;

(nskk-describe "search learning"
  (nskk-it "score affects result ordering"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-with-mock-dict '(("かんじ" "漢字")
                             ("かんたん" "簡単"))
        (let* ((nskk-search-sort-method 'frequency)
               (before
                (nskk-search-prefix
                 (nskk-dict-system-index) "かん" nil nil))
               (target (car (last before)))
               (reading (car target))
               (candidate
                (car (nskk-dict-entry-candidates (cdr target)))))
          (nskk-given
            (should (= (length before) 2))
            (should-not (equal reading (caar before))))
          (nskk-when
            (nskk-search-learn reading candidate))
          (nskk-then
            (let ((after
                   (nskk-search-prefix
                    (nskk-dict-system-index) "かん" nil nil)))
              (should (equal reading (caar after)))))))))

  (nskk-it "learned ordering survives a save/load round trip through the filesystem"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-with-mock-dict '(("かんじ" "漢字")
                             ("かんたん" "簡単"))
        (let* ((nskk-search-sort-method 'frequency)
               (nskk-search-learning-file
                (make-temp-file "nskk-search-cache-it" nil ".dat"))
               (before
                (nskk-search-prefix
                 (nskk-dict-system-index) "かん" nil nil))
               (target (car (last before)))
               (reading (car target))
               (candidate
                (car (nskk-dict-entry-candidates (cdr target)))))
          (unwind-protect
              (progn
                (nskk-given
                  (should (= (length before) 2))
                  (should-not (equal reading (caar before))))
                (nskk-when
                  (nskk-search-learn reading candidate)
                  (nskk-search-save-learning-data)
                  (nskk-prolog-retract-all 'learning-score 3))
                (nskk-then
                  ;; Retracting the in-memory facts undoes the reorder,
                  ;; proving the assertion below observes the reloaded
                  ;; file rather than a fact that was never cleared.
                  (should-not
                   (equal reading
                          (caar (nskk-search-prefix
                                 (nskk-dict-system-index) "かん" nil nil)))))
                (nskk-when
                  (nskk-search-load-learning-data))
                (nskk-then
                  (let ((after
                         (nskk-search-prefix
                          (nskk-dict-system-index) "かん" nil nil)))
                    (should (equal reading (caar after))))))
            (delete-file nskk-search-learning-file)))))))


(provide 'nskk-search-cache-integration-test)

;;; nskk-search-cache-integration-test.el ends here
