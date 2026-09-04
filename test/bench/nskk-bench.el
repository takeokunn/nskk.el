;;; nskk-bench.el --- NSKK comprehensive performance benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: i18n

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; NSKK comprehensive performance benchmarks.

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'nskk-test-framework)

(declare-function nskk-mode "nskk")

;;;; ── Benchmark Infrastructure ────────────────────────────────────────────────

(defvar nskk-bench--results nil "Accumulated benchmark result plists, newest first.\nEach plist stores the sample count, median and range in milliseconds per\noperation, and the total garbage collections across samples.")

(defun nskk-bench--run (layer name n thunk)
  "Run THUNK N times per sample and record LAYER / NAME statistics.
The sample count defaults to three and can be overridden with the positive
integer environment variable NSKK_BENCH_SAMPLES."
  (let* ((configured (getenv "NSKK_BENCH_SAMPLES"))
         (samples (if (and configured
                           (string-match-p "^[1-9][0-9]*$" configured))
                      (string-to-number configured)
                    3))
         (warmup-n (min 1000 (max 1 (/ n 100))))
         timings
         gc-counts)
    (dotimes (_ warmup-n)
      (funcall thunk))
    (dotimes (_ samples)
      (garbage-collect)
      (let ((raw (benchmark-run n (funcall thunk))))
        (push (* 1000.0 (/ (car raw) n)) timings)
        (push (cadr raw) gc-counts)))
    (setq timings (sort timings (function <)))
    (let* ((middle (/ samples 2))
           (median (if (cl-oddp samples)
                       (nth middle timings)
                     (/ (+ (nth (1- middle) timings)
                           (nth middle timings))
                        2.0)))
           (plist (list :layer layer
                        :name name
                        :n n
                        :samples samples
                        :ms-per-op median
                        :min-ms-per-op (car timings)
                        :max-ms-per-op (car (last timings))
                        :gc-count (apply (function +) gc-counts))))
      (push plist nskk-bench--results)
      plist)))

(defmacro nskk-bench (layer name n &rest body)
  "Benchmark BODY (N iterations) under LAYER / NAME."
  (declare (indent 3))
  `(nskk-bench--run ,layer ,name ,n (lambda () ,@body)))

;;;; ── Reporting ────────────────────────────────────────────────────────────────

(defun nskk-bench-report ()
  "Print benchmark results as an aligned ASCII table."
  (let ((results (nreverse (copy-sequence nskk-bench--results))))
    (message "%-5s  %-40s  %6s  %3s  %10s  %21s  %3s"
             "LAYER" "SCENARIO" "N" "S" "median" "min..max ms/op" "GC")
    (message "%s" (make-string 101 ?-))
    (let (last-layer)
      (dolist (r results)
        (let ((layer (plist-get r :layer)))
          (when (and last-layer (not (equal last-layer layer)))
            (message ""))
          (setq last-layer layer)
          (message "%-5s  %-40s  %6d  %3d  %9.4fms  %9.4f..%-9.4f  %3d"
                   layer
                   (plist-get r :name)
                   (plist-get r :n)
                   (plist-get r :samples)
                   (plist-get r :ms-per-op)
                   (plist-get r :min-ms-per-op)
                   (plist-get r :max-ms-per-op)
                   (plist-get r :gc-count)))))))

;;;; ── L0: Prolog Engine ────────────────────────────────────────────────────────

(defun nskk-bench-run-l0 ()
  "Benchmark L0: Prolog engine hot paths."
  (nskk-bench "L0" "prolog-holds-p hash-hit (valid-mode hiragana)" 50000
    (nskk-prolog-holds-p '(valid-mode hiragana)))

  (nskk-bench "L0" "prolog-holds-p hash-miss (valid-mode bogus)" 50000
    (nskk-prolog-holds-p '(valid-mode nonexistent-mode)))

  (nskk-bench "L0" "prolog-query-value (input-route hiragana)" 50000
    (nskk-prolog-query-value '(input-route hiragana \?action) '\?action))

  (nskk-bench "L0" "prolog-holds-p (valid-henkan-phase on)" 50000
    (nskk-prolog-holds-p '(valid-henkan-phase on)))

  (nskk-bench "L0" "prolog-holds-p (henkan-mode-phase on)" 50000
    (nskk-prolog-holds-p '(henkan-mode-phase on))))

;;;; ── L1: Romaji Converter ─────────────────────────────────────────────────────

(defun nskk-bench-run-l1 ()
  "Benchmark L1: romaji-to-kana converter hot paths."
  (nskk-bench "L1" "converter-convert complete (ka→か)" 50000
    (nskk-converter-convert "ka"))

  (nskk-bench "L1" "converter-convert complete (shi→し)" 50000
    (nskk-converter-convert "shi"))

  (nskk-bench "L1" "converter-convert incomplete (k)" 50000
    (nskk-converter-convert "k"))

  (nskk-bench "L1" "converter-convert no-match (1)" 50000
    (nskk-converter-convert "1"))

  (nskk-bench "L1" "converter-lookup (ka)" 50000
    (nskk-converter-lookup "ka"))

  (nskk-bench "L1" "converter-lookup (miss: zz)" 50000
    (nskk-converter-lookup "zz"))

  (nskk-bench "L1" "converter-get-possible-completions (k)" 10000
    (nskk-converter-get-possible-completions "k")))

;;;; ── L2a: State Management ────────────────────────────────────────────────────

(defun nskk-bench-run-l2a ()
  "Benchmark L2a: state management hot paths."
  (nskk-bench "L2a" "state-create" 1000
    (nskk-state-create))

  (let ((state (nskk-state-create)))
    (nskk-bench "L2a" "state-p (valid struct)" 100000
      (nskk-state-p state))

    (nskk-bench "L2a" "state-p (nil input)" 100000
      (nskk-state-p nil))

    (nskk-bench "L2a" "state-get (mode)" 50000
      (nskk-state-get state 'mode))

    (nskk-bench "L2a" "state-get (input-buffer)" 50000
      (nskk-state-get state 'input-buffer))

    (nskk-bench "L2a" "state-set (input-buffer)" 50000
      (nskk-state-set state 'input-buffer ""))

    (nskk-bench "L2a" "state-valid-mode-p (hit: hiragana)" 50000
      (nskk-state-valid-mode-p 'hiragana))

    (nskk-bench "L2a" "state-valid-mode-p (miss: bogus)" 50000
      (nskk-state-valid-mode-p 'bogus-mode))

    (nskk-bench "L2a" "state-append-input (buf-len=0)" 50000
      (let ((s (nskk-state-create)))
        (nskk-state-append-input s ?a)))

    (nskk-bench "L2a" "state-append-input (buf-len=5)" 50000
      (let ((s (nskk-state-create)))
        (setf (nskk-state-input-buffer s) "hello")
        (nskk-state-append-input s ?a)))

    (nskk-bench "L2a" "state-append-input (buf-len=20)" 50000
      (let ((s (nskk-state-create)))
        (setf (nskk-state-input-buffer s) (make-string 20 ?a))
        (nskk-state-append-input s ?a)))

    (nskk-bench "L2a" "state-append-input (buf-len=50)" 20000
      (let ((s (nskk-state-create)))
        (setf (nskk-state-input-buffer s) (make-string 50 ?a))
        (nskk-state-append-input s ?a)))

    (nskk-bench "L2a" "state-delete-last-char (buf=ka)" 50000
      (let ((s (nskk-state-create)))
        (setf (nskk-state-input-buffer s) "ka")
        (nskk-state-delete-last-char s)))

    (nskk-bench "L2a" "state-delete-last-char (empty buf)" 50000
      (let ((s (nskk-state-create)))
        (nskk-state-delete-last-char s)))

    (nskk-bench "L2a" "state-henkan-on-p (nil phase)" 50000
      (nskk-state-henkan-on-p state))

    (nskk-bench "L2a" "state-henkan-active-p (nil phase)" 50000
      (nskk-state-henkan-active-p state))

    (let ((on-state (nskk-state-create)))
      (nskk-state-force-henkan-phase on-state 'on)
      (nskk-bench "L2a" "state-henkan-on-p (on phase)" 50000
        (nskk-state-henkan-on-p on-state))
      (nskk-bench "L2a" "state-in-henkan-mode-p (on phase)" 50000
        (nskk-state-in-henkan-mode-p on-state)))

    (nskk-bench "L2a" "state-set-henkan-phase (nil→on)" 10000
      (let ((s (nskk-state-create)))
        (nskk-state-set-henkan-phase s 'on)))

    (nskk-bench "L2a" "state-force-henkan-phase (bypass validation)" 20000
      (let ((s (nskk-state-create)))
        (nskk-state-force-henkan-phase s 'on)))

    (nskk-bench "L2a" "state-transition (hiragana→katakana)" 10000
      (let ((s (nskk-state-create 'hiragana)))
        (nskk-state-transition s 'hiragana 'katakana)))

    (nskk-bench "L2a" "state-reset (10 slot defaults via Prolog)" 5000
      (let ((s (nskk-state-create 'hiragana)))
        (nskk-state-reset s)))

    (let ((cs (nskk-state-create)))
      (nskk-state-set-candidates cs '("漢字" "感じ" "幹事" "漢字A" "漢字B"))
      (nskk-bench "L2a" "state-next-candidate (5 candidates)" 50000
        (nskk-state-next-candidate cs))
      (nskk-bench "L2a" "state-previous-candidate (5 candidates)" 50000
        (nskk-state-previous-candidate cs))
      (nskk-bench "L2a" "state-current-candidate (5 candidates)" 50000
        (nskk-state-current-candidate cs)))

    (nskk-bench "L2a" "state-get-metadata (okurigana, unset)" 50000
      (nskk-state-get-metadata state 'okurigana))

    (nskk-bench "L2a" "state-put-metadata (okurigana)" 50000
      (let ((s (nskk-state-create)))
        (nskk-state-put-metadata s 'okurigana "k")))))

;;;; ── L2b: Kana Utilities ──────────────────────────────────────────────────────

(defun nskk-bench-run-l2b ()
  "Benchmark L2b: kana conversion utilities."
  (nskk-bench "L2b" "kana-hiragana-to-katakana (1 char: か)" 50000
    (nskk-kana-string-hiragana-to-katakana "か"))

  (nskk-bench "L2b" "kana-hiragana-to-katakana (3 chars: かんじ)" 50000
    (nskk-kana-string-hiragana-to-katakana "かんじ"))

  (nskk-bench "L2b" "kana-hiragana-to-katakana (10 chars)" 20000
    (nskk-kana-string-hiragana-to-katakana "にほんごにゅうりょく"))

  (nskk-bench "L2b" "kana-zenkaku-to-hankaku (1 char: カ)" 50000
    (nskk-kana-zenkaku-to-hankaku "カ"))

  (nskk-bench "L2b" "kana-zenkaku-to-hankaku (3 chars: カンジ)" 50000
    (nskk-kana-zenkaku-to-hankaku "カンジ")))

;;;; ── L2c: Cache Layer ─────────────────────────────────────────────────────────

(defun nskk-bench-run-l2c ()
  "Benchmark L2c: LRU/LFU cache hot paths."
  (let* ((lru (nskk-cache-lru-create 64)))
    (nskk-cache-lru-put lru "かんじ" '("漢字" "感じ"))
    (nskk-cache-lru-put lru "にほん" '("日本"))

    (nskk-bench "L2c" "cache-lru-get (hit)" 50000
      (nskk-cache-lru-get lru "かんじ"))

    (nskk-bench "L2c" "cache-lru-get (miss)" 50000
      (nskk-cache-lru-get lru "MISSING-KEY"))

    (nskk-bench "L2c" "cache-lru-put (new key)" 20000
      (nskk-cache-lru-put (nskk-cache-lru-create 64) "test" '("テスト")))

    (let* ((varying (nskk-cache-lru-create 1000))
           (nkeys 500)
           (keys (make-vector nkeys nil))
           (cursor 0))
      (dotimes (index nkeys)
        (aset keys index (format "vkey%d" index))
        (nskk-cache-lru-put varying (aref keys index) index))
      (nskk-bench "L2c" "cache-lru-get (hit, 500 distinct keys, splices every call)" 50000
        (setq cursor (mod (1+ cursor) nkeys))
        (nskk-cache-lru-get varying (aref keys cursor))))

    (nskk-bench "L2c" "cache-lru-put (overwrite existing)" 20000
      (nskk-cache-lru-put lru "かんじ" '("漢字" "感じ" "幹事")))

    (let ((full (nskk-cache-lru-create 64))
          (counter 0))
      (dotimes (index 64)
        (nskk-cache-lru-put full (format "key%d" index) index))
      (nskk-bench "L2c" "cache-lru-put (new key, evicts tail)" 20000
        (setq counter (1+ counter))
        (nskk-cache-lru-put full (format "evict-%d" counter) counter))))

  (let* ((lfu (nskk-cache-lfu-create 64)))
    (nskk-cache-lfu-put lfu "かんじ" '("漢字" "感じ"))
    (nskk-cache-lfu-put lfu "にほん" '("日本"))

    (nskk-bench "L2c" "cache-lfu-get (hit)" 50000
      (nskk-cache-lfu-get lfu "かんじ"))

    (nskk-bench "L2c" "cache-lfu-get (miss)" 50000
      (nskk-cache-lfu-get lfu "MISSING-KEY"))

    (nskk-bench "L2c" "cache-lfu-put (new key)" 20000
      (nskk-cache-lfu-put (nskk-cache-lfu-create 64) "test" '("テスト")))

    (nskk-bench "L2c" "cache-lfu-put (overwrite existing)" 20000
      (nskk-cache-lfu-put lfu "かんじ" '("漢字" "感じ" "幹事")))

    (let ((full (nskk-cache-lfu-create 64))
          (counter 0))
      (dotimes (index 64)
        (nskk-cache-lfu-put full (format "key%d" index) index))
      (nskk-bench "L2c" "cache-lfu-put (new key, evicts min-freq)" 20000
        (setq counter (1+ counter))
        (nskk-cache-lfu-put full (format "evict-%d" counter) counter))))

  (let* ((cache (nskk-cache-create :type 'lru :capacity 64)))
    (nskk-cache-put cache "かんじ" '("漢字" "感じ"))

    (nskk-bench "L2c" "cache-get unified LRU (hit)" 20000
      (nskk-cache-get cache "かんじ"))

    (nskk-bench "L2c" "cache-get unified LRU (miss)" 20000
      (nskk-cache-get cache "MISSING"))

    (nskk-bench "L2c" "cache-put unified LRU (overwrite existing)" 20000
      (nskk-cache-put cache "かんじ" '("漢字" "感じ" "幹事")))))

;;;; ── L3: Dictionary Search ────────────────────────────────────────────────────

(defun nskk-bench-run-l3 ()
  "Benchmark L3: dictionary search hot paths (mock dict, 13 entries)."
  (nskk-with-mock-dict nil
    (let ((idx (nskk-dict-system-index)))

      (nskk-bench "L3" "search-exact (hit: かんじ)" 5000
        (nskk-search-exact idx "かんじ" nil))

      (nskk-bench "L3" "search-exact (miss: xxxxxxx)" 5000
        (nskk-search-exact idx "xxxxxxx" nil))

      (nskk-bench "L3" "search-prefix (に, limit=5)" 5000
        (nskk-search-prefix idx "に" nil 5))

      (nskk-bench "L3" "search-prefix (か, limit=5)" 5000
        (nskk-search-prefix idx "か" nil 5))

      (nskk-bench "L3" "search-partial (か, limit=5)" 2000
        (nskk-search-partial idx "か" nil 5))

      (nskk-bench "L3" "search-fuzzy (かんし, limit=3)" 500
        (nskk-search-fuzzy idx "かんし" 3))

      (nskk-bench "L3" "search dispatcher (exact, かんじ)" 3000
        (nskk-search idx "かんじ"))

      (progn
        (let ((results '(("かんじ" "漢字" nil)
                         ("かんじ" "感じ" nil)
                         ("かんじ" "幹事" nil))))
          (nskk-bench "L3" "search-sort-results (3 entries)" 10000
            (nskk--search-sort-results results)))
        (let ((large-results
               (cl-loop for index below 10000
                        for key = (format "%05d" (- 10000 index))
                        collect (cons key nil)))
              (nskk-search-sort-method 'kana))
          (nskk-bench "L3" "search post-process top-k (10000, limit=10)" 100
            (nskk--search-post-process-results large-results nil 10))))

      (nskk-bench "L3" "levenshtein-distance (かんじ vs かんし)" 10000
        (nskk--search-levenshtein-distance "かんじ" "かんし"))

      (let ((long-source (make-string 256 ?a))
            (short-source (make-string 32 ?a))
            (near-source (concat (make-string 255 ?a) "b")))
        (nskk-bench "L3" "levenshtein-distance (longer strings)" 3000
          (nskk--search-levenshtein-distance
           "にほんごにゅうりょく" "にほんご"))
        (nskk-bench "L3" "levenshtein bounded length-reject (large mismatch)" 3000
          (nskk--search-levenshtein-distance-bounded
           long-source short-source 2))
        (nskk-bench "L3" "levenshtein exact (large mismatch baseline)" 3000
          (nskk--search-levenshtein-distance long-source short-source))
        (nskk-bench "L3" "levenshtein bounded near-match (same length)" 3000
          (nskk--search-levenshtein-distance-bounded
           long-source near-source 2))
        (nskk-bench "L3" "levenshtein exact near-match baseline" 3000
          (nskk--search-levenshtein-distance long-source near-source)))

      (let ((cache (nskk-cache-create :type 'lru :capacity 128)))
        (nskk-bench "L3" "search-with-cache (cold miss, fresh cache)" 1000
          (nskk-search-with-cache
           (nskk-cache-create :type 'lru :capacity 128) idx "かんじ"))
        (nskk-search-with-cache cache idx "かんじ")
        (nskk-bench "L3" "search-with-cache (warm hit)" 5000
          (nskk-search-with-cache cache idx "かんじ"))
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-retract-all 'learning-score 3)
          (nskk-prolog-set-index 'learning-score 3 :hash)
          (dotimes (index 10000)
            (nskk-prolog-assert
             (list (list 'learning-score
                         (format "query-%05d" index)
                         "candidate"
                         1))))
          (nskk-bench "L3" "search-learn transaction (10000 facts)" 10
            (nskk-search-learn "query-05000" "candidate")))))))

;;;; ── L4a: Input Processing ────────────────────────────────────────────────────

(defun nskk-bench-run-l4a ()
  "Benchmark L4a: character input processing hot paths."
  (nskk-bench "L4a" "fullwidth prolog-query-value (hit: A)" 100000
    (nskk-prolog-query-value `(fullwidth-char ,?A \?fw) '\?fw))

  (nskk-bench "L4a" "fullwidth prolog-query-value (SPC)" 100000
    (nskk-prolog-query-value `(fullwidth-char ,?\s \?fw) '\?fw))

  (let ((nskk-bench--saved-romaji-buffer (nskk-state-romaji-buffer)))
    (nskk-state-set-romaji-buffer "")
    (unwind-protect
        (progn
          (nskk-bench "L4a" "classify-romaji-input (match: result of a)" 30000
            (nskk--classify-romaji-input ?a nil (nskk-converter-convert "a")))

          (nskk-bench "L4a" "classify-romaji-input (incomplete: k)" 30000
            (nskk--classify-romaji-input ?k nil (nskk-converter-convert "k")))

          (nskk-bench "L4a" "classify-romaji-input (sokuon: kk)" 30000
            (nskk--classify-romaji-input ?k ?k (nskk-converter-convert "kk")))

          (nskk-bench "L4a" "classify-romaji-input (nn-double)" 30000
            (nskk--classify-romaji-input ?n ?n (nskk-converter-convert "nn")))

          (nskk-bench "L4a" "classify-romaji-input (n+consonant: nm)" 30000
            (nskk--classify-romaji-input ?m ?n (nskk-converter-convert "nm"))))
      (nskk-state-set-romaji-buffer nskk-bench--saved-romaji-buffer)))

  (nskk-bench "L4a" "compute-effective-char (lowercase a)" 30000
    (nskk--compute-effective-char ?a))

  (nskk-bench "L4a" "compute-effective-char (uppercase A, no conv)" 30000
    (nskk--compute-effective-char ?A))

  (with-temp-buffer
    (nskk-mode 1)
    (nskk-set-mode-hiragana)

    (nskk-bench "L4a" "convert-input-to-kana (a → あ, clean buf)" 20000
      (progn
        (nskk-state-set-romaji-buffer "")
        (nskk-convert-input-to-kana ?a)))

    (nskk-bench "L4a" "convert-input-to-kana (k, incomplete)" 20000
      (progn
        (nskk-state-set-romaji-buffer "")
        (nskk-convert-input-to-kana ?k)))

    (nskk-bench "L4a" "convert-input-to-kana (a after k → か)" 20000
      (progn
        (nskk-state-set-romaji-buffer "k")
        (nskk-convert-input-to-kana ?a)))

    (nskk-bench "L4a" "convert-input-to-kana (nn → ん)" 20000
      (progn
        (nskk-state-set-romaji-buffer "n")
        (nskk-convert-input-to-kana ?n)))

    (nskk-bench "L4a" "convert-input-to-kana (kk → っ, sokuon)" 20000
      (progn
        (nskk-state-set-romaji-buffer "k")
        (nskk-convert-input-to-kana ?k)))

    (nskk-mode -1)))

;;;; ── L4b: Henkan Pipeline ─────────────────────────────────────────────────────

(defun nskk-bench-run-l4b ()
  "Benchmark L4b: henkan conversion pipeline."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-set-mode-hiragana)

    (nskk-bench "L4b" "converting-p (not converting, nil phase)" 50000
      (nskk-converting-p))

    (nskk-bench "L4b" "preedit-string (empty, no marker)" 20000
      (nskk-preedit-string))

    (nskk-with-mock-dict nil
      (nskk-bench "L4b" "core-search (exact, かんじ)" 1000
        (nskk-core-search "かんじ"))

      (nskk-bench "L4b" "core-search (miss, xxxxxxx)" 1000
        (nskk-core-search "xxxxxxx")))

    (nskk-mode -1)))

;;;; ── E2E: Keystroke Simulation ───────────────────────────────────────────────

(defun nskk-bench-run-e2e ()
  "Benchmark complete keystroke sequences through the full stack."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-set-mode-hiragana)

    (nskk-bench "E2E" "type 'a' hiragana (→ あ)" 10000
      (progn
        (erase-buffer)
        (nskk-state-set-romaji-buffer "")
        (nskk-process-japanese-input ?a 1)))

    (nskk-bench "E2E" "type 'ka' hiragana (→ か)" 5000
      (progn
        (erase-buffer)
        (nskk-state-set-romaji-buffer "")
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?a 1)))

    (nskk-bench "E2E" "type 'shi' hiragana (→ し)" 5000
      (progn
        (erase-buffer)
        (nskk-state-set-romaji-buffer "")
        (nskk-process-japanese-input ?s 1)
        (nskk-process-japanese-input ?h 1)
        (nskk-process-japanese-input ?i 1)))

    (nskk-bench "E2E" "type 'nihongo' hiragana (→ にほんご)" 1000
      (progn
        (erase-buffer)
        (nskk-state-set-romaji-buffer "")
        (dolist (ch (string-to-list "nihongo"))
          (nskk-process-japanese-input ch 1))))

    (nskk-bench "E2E" "type 'kka' hiragana (→ っか, sokuon)" 5000
      (progn
        (erase-buffer)
        (nskk-state-set-romaji-buffer "")
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?a 1)))

    (nskk-mode -1)))

;;;; ── Runner ───────────────────────────────────────────────────────────────────

(defun nskk-run-all-benchmarks ()
  "Run all NSKK benchmarks and print a summary table."
  (setq nskk-bench--results nil)
  (message "")
  (message "== NSKK Benchmarks =====================================================")
  (message "  Emacs: %s" emacs-version)
  (message "  System: %s (%s), %s processors"
           system-configuration system-type
           (if (fboundp (quote num-processors)) (num-processors) "unknown"))
  (message "  GC threshold: %s bytes" gc-cons-threshold)
  (message "  Samples: %s (override with NSKK_BENCH_SAMPLES)"
           (let ((configured (getenv "NSKK_BENCH_SAMPLES"))) (if (and configured (string-match-p "^[1-9][0-9]*$" configured)) (string-to-number configured) 3)))
  (message "  Started: %s" (format-time-string "%Y-%m-%d %H:%M:%S %z"))
  (message "")
  (message "> L0: Prolog Engine")
  (nskk-bench-run-l0)
  (message "> L1: Romaji Converter")
  (nskk-bench-run-l1)
  (message "> L2a: State Management")
  (nskk-bench-run-l2a)
  (message "> L2b: Kana Utilities")
  (nskk-bench-run-l2b)
  (message "> L2c: Cache Layer")
  (nskk-bench-run-l2c)
  (message "> L3: Dictionary Search")
  (nskk-bench-run-l3)
  (message "> L4a: Input Processing")
  (nskk-bench-run-l4a)
  (message "> L4b: Henkan Pipeline")
  (nskk-bench-run-l4b)
  (message "> E2E: Keystroke Simulation")
  (nskk-bench-run-e2e)
  (message "")
  (message "== Results =============================================================")
  (message "")
  (nskk-bench-report)
  (message ""))

(when noninteractive
  (nskk-run-all-benchmarks))

(provide 'nskk-bench)

;;; nskk-bench.el ends here
