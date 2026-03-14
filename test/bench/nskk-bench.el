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

;; Comprehensive performance benchmarks covering all hot paths in the NSKK
;; input method, organized by architecture layer:
;;
;;   L0 : Prolog engine            (nskk-prolog.el)       [L0 Foundation]
;;   L1 : Romaji converter         (nskk-converter.el)   [L2 Domain]
;;   L2a: State management         (nskk-state.el)       [L2 Domain]
;;   L2b: Kana utilities           (nskk-kana.el)        [L1 Core Engine]
;;   L2c: Cache layer              (nskk-cache.el)       [L1 Core Engine]
;;   L3 : Dictionary search        (nskk-search.el)      [L2 Domain]
;;   L4a: Input processing         (nskk-input.el)       [L5 Presentation]
;;   L4b: Henkan pipeline          (nskk-henkan.el)      [L3 Application]
;;   E2E: Keystroke simulation     (full stack)
;;
;; Note: L0-L4 labels above are bench group names (for report column width).
;; Architecture layers: L0 Foundation, L1 Core Engine, L2 Domain,
;;   L3 Application, L5 Presentation (L4 is intentionally unoccupied).
;;
;; Usage:
;;   make bench
;;
;; Output format (per benchmark):
;;   LAYER  SCENARIO                              N       ms/op    GC
;;   ----   ------------------------------------  ------  -------  --
;;   L0     prolog-holds-p (hash hit)             10000   0.001ms   0

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'nskk-test-framework)

;;;; ── Benchmark Infrastructure ────────────────────────────────────────────────

(defvar nskk-bench--results nil
  "Accumulated benchmark result plists, newest first.
Each plist has :layer :name :n :ms-per-op :gc-count.")

(defun nskk-bench--run (layer name n thunk)
  "Run THUNK N times and record the result under LAYER / NAME."
  (garbage-collect)
  (let* ((raw   (benchmark-run n (funcall thunk)))
         (total (car raw))
         (gc    (cadr raw))
         (ms-op (* 1000.0 (/ total n)))
         (plist (list :layer layer :name name :n n :ms-per-op ms-op :gc-count gc)))
    (push plist nskk-bench--results)
    plist))

(defmacro nskk-bench (layer name n &rest body)
  "Benchmark BODY (N iterations) under LAYER / NAME."
  (declare (indent 3))
  `(nskk-bench--run ,layer ,name ,n (lambda () ,@body)))

;;;; ── Reporting ────────────────────────────────────────────────────────────────

(defun nskk-bench-report ()
  "Print benchmark results as an aligned ASCII table."
  (let ((results (nreverse (copy-sequence nskk-bench--results))))
    (message "%-5s  %-44s  %6s  %10s  %3s"
             "LAYER" "SCENARIO" "N" "ms/op" "GC")
    (message "%s" (make-string 76 ?─))
    (let (last-layer)
      (dolist (r results)
        (let ((layer (plist-get r :layer)))
          (when (and last-layer (not (equal last-layer layer)))
            (message ""))
          (setq last-layer layer)
          (message "%-5s  %-44s  %6d  %10.4fms  %3d"
                   layer
                   (plist-get r :name)
                   (plist-get r :n)
                   (plist-get r :ms-per-op)
                   (plist-get r :gc-count)))))))

;;;; ── L0: Prolog Engine ────────────────────────────────────────────────────────

(defun nskk-bench-run-l0 ()
  "Benchmark L0: Prolog engine hot paths."
  ;; Hash-indexed fact lookup — the most common Prolog call site
  (nskk-bench "L0" "prolog-holds-p hash-hit (valid-mode hiragana)" 50000
    (nskk-prolog-holds-p '(valid-mode hiragana)))

  (nskk-bench "L0" "prolog-holds-p hash-miss (valid-mode bogus)" 50000
    (nskk-prolog-holds-p '(valid-mode nonexistent-mode)))

  ;; Value retrieval — used in nskk-self-insert routing
  (nskk-bench "L0" "prolog-query-value (input-route hiragana)" 50000
    (nskk-prolog-query-value '(input-route hiragana \?action) '\?action))

  ;; Henkan phase lookup — used on every Japanese keypress
  (nskk-bench "L0" "prolog-holds-p (valid-henkan-phase on)" 50000
    (nskk-prolog-holds-p '(valid-henkan-phase on)))

  (nskk-bench "L0" "prolog-holds-p (henkan-mode-phase on)" 50000
    (nskk-prolog-holds-p '(henkan-mode-phase on))))

;;;; ── L1: Romaji Converter ─────────────────────────────────────────────────────

(defun nskk-bench-run-l1 ()
  "Benchmark L1: romaji-to-kana converter hot paths."
  ;; Complete match: most common case on every keystroke
  (nskk-bench "L1" "converter-convert complete (ka→か)" 50000
    (nskk-converter-convert "ka"))

  (nskk-bench "L1" "converter-convert complete (shi→し)" 50000
    (nskk-converter-convert "shi"))

  ;; Incomplete: first char of a two-char sequence
  (nskk-bench "L1" "converter-convert incomplete (k)" 50000
    (nskk-converter-convert "k"))

  ;; No match: ASCII passthrough (digits, symbols)
  (nskk-bench "L1" "converter-convert no-match (1)" 50000
    (nskk-converter-convert "1"))

  ;; Direct table lookup without full conversion logic
  (nskk-bench "L1" "converter-lookup (ka)" 50000
    (nskk-converter-lookup "ka"))

  (nskk-bench "L1" "converter-lookup (miss: zz)" 50000
    (nskk-converter-lookup "zz"))

  ;; Possible completions: used for dynamic completion (dcomp)
  (nskk-bench "L1" "converter-get-possible-completions (k)" 10000
    (nskk-converter-get-possible-completions "k")))

;;;; ── L2a: State Management ────────────────────────────────────────────────────

(defun nskk-bench-run-l2a ()
  "Benchmark L2a: state management hot paths."
  ;; State creation — once per buffer, but also exercised heavily in tests
  (nskk-bench "L2a" "state-create" 1000
    (nskk-state-create))

  (let ((state (nskk-state-create)))
    ;; Struct predicate — hot-path guard used in every public function
    (nskk-bench "L2a" "state-p (valid struct)" 100000
      (nskk-state-p state))

    (nskk-bench "L2a" "state-p (nil input)" 100000
      (nskk-state-p nil))

    ;; Generic getter — dispatches via intern + fboundp
    (nskk-bench "L2a" "state-get (mode)" 50000
      (nskk-state-get state 'mode))

    (nskk-bench "L2a" "state-get (input-buffer)" 50000
      (nskk-state-get state 'input-buffer))

    ;; Generic setter (non-mode) — slot dispatch via cond
    (nskk-bench "L2a" "state-set (input-buffer)" 50000
      (nskk-state-set state 'input-buffer ""))

    ;; Mode validation — Prolog holds-p query
    (nskk-bench "L2a" "state-valid-mode-p (hit: hiragana)" 50000
      (nskk-state-valid-mode-p 'hiragana))

    (nskk-bench "L2a" "state-valid-mode-p (miss: bogus)" 50000
      (nskk-state-valid-mode-p 'bogus-mode))

    ;; append-input at increasing buffer lengths — exposes O(n) concat growth
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

    ;; delete-last-char — backspace hot path
    (nskk-bench "L2a" "state-delete-last-char (buf=ka)" 50000
      (let ((s (nskk-state-create)))
        (setf (nskk-state-input-buffer s) "ka")
        (nskk-state-delete-last-char s)))

    (nskk-bench "L2a" "state-delete-last-char (empty buf)" 50000
      (let ((s (nskk-state-create)))
        (nskk-state-delete-last-char s)))

    ;; Henkan phase predicates — checked on every input event in henkan mode
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

    ;; Phase transition — validated via Prolog valid-henkan-transition
    (nskk-bench "L2a" "state-set-henkan-phase (nil→on)" 10000
      (let ((s (nskk-state-create)))
        (nskk-state-set-henkan-phase s 'on)))

    (nskk-bench "L2a" "state-force-henkan-phase (bypass validation)" 20000
      (let ((s (nskk-state-create)))
        (nskk-state-force-henkan-phase s 'on)))

    ;; Mode transition
    (nskk-bench "L2a" "state-transition (hiragana→katakana)" 10000
      (let ((s (nskk-state-create 'hiragana)))
        (nskk-state-transition s 'hiragana 'katakana)))

    ;; State reset — called after kakutei to clear conversion context
    (nskk-bench "L2a" "state-reset (10 slot defaults via Prolog)" 5000
      (let ((s (nskk-state-create 'hiragana)))
        (nskk-state-reset s)))

    ;; Candidate navigation
    (let ((cs (nskk-state-create)))
      (nskk-state-set-candidates cs '("漢字" "感じ" "幹事" "漢字A" "漢字B"))
      (nskk-bench "L2a" "state-next-candidate (5 candidates)" 50000
        (nskk-state-next-candidate cs))
      (nskk-bench "L2a" "state-previous-candidate (5 candidates)" 50000
        (nskk-state-previous-candidate cs))
      (nskk-bench "L2a" "state-current-candidate (5 candidates)" 50000
        (nskk-state-current-candidate cs)))

    ;; Metadata access — used for okurigana and kana-type
    (nskk-bench "L2a" "state-get-metadata (okurigana, unset)" 50000
      (nskk-state-get-metadata state 'okurigana))

    (nskk-bench "L2a" "state-put-metadata (okurigana)" 50000
      (let ((s (nskk-state-create)))
        (nskk-state-put-metadata s 'okurigana "k")))))

;;;; ── L2b: Kana Utilities ──────────────────────────────────────────────────────

(defun nskk-bench-run-l2b ()
  "Benchmark L2b: kana conversion utilities."
  ;; Hiragana → katakana: called on every char output in katakana mode
  (nskk-bench "L2b" "kana-hiragana-to-katakana (1 char: か)" 50000
    (nskk-kana-string-hiragana-to-katakana "か"))

  (nskk-bench "L2b" "kana-hiragana-to-katakana (3 chars: かんじ)" 50000
    (nskk-kana-string-hiragana-to-katakana "かんじ"))

  (nskk-bench "L2b" "kana-hiragana-to-katakana (10 chars)" 20000
    (nskk-kana-string-hiragana-to-katakana "にほんごにゅうりょく"))

  ;; Zenkaku → hankaku: called for katakana-半角 mode output
  (nskk-bench "L2b" "kana-zenkaku-to-hankaku (1 char: カ)" 50000
    (nskk-kana-zenkaku-to-hankaku "カ"))

  (nskk-bench "L2b" "kana-zenkaku-to-hankaku (3 chars: カンジ)" 50000
    (nskk-kana-zenkaku-to-hankaku "カンジ")))

;;;; ── L2c: Cache Layer ─────────────────────────────────────────────────────────

(defun nskk-bench-run-l2c ()
  "Benchmark L2c: LRU/LFU cache hot paths."
  ;; LRU cache
  (let* ((lru (nskk-cache-lru-create 64)))
    (nskk-cache-lru-put lru "かんじ" '("漢字" "感じ"))
    (nskk-cache-lru-put lru "にほん" '("日本"))

    (nskk-bench "L2c" "cache-lru-get (hit)" 50000
      (nskk-cache-lru-get lru "かんじ"))

    (nskk-bench "L2c" "cache-lru-get (miss)" 50000
      (nskk-cache-lru-get lru "MISSING-KEY"))

    (nskk-bench "L2c" "cache-lru-put (new key)" 20000
      (nskk-cache-lru-put (nskk-cache-lru-create 64) "test" '("テスト")))

    (nskk-bench "L2c" "cache-lru-put (overwrite existing)" 20000
      (nskk-cache-lru-put lru "かんじ" '("漢字" "感じ" "幹事"))))

  ;; LFU cache
  (let* ((lfu (nskk-cache-lfu-create 64)))
    (nskk-cache-lfu-put lfu "かんじ" '("漢字" "感じ"))
    (nskk-cache-lfu-put lfu "にほん" '("日本"))

    (nskk-bench "L2c" "cache-lfu-get (hit)" 50000
      (nskk-cache-lfu-get lfu "かんじ"))

    (nskk-bench "L2c" "cache-lfu-get (miss)" 50000
      (nskk-cache-lfu-get lfu "MISSING-KEY"))

    (nskk-bench "L2c" "cache-lfu-put (new key)" 20000
      (nskk-cache-lfu-put (nskk-cache-lfu-create 64) "test" '("テスト"))))

  ;; Unified cache API — dispatches to LRU via Prolog metadata
  (let* ((cache (nskk-cache-create :type 'lru :capacity 64)))
    (nskk-cache-put cache "かんじ" '("漢字" "感じ"))

    (nskk-bench "L2c" "cache-get unified LRU (hit)" 20000
      (nskk-cache-get cache "かんじ"))

    (nskk-bench "L2c" "cache-get unified LRU (miss)" 20000
      (nskk-cache-get cache "MISSING"))))

;;;; ── L3: Dictionary Search ────────────────────────────────────────────────────

(defun nskk-bench-run-l3 ()
  "Benchmark L3: dictionary search hot paths (mock dict, 13 entries)."
  (nskk-with-mock-dict nil
    (let ((idx nskk--system-dict-index))

      ;; Exact search — the primary henkan trigger
      (nskk-bench "L3" "search-exact (hit: かんじ)" 5000
        (nskk-search-exact idx "かんじ" nil))

      (nskk-bench "L3" "search-exact (miss: xxxxxxx)" 5000
        (nskk-search-exact idx "xxxxxxx" nil))

      ;; Prefix search — dcomp and incremental completion
      (nskk-bench "L3" "search-prefix (に, limit=5)" 5000
        (nskk-search-prefix idx "に" nil 5))

      (nskk-bench "L3" "search-prefix (か, limit=5)" 5000
        (nskk-search-prefix idx "か" nil 5))

      ;; Partial (substring) search
      (nskk-bench "L3" "search-partial (か, limit=5)" 2000
        (nskk-search-partial idx "か" nil 5))

      ;; Fuzzy search (Levenshtein distance) — most CPU intensive
      (nskk-bench "L3" "search-fuzzy (かんし, limit=3)" 500
        (nskk-search-fuzzy idx "かんし" 3))

      ;; Unified dispatcher (defaults to exact search)
      (nskk-bench "L3" "search dispatcher (exact, かんじ)" 3000
        (nskk-search idx "かんじ"))

      ;; Result sorting — called after every successful search
      (let ((results '(("かんじ" "漢字" nil)
                       ("かんじ" "感じ" nil)
                       ("かんじ" "幹事" nil))))
        (nskk-bench "L3" "search-sort-results (3 entries)" 10000
          (nskk--search-sort-results results)))

      ;; Levenshtein distance alone — inner loop of fuzzy search
      (nskk-bench "L3" "levenshtein-distance (かんじ vs かんし)" 10000
        (nskk--search-levenshtein-distance "かんじ" "かんし"))

      (nskk-bench "L3" "levenshtein-distance (longer strings)" 3000
        (nskk--search-levenshtein-distance "にほんごにゅうりょく" "にほんご"))

      ;; Cache integration: cold miss vs warm hit
      (let ((cache (nskk-cache-create :type 'lru :capacity 128)))
        (nskk-bench "L3" "search-with-cache (cold miss)" 1000
          (nskk-search-with-cache cache idx "かんじ"))
        ;; Warm the cache first
        (nskk-search-with-cache cache idx "かんじ")
        (nskk-bench "L3" "search-with-cache (warm hit)" 5000
          (nskk-search-with-cache cache idx "かんじ"))))))

;;;; ── L4a: Input Processing ────────────────────────────────────────────────────

(defun nskk-bench-run-l4a ()
  "Benchmark L4a: character input processing hot paths."
  ;; Full-width char Prolog rule query — used in jisx0208-latin mode
  (nskk-bench "L4a" "fullwidth prolog-query-value (hit: A)" 100000
    (nskk-prolog-query-value `(fullwidth-char ,?A \?fw) '\?fw))

  (nskk-bench "L4a" "fullwidth prolog-query-value (SPC)" 100000
    (nskk-prolog-query-value `(fullwidth-char ,?\s \?fw) '\?fw))

  ;; Romaji input classification — called once per keypress in Japanese mode
  ;; Each case exercises a different branch of the priority dispatch.
  (let ((nskk--romaji-buffer ""))
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

  ;; compute-effective-char — called once per keypress to detect henkan-start
  (nskk-bench "L4a" "compute-effective-char (lowercase a)" 30000
    (nskk--compute-effective-char ?a))

  (nskk-bench "L4a" "compute-effective-char (uppercase A, no conv)" 30000
    (nskk--compute-effective-char ?A))

  ;; convert-input-to-kana — the central romaji accumulation loop
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-set-mode-hiragana)

    (nskk-bench "L4a" "convert-input-to-kana (a → あ, clean buf)" 20000
      (progn
        (setq nskk--romaji-buffer "")
        (nskk-convert-input-to-kana ?a)))

    (nskk-bench "L4a" "convert-input-to-kana (k, incomplete)" 20000
      (progn
        (setq nskk--romaji-buffer "")
        (nskk-convert-input-to-kana ?k)))

    (nskk-bench "L4a" "convert-input-to-kana (a after k → か)" 20000
      (progn
        (setq nskk--romaji-buffer "k")
        (nskk-convert-input-to-kana ?a)))

    (nskk-bench "L4a" "convert-input-to-kana (nn → ん)" 20000
      (progn
        (setq nskk--romaji-buffer "n")
        (nskk-convert-input-to-kana ?n)))

    (nskk-bench "L4a" "convert-input-to-kana (kk → っ, sokuon)" 20000
      (progn
        (setq nskk--romaji-buffer "k")
        (nskk-convert-input-to-kana ?k)))

    (nskk-mode -1)))

;;;; ── L4b: Henkan Pipeline ─────────────────────────────────────────────────────

(defun nskk-bench-run-l4b ()
  "Benchmark L4b: henkan conversion pipeline."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-set-mode-hiragana)

    ;; converting-p — checked many times per input event as an early guard
    (nskk-bench "L4b" "converting-p (not converting, nil phase)" 50000
      (nskk-converting-p))

    ;; preedit-string — called for overlay refresh on every char
    (nskk-bench "L4b" "preedit-string (empty, no marker)" 20000
      (nskk-preedit-string))

    ;; core-search — the henkan execution path; combines user+system dict
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

    ;; Typing "a" → inserts あ (single vowel, clean romaji buffer)
    (nskk-bench "E2E" "type 'a' hiragana (→ あ)" 10000
      (progn
        (erase-buffer)
        (setq nskk--romaji-buffer "")
        (nskk-process-japanese-input ?a 1)))

    ;; Typing "ka" → inserts か (consonant+vowel pair)
    (nskk-bench "E2E" "type 'ka' hiragana (→ か)" 5000
      (progn
        (erase-buffer)
        (setq nskk--romaji-buffer "")
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?a 1)))

    ;; Typing "shi" → inserts し (3-char romaji)
    (nskk-bench "E2E" "type 'shi' hiragana (→ し)" 5000
      (progn
        (erase-buffer)
        (setq nskk--romaji-buffer "")
        (nskk-process-japanese-input ?s 1)
        (nskk-process-japanese-input ?h 1)
        (nskk-process-japanese-input ?i 1)))

    ;; Typing "nihongo" → inserts にほんご (7 chars, includes n+consonant ん)
    (nskk-bench "E2E" "type 'nihongo' hiragana (→ にほんご)" 1000
      (progn
        (erase-buffer)
        (setq nskk--romaji-buffer "")
        (dolist (ch (string-to-list "nihongo"))
          (nskk-process-japanese-input ch 1))))

    ;; Repeated sokuon: "kka" → っか
    (nskk-bench "E2E" "type 'kka' hiragana (→ っか, sokuon)" 5000
      (progn
        (erase-buffer)
        (setq nskk--romaji-buffer "")
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?k 1)
        (nskk-process-japanese-input ?a 1)))

    (nskk-mode -1)))

;;;; ── Runner ───────────────────────────────────────────────────────────────────

(defun nskk-run-all-benchmarks ()
  "Run all NSKK benchmarks and print a summary table."
  (setq nskk-bench--results nil)
  (message "")
  (message "══ NSKK Benchmarks ══════════════════════════════════════════════════════")
  (message "  Emacs %s  |  %s" emacs-version (format-time-string "%Y-%m-%d %H:%M:%S"))
  (message "")

  (message "▶ L0: Prolog Engine")
  (nskk-bench-run-l0)

  (message "▶ L1: Romaji Converter")
  (nskk-bench-run-l1)

  (message "▶ L2a: State Management")
  (nskk-bench-run-l2a)

  (message "▶ L2b: Kana Utilities")
  (nskk-bench-run-l2b)

  (message "▶ L2c: Cache Layer")
  (nskk-bench-run-l2c)

  (message "▶ L3: Dictionary Search")
  (nskk-bench-run-l3)

  (message "▶ L4a: Input Processing")
  (nskk-bench-run-l4a)

  (message "▶ L4b: Henkan Pipeline")
  (nskk-bench-run-l4b)

  (message "▶ E2E: Keystroke Simulation")
  (nskk-bench-run-e2e)

  (message "")
  (message "══ Results ══════════════════════════════════════════════════════════════")
  (message "")
  (nskk-bench-report)
  (message ""))

;; Auto-run when loaded in batch mode (make bench)
(when noninteractive
  (nskk-run-all-benchmarks))

(provide 'nskk-bench)

;;; nskk-bench.el ends here
