;;; nskk-pbt-generators.el --- Property-Based Testing generators for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Property-Based Testing generators for NSKK.
;;
;; This file provides test data generators for property-based testing
;; (PBT) of the NSKK input method. Generators produce random but valid
;; test data that can be used to verify system properties.
;;
;; Features:
;; - Seed-based deterministic random generation
;; - Size parameter for controlling generator complexity
;; - Composable generators for building complex test data
;; - Specialized generators for Japanese input patterns
;;
;; Usage:
;;
;;   ;; Set seed for reproducible tests
;;   (nskk-pbt-set-seed 12345)
;;
;;   ;; Generate test data
;;   (nskk-generate 'key-sequence 10)     ; 10 key sequence
;;   (nskk-generate 'valid-mode)          ; Random NSKK mode
;;   (nskk-generate 'romaji-pattern 3)    ; Romaji pattern with size 3
;;
;; Generator Types:
;; - key-sequence: Random key press sequences
;; - valid-mode: Valid NSKK modes (ascii, hiragana, katakana, etc.)
;; - state-object: Random but valid nskk-state objects
;; - romaji-pattern: Extended romaji input patterns
;; - azik-rule: AZIK rule test cases
;; - okurigana-pattern: Valid consonant markers for okurigana
;; - search-query: Dictionary query patterns

;;; Code:

(require 'cl-lib)
(require 'nskk-test-framework)

;;;;
;;;; Seed-Based Random Control
;;;;

(defvar nskk-pbt--current-seed nil
  "Current random seed for deterministic generation.
When nil, uses system random.")

(defvar nskk-pbt--seed-state nil
  "Internal state for seeded random generation.")

(defun nskk-pbt-set-seed (seed)
  "Set the random SEED for deterministic generation.
SEED should be an integer. Use nil to reset to system random."
  (setq nskk-pbt--current-seed seed)
  (when seed
    ;; Initialize linear congruential generator state
    (setq nskk-pbt--seed-state (mod seed (expt 2 31)))))

(defun nskk-pbt-get-seed ()
  "Get the current random seed.
Returns nil if using system random."
  nskk-pbt--current-seed)

(defun nskk-pbt--random (&optional limit)
  "Generate a random number with optional LIMIT.
Uses seeded generation if seed is set, otherwise uses system random."
  (if nskk-pbt--current-seed
      ;; Linear congruential generator (same algorithm as many languages)
      (let ((new-state (mod (+ (* 1103515245 nskk-pbt--seed-state) 12345)
                            (expt 2 31))))
        (setq nskk-pbt--seed-state new-state)
        (if limit
            (mod new-state limit)
          new-state))
    ;; Use system random
    (if limit
        (random limit)
      (random))))

(defun nskk-pbt--random-choice (list)
  "Choose a random element from LIST."
  (when list
    (nth (nskk-pbt--random (length list)) list)))

(defun nskk-pbt--random-bool ()
  "Generate a random boolean value."
  (= (nskk-pbt--random 2) 0))

(defun nskk-pbt--random-int (min max)
  "Generate a random integer between MIN and MAX (inclusive)."
  (+ min (nskk-pbt--random (1+ (- max min)))))

(defun nskk-pbt--random-string (chars length)
  "Generate a random string from CHARS with specified LENGTH."
  (apply #'string
         (cl-loop repeat length
                  collect (nskk-pbt--random-choice chars))))

;;;;
;;;; Size Parameter Helper
;;;;

(defun nskk-pbt--resolve-size (&optional size default)
  "Resolve SIZE parameter, using DEFAULT if nil.
DEFAULT defaults to 5 if not specified."
  (or size default 5))

;;;;
;;;; Generator 1: Keypress Sequence Generator
;;;;

(defconst nskk-pbt--lowercase-letters
  (string-to-list "abcdefghijklmnopqrstuvwxyz")
  "List of lowercase letter characters.")

(defconst nskk-pbt--uppercase-letters
  (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "List of uppercase letter characters.")

(defconst nskk-pbt--mode-switch-keys
  '(("C-j" . hiragana)
    ("q" . katakana)
    ("l" . latin)
    (";" . abbrev))
  "Mode switch key sequences with their target modes.")

(cl-defun nskk-pbt--generate-key-char (&key include-special include-uppercase)
  "Generate a single key character.
When INCLUDE-SPECIAL is non-nil, may generate special keys.
When INCLUDE-UPPERCASE is non-nil, may generate uppercase letters."
  (let ((choices (append nskk-pbt--lowercase-letters
                        (when include-uppercase nskk-pbt--uppercase-letters))))
    (if (and include-special (nskk-pbt--random-bool))
        ;; Generate special key (represented as string)
        (nskk-pbt--random-choice (mapcar #'car nskk-pbt--mode-switch-keys))
      ;; Generate regular character
      (string (nskk-pbt--random-choice choices)))))

(defun nskk-pbt--generate-key-sequence (&optional length)
  "Generate a sequence of key presses with optional LENGTH.
Default length is 1-20 characters if not specified."
  (let* ((len (or length (nskk-pbt--random-int 1 20)))
         (result nil))
    (dotimes (_ len)
      (push (nskk-pbt--generate-key-char
             :include-special (nskk-pbt--random-bool)
             :include-uppercase (nskk-pbt--random-bool))
            result))
    (nreverse result)))

(nskk-register-generator 'key-sequence
  (lambda (&rest args)
    (apply #'nskk-pbt--generate-key-sequence args)))

;; Composable key sequence with length parameter
(nskk-register-generator 'key-sequence-of-length
  (lambda (length)
    (nskk-pbt--generate-key-sequence length)))

;; Key sequence with only lowercase letters
(nskk-register-generator 'lowercase-key-sequence
  (lambda (&optional length)
    (let* ((len (or length (nskk-pbt--random-int 1 20))))
      (cl-loop repeat len
               collect (string (nskk-pbt--random-choice nskk-pbt--lowercase-letters))))))

;; Key sequence with only lowercase letters (excluding mode-switch keys: q, l, ;)
(defconst nskk-pbt--typing-letters
  (cl-remove-if (lambda (c) (memq c '(?q ?l ?\;)))
                (string-to-list "abcdefghijklmnopqrstuvwxyz"))
  "List of lowercase letter characters excluding mode-switch keys.")

(nskk-register-generator 'typing-key-sequence
  (lambda (&optional length)
    (let* ((len (or length (nskk-pbt--random-int 1 20))))
      (cl-loop repeat len
               collect (string (nskk-pbt--random-choice nskk-pbt--typing-letters))))))

;; Key sequence for okurigana input (with uppercase consonant)
(nskk-register-generator 'okurigana-key-sequence
  (lambda (&optional length)
    (let* ((len (or length (nskk-pbt--random-int 2 10)))
           (result nil)
           (okurigana-pos (nskk-pbt--random-int 1 (1- len))))
      (dotimes (i len)
        (if (= i okurigana-pos)
            ;; Insert uppercase consonant for okurigana
            (push (string (nskk-pbt--random-choice nskk-pbt--uppercase-letters)) result)
          (push (string (nskk-pbt--random-choice nskk-pbt--lowercase-letters)) result)))
      (nreverse result))))

;;;;
;;;; Generator 2: Valid Mode Generator
;;;;

(defconst nskk-pbt--valid-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin)
  "List of valid NSKK modes.")

(defun nskk-pbt--generate-valid-mode ()
  "Generate a random valid NSKK mode."
  (nskk-pbt--random-choice nskk-pbt--valid-modes))

(nskk-register-generator 'valid-mode
  (lambda (&rest _args)
    (nskk-pbt--generate-valid-mode)))

;; Mode with transition context
(nskk-register-generator 'mode-transition
  (lambda (&rest _args)
    (let ((from-mode (nskk-pbt--generate-valid-mode))
          (to-mode (nskk-pbt--generate-valid-mode)))
      (cons from-mode to-mode))))

;;;;
;;;; Generator 3: State Object Generator
;;;;

(defun nskk-pbt--generate-input-buffer (&optional max-length)
  "Generate a random input buffer string.
MAX-LENGTH defaults to 20 if not specified."
  (let* ((len (nskk-pbt--random-int 0 (or max-length 20)))
         (chars (append nskk-pbt--lowercase-letters
                       (string-to-list "aeiou"))))
    (if (= len 0)
        ""
      (nskk-pbt--random-string chars len))))

(defun nskk-pbt--generate-candidates (&optional count)
  "Generate a list of random conversion candidates.
COUNT defaults to 0-10 if not specified."
  (let* ((num (or count (nskk-pbt--random-int 0 10)))
         (hiragana-chars '("あ" "い" "う" "え" "お" "か" "き" "く" "け" "こ"
                           "さ" "し" "す" "せ" "そ" "た" "ち" "つ" "て" "と"))
         (kanji-chars '("漢" "字" "変" "換" "日" "本" "語" "入" "力")))
    (cl-loop repeat num
             collect (let ((candidate-type (nskk-pbt--random 3)))
                       (cond
                        ((= candidate-type 0)
                         ;; Hiragana candidate
                         (mapconcat #'identity
                                   (cl-loop repeat (nskk-pbt--random-int 1 5)
                                            collect (nskk-pbt--random-choice hiragana-chars))
                                   ""))
                        ((= candidate-type 1)
                         ;; Kanji candidate
                         (mapconcat #'identity
                                   (cl-loop repeat (nskk-pbt--random-int 1 3)
                                            collect (nskk-pbt--random-choice kanji-chars))
                                   ""))
                        (t
                         ;; Mixed candidate
                         (concat (nskk-pbt--random-choice hiragana-chars)
                                (nskk-pbt--random-choice kanji-chars))))))))

(defun nskk-pbt--generate-state-object (&optional size)
  "Generate a random but valid nskk-state object.
SIZE controls the complexity of generated state."
  (let* ((sz (nskk-pbt--resolve-size size 5))
         (mode (nskk-pbt--generate-valid-mode))
         (input-buffer (nskk-pbt--generate-input-buffer (* sz 4)))
         (candidates (nskk-pbt--generate-candidates sz))
         (num-candidates (length candidates))
         (current-index (if (> num-candidates 0)
                            (nskk-pbt--random-int 0 (1- num-candidates))
                          0))
         (henkan-position (if (and (> num-candidates 0)
                                   (nskk-pbt--random-bool))
                              (nskk-pbt--random-int 0 (length input-buffer))
                            nil)))
    ;; Return a plist representing the state
    ;; (Cannot create actual nskk-state struct without loading nskk-state)
    (list :mode mode
          :input-buffer input-buffer
          :converted-buffer ""
          :candidates candidates
          :current-index current-index
          :henkan-position henkan-position
          :marker-position nil
          :previous-mode mode
          :undo-stack nil
          :redo-stack nil
          :metadata nil)))

(nskk-register-generator 'state-object
  (lambda (&rest args)
    (apply #'nskk-pbt--generate-state-object args)))

;; State with specific mode
(nskk-register-generator 'state-in-mode
  (lambda (mode &optional size)
    (let ((state (nskk-pbt--generate-state-object size)))
      (plist-put state :mode mode))))

;; State in henkan mode (with candidates)
(nskk-register-generator 'henkan-state
  (lambda (&optional size)
    (let* ((sz (nskk-pbt--resolve-size size 5))
           (state (nskk-pbt--generate-state-object sz)))
      (plist-put state :candidates (nskk-pbt--generate-candidates (max 1 sz)))
      (plist-put state :henkan-position 0)
      state)))

;;;;
;;;; Generator 4: Romaji Pattern Generator
;;;;

(defconst nskk-pbt--basic-vowels
  '("a" "i" "u" "e" "o")
  "Basic vowel patterns.")

(defconst nskk-pbt--consonant-vowel-basic
  '("ka" "ki" "ku" "ke" "ko"
    "sa" "shi" "su" "se" "so"
    "ta" "chi" "tsu" "te" "to"
    "na" "ni" "nu" "ne" "no"
    "ha" "hi" "fu" "he" "ho"
    "ma" "mi" "mu" "me" "mo"
    "ya" "yu" "yo"
    "ra" "ri" "ru" "re" "ro"
    "wa" "wo" "n")
  "Basic consonant+vowel combinations.")

(defconst nskk-pbt--consonant-vowel-dakuon
  '("ga" "gi" "gu" "ge" "go"
    "za" "ji" "zu" "ze" "zo"
    "da" "ji" "zu" "de" "do"
    "ba" "bi" "bu" "be" "bo"
    "pa" "pi" "pu" "pe" "po")
  "Dakuon (voiced) consonant+vowel combinations.")

(defconst nskk-pbt--special-patterns
  '("sha" "shu" "she" "sho"
    "cha" "chu" "che" "cho"
    "ja" "ju" "je" "jo"
    "nya" "nyu" "nye" "nyo"
    "hya" "hyu" "hye" "hyo"
    "mya" "myu" "mye" "myo"
    "rya" "ryu" "rye" "ryo"
    "gya" "gyu" "gye" "gyo"
    "bya" "byu" "bye" "byo"
    "pya" "pyu" "pye" "pyo"
    "kya" "kyu" "kye" "kyo"
    "fa" "fi" "fe" "fo")
  "Special romaji patterns (yōon, etc.).")

(defconst nskk-pbt--incomplete-patterns
  '("k" "ky" "g" "gy" "s" "sh" "sy"
    "z" "j" "t" "ts" "ch" "ty" "d"
    "n" "ny" "h" "hy" "f" "b" "by"
    "p" "py" "m" "my" "y" "r" "ry" "w")
  "Incomplete romaji patterns (awaiting more input).")

(defun nskk-pbt--generate-romaji-pattern (&optional size type)
  "Generate a romaji pattern with optional SIZE and TYPE.
TYPE can be 'basic, 'extended, 'incomplete, or nil for random."
  (let* ((sz (nskk-pbt--resolve-size size 3))
         (pattern-type (or type
                          (nskk-pbt--random-choice '(basic extended mixed incomplete)))))
    (cl-case pattern-type
      (basic
       ;; Basic patterns only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk-pbt--random-choice
                                    (append nskk-pbt--basic-vowels
                                            nskk-pbt--consonant-vowel-basic)))
                  ""))
      (extended
       ;; Extended patterns including special
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk-pbt--random-choice
                                    (append nskk-pbt--consonant-vowel-basic
                                            nskk-pbt--consonant-vowel-dakuon
                                            nskk-pbt--special-patterns)))
                  ""))
      (incomplete
       ;; Incomplete pattern
       (if (nskk-pbt--random-bool)
           (nskk-pbt--random-choice nskk-pbt--incomplete-patterns)
         ;; Mixed: complete syllables + incomplete ending
         (concat (mapconcat #'identity
                           (cl-loop repeat (max 0 (1- sz))
                                    collect (nskk-pbt--random-choice
                                             (append nskk-pbt--consonant-vowel-basic
                                                     nskk-pbt--special-patterns)))
                           "")
                 (nskk-pbt--random-choice nskk-pbt--incomplete-patterns))))
      (t
       ;; Mixed: all types
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk-pbt--random-choice
                                    (append nskk-pbt--basic-vowels
                                            nskk-pbt--consonant-vowel-basic
                                            nskk-pbt--consonant-vowel-dakuon
                                            nskk-pbt--special-patterns)))
                  "")))))

(nskk-register-generator 'romaji-pattern
  (lambda (&rest args)
    (apply #'nskk-pbt--generate-romaji-pattern args)))

;; Specific romaji pattern types
(nskk-register-generator 'romaji-basic
  (lambda (&optional size)
    (nskk-pbt--generate-romaji-pattern size 'basic)))

(nskk-register-generator 'romaji-extended
  (lambda (&optional size)
    (nskk-pbt--generate-romaji-pattern size 'extended)))

(nskk-register-generator 'romaji-incomplete
  (lambda (&optional size)
    (nskk-pbt--generate-romaji-pattern size 'incomplete)))

;;;;
;;;; Generator 5: AZIK Rule Generator
;;;;

;; AZIK rule categories for stratified sampling
(defconst nskk-pbt--azik-categories
  '((basic-vowels . ("a" "i" "u" "e" "o"))
    (special-keys . (";" ":"))
    (compatibility-x . ("xa" "xi" "xu" "xe" "xo"))
    (compatibility-c . ("ca" "ci" "cu" "ce" "co"))
    (hatsuon-k . ("kz" "kk" "kj" "kd" "kl"))
    (hatsuon-s . ("sz" "sk" "sj" "sd" "sl"))
    (hatsuon-t . ("tz" "tk" "tj" "td" "tl"))
    (hatsuon-n . ("nz" "nk" "nj" "nd" "nl"))
    (hatsuon-h . ("hz" "hk" "hj" "hd" "hl"))
    (hatsuon-m . ("mz" "mk" "mj" "md" "ml"))
    (hatsuon-y . ("yz" "yk" "yj" "yd" "yl"))
    (hatsuon-r . ("rz" "rk" "rj" "rd" "rl"))
    (hatsuon-w . ("wz" "wk" "wj" "wd" "wl"))
    (hatsuon-g . ("gz" "gk" "gj" "gd" "gl"))
    (hatsuon-z . ("zz" "zk" "zj" "zd" "zl"))
    (hatsuon-d . ("dz" "dk" "dj" "dd" "dl"))
    (hatsuon-b . ("bz" "bk" "bj" "bd" "bl"))
    (hatsuon-p . ("pz" "pk" "pj" "pd" "pl"))
    (diphthong-k . ("kq" "kh" "kw" "kp"))
    (diphthong-s . ("sq" "sh" "sw" "sp"))
    (diphthong-t . ("tq" "th" "tw" "tp"))
    (diphthong-n . ("nq" "nh" "nw" "np"))
    (diphthong-h . ("hq" "hh" "hw" "hp"))
    (diphthong-m . ("mq" "mh" "mw" "mp"))
    (diphthong-y . ("yq" "yh" "yw" "yp"))
    (diphthong-r . ("rq" "rh" "rw" "rp"))
    (diphthong-w . ("wq" "wh" "ww" "wp"))
    (diphthong-g . ("gq" "gh" "gw" "gp"))
    (diphthong-z . ("zq" "zh" "zw" "zp"))
    (diphthong-d . ("dq" "dh" "dw" "dp"))
    (diphthong-b . ("bq" "bh" "bw" "bp"))
    (diphthong-p . ("pq" "ph" "pw" "pp"))
    (youon-kg . ("kga" "kgu" "kge" "kgo" "kgz" "kgk" "kgj" "kgd" "kgl" "kgq" "kgh" "kgw" "kgp"))
    (youon-hg . ("hga" "hgu" "hge" "hgo" "hgz" "hgk" "hgj" "hgd" "hgl" "hgq" "hgh" "hgw" "hgp"))
    (youon-mg . ("mga" "mgu" "mge" "mgo" "mgz" "mgk" "mgj" "mgd" "mgl" "mgq" "mgh" "mgw" "mgp"))
    (youon-rg . ("rga" "rgu" "rge" "rgo" "rgz" "rgk" "rgj" "rgd" "rgl" "rgq" "rgh" "rgw" "rgp"))
    (youon-gg . ("gga" "ggu" "gge" "ggo" "ggz" "ggk" "ggj" "ggd" "ggl" "ggq" "ggh" "ggw" "ggp"))
    (youon-jg . ("jga" "jgu" "jge" "jgo" "jgz" "jgk" "jgj" "jgd" "jgl" "jgq" "jgh" "jgw" "jgp"))
    (youon-bg . ("bga" "bgu" "bge" "bgo" "bgz" "bgk" "bgj" "bgd" "bgl" "bgq" "bgh" "bgw" "bgp"))
    (youon-pg . ("pga" "pgu" "pge" "pgo" "pgz" "pgk" "pgj" "pgd" "pgl" "pgq" "pgh" "pgw" "pgp"))
    (same-finger . ("kf" "nf" "mf" "gf" "pf" "rf" "yf"))
    (special-ext . ("km" "kr" "gr" "kt" "gt" "zr" "st" "sr" "tt" "dt"
                    "tb" "tm" "tr" "ds" "dm" "nr" "nt" "nb" "ht" "bt"
                    "ms" "mt" "mn" "yr" "rr" "wt" "wr"))
    (gairaigo . ("tgi" "tgu" "dci" "dcu" "wso"))
    (standard-basic . ("ka" "ki" "ku" "ke" "ko" "ga" "gi" "gu" "ge" "go"
                       "sa" "si" "shi" "su" "se" "so" "za" "zi" "ji" "zu" "ze" "zo"
                       "ta" "ti" "chi" "tu" "tsu" "te" "to" "da" "di" "du" "de" "do"
                       "na" "ni" "nu" "ne" "no" "nn" "n'"
                       "ha" "hi" "hu" "fu" "he" "ho" "ba" "bi" "bu" "be" "bo" "pa" "pi" "pu" "pe" "po"
                       "ma" "mi" "mu" "me" "mo" "ya" "yu" "yo"
                       "ra" "ri" "ru" "re" "ro" "wa" "wo"))
    (standard-youon . ("kya" "kyu" "kye" "kyo" "gya" "gyu" "gye" "gyo"
                       "sya" "sha" "syu" "shu" "sye" "she" "syo" "sho"
                       "zya" "ja" "zyu" "ju" "zye" "je" "zyo" "jo"
                       "tya" "cha" "tyu" "chu" "tye" "che" "tyo" "cho"
                       "dya" "dyu" "dye" "dyo"
                       "nya" "nyu" "nye" "nyo"
                       "hya" "hyu" "hye" "hyo"
                       "bya" "byu" "bye" "byo"
                       "pya" "pyu" "pye" "pyo"
                       "mya" "myu" "mye" "myo"
                       "rya" "ryu" "rye" "ryo"
                       "fa" "fi" "fe" "fo"))
    (small-chars . ("la" "li" "lu" "le" "lo" "lya" "lyu" "lyo" "ltu" "ltsu" "xtu" "xtsu")))
  "AZIK rule categories with their patterns for stratified sampling.")

(defun nskk-pbt--get-all-azik-patterns ()
  "Get all AZIK patterns as a flat list."
  (apply #'append (mapcar #'cdr nskk-pbt--azik-categories)))

(defun nskk-pbt--generate-azik-rule (&optional category)
  "Generate an AZIK rule test case.
If CATEGORY is specified, sample from that category only.
Uses stratified sampling for efficiency when CATEGORY is nil."
  (if category
      ;; Sample from specific category
      (let ((cat-patterns (cdr (assoc category nskk-pbt--azik-categories))))
        (if cat-patterns
            (nskk-pbt--random-choice cat-patterns)
          (error "Unknown AZIK category: %s" category)))
    ;; Stratified sampling across all categories
    (let* ((selected-category (nskk-pbt--random-choice nskk-pbt--azik-categories))
           (patterns (cdr selected-category)))
      (nskk-pbt--random-choice patterns))))

(nskk-register-generator 'azik-rule
  (lambda (&rest args)
    (apply #'nskk-pbt--generate-azik-rule args)))

;; AZIK rule with expected output
(nskk-register-generator 'azik-rule-with-expected
  (lambda (&optional category)
    (let ((rule (nskk-pbt--generate-azik-rule category)))
      ;; Return cons cell (rule . category-name) for lookup
      (cons rule category))))

;; All AZIK patterns for exhaustive testing
(nskk-register-generator 'azik-all-patterns
  (lambda (&rest _args)
    (nskk-pbt--get-all-azik-patterns)))

;; AZIK category list
(nskk-register-generator 'azik-categories
  (lambda (&rest _args)
    (mapcar #'car nskk-pbt--azik-categories)))

;;;;
;;;; Generator 6: Okurigana Pattern Generator
;;;;

(defconst nskk-pbt--okurigana-consonants
  '("K" "S" "T" "N" "H" "M" "Y" "R" "W" "G" "Z" "D" "B" "P")
  "Valid uppercase consonant markers for okurigana.")

(defconst nskk-pbt--okurigana-mapping
  '(("K" . "k")
    ("S" . "s")
    ("T" . "t")
    ("N" . "n")
    ("H" . "h")
    ("M" . "m")
    ("Y" . "y")
    ("R" . "r")
    ("W" . "w")
    ("G" . "g")
    ("Z" . "z")
    ("D" . "d")
    ("B" . "b")
    ("P" . "p"))
  "Mapping from okurigana consonants to lowercase equivalents.")

(defun nskk-pbt--generate-okurigana-consonant ()
  "Generate a random okurigana consonant marker."
  (nskk-pbt--random-choice nskk-pbt--okurigana-consonants))

(nskk-register-generator 'okurigana-consonant
  (lambda (&rest _args)
    (nskk-pbt--generate-okurigana-consonant)))

;; Okurigana pattern with romaji prefix
(nskk-register-generator 'okurigana-pattern
  (lambda (&optional prefix-length)
    (let* ((prefix-len (or prefix-length (nskk-pbt--random-int 1 5)))
           (prefix (mapconcat #'identity
                             (cl-loop repeat prefix-len
                                      collect (nskk-pbt--random-choice
                                               (append nskk-pbt--basic-vowels
                                                       nskk-pbt--consonant-vowel-basic)))
                             ""))
           (consonant (nskk-pbt--generate-okurigana-consonant)))
      (concat prefix consonant))))

;; Okurigana pattern with expected output
(nskk-register-generator 'okurigana-with-expected
  (lambda (&optional prefix-length)
    (let* ((prefix-len (or prefix-length (nskk-pbt--random-int 1 5)))
           (prefix (mapconcat #'identity
                             (cl-loop repeat prefix-len
                                      collect (nskk-pbt--random-choice
                                               (append nskk-pbt--basic-vowels
                                                       nskk-pbt--consonant-vowel-basic)))
                             ""))
           (consonant (nskk-pbt--generate-okurigana-consonant))
           (lower-consonant (cdr (assoc consonant nskk-pbt--okurigana-mapping))))
      (list :input (concat prefix consonant)
            :prefix prefix
            :consonant consonant
            :lower-consonant lower-consonant))))

;;;;
;;;; Generator 7: Search Query Generator
;;;;

(defconst nskk-pbt--search-types
  '(exact prefix partial fuzzy)
  "Valid search types for dictionary queries.")

(defconst nskk-pbt--okuri-types
  '(okuri-ari okuri-nasi nil)
  "Valid okurigana types for dictionary queries.")

(defconst nskk-pbt--hiragana-chars
  '("あ" "い" "う" "え" "お" "か" "き" "く" "け" "こ"
    "さ" "し" "す" "せ" "そ" "た" "ち" "つ" "て" "と"
    "な" "に" "ぬ" "ね" "の" "は" "ひ" "ふ" "へ" "ほ"
    "ま" "み" "む" "め" "も" "や" "ゆ" "よ"
    "ら" "り" "る" "れ" "ろ" "わ" "を" "ん")
  "Hiragana characters for dictionary query generation.")

(defconst nskk-pbt--katakana-chars
  '("ア" "イ" "ウ" "エ" "オ" "カ" "キ" "ク" "ケ" "コ"
    "サ" "シ" "ス" "セ" "ソ" "タ" "チ" "ツ" "テ" "ト"
    "ナ" "ニ" "ヌ" "ネ" "ノ" "ハ" "ヒ" "フ" "ヘ" "ホ"
    "マ" "ミ" "ム" "メ" "モ" "ヤ" "ユ" "ヨ"
    "ラ" "リ" "ル" "レ" "ロ" "ワ" "ヲ" "ン")
  "Katakana characters for dictionary query generation.")

(defun nskk-pbt--generate-search-query (&optional size)
  "Generate a dictionary search query string.
SIZE controls the length of the query."
  (let* ((sz (nskk-pbt--resolve-size size 3))
         (query-type (nskk-pbt--random 4)))
    (cl-case query-type
      (0 ;; Hiragana only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk-pbt--random-choice nskk-pbt--hiragana-chars))
                  ""))
      (1 ;; Katakana only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk-pbt--random-choice nskk-pbt--katakana-chars))
                  ""))
      (2 ;; Mixed hiragana/katakana
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (if (nskk-pbt--random-bool)
                                       (nskk-pbt--random-choice nskk-pbt--hiragana-chars)
                                     (nskk-pbt--random-choice nskk-pbt--katakana-chars)))
                  ""))
      (t ;; With okurigana marker
       (let ((base (mapconcat #'identity
                             (cl-loop repeat (1- sz)
                                      collect (nskk-pbt--random-choice nskk-pbt--hiragana-chars))
                             "")))
         (concat base (nskk-pbt--random-choice '("か" "き" "く" "け" "こ"
                                                  "さ" "し" "す" "せ" "そ"))))))))

(nskk-register-generator 'search-query
  (lambda (&rest args)
    (apply #'nskk-pbt--generate-search-query args)))

;; Search query with type
(nskk-register-generator 'search-query-with-type
  (lambda (&optional size)
    (let ((query (nskk-pbt--generate-search-query size))
          (search-type (nskk-pbt--random-choice nskk-pbt--search-types))
          (okuri-type (nskk-pbt--random-choice nskk-pbt--okuri-types)))
      (list :query query
            :search-type search-type
            :okuri-type okuri-type))))

;; Search parameters (query, type, limit)
(nskk-register-generator 'search-params
  (lambda (&optional size)
    (let ((sz (nskk-pbt--resolve-size size 3)))
      (list :query (nskk-pbt--generate-search-query sz)
            :search-type (nskk-pbt--random-choice nskk-pbt--search-types)
            :okuri-type (nskk-pbt--random-choice nskk-pbt--okuri-types)
            :limit (nskk-pbt--random-int 1 100)))))

;;;;
;;;; Composite Generators
;;;;

;; Generate a complete input scenario
(nskk-register-generator 'input-scenario
  (lambda (&optional size)
    (let* ((sz (nskk-pbt--resolve-size size 5))
           (mode (nskk-pbt--generate-valid-mode))
           (key-sequence (nskk-pbt--generate-key-sequence sz)))
      (list :mode mode
            :key-sequence key-sequence
            :expected-length (length key-sequence)))))

;; Generate conversion scenario (input to conversion)
(nskk-register-generator 'conversion-scenario
  (lambda (&optional size)
    (let* ((sz (nskk-pbt--resolve-size size 3))
           (romaji (nskk-pbt--generate-romaji-pattern sz 'basic))
           (mode (if (nskk-pbt--random-bool) 'hiragana 'katakana)))
      (list :romaji-input romaji
            :mode mode
            :has-okurigana (nskk-pbt--random-bool)))))

;; Generate mode switch scenario
(nskk-register-generator 'mode-switch-scenario
  (lambda (&rest _args)
    (let* ((initial-mode (nskk-pbt--generate-valid-mode))
           (target-mode (nskk-pbt--generate-valid-mode))
           (trigger-key (car (rassoc target-mode nskk-pbt--mode-switch-keys))))
      (list :initial-mode initial-mode
            :target-mode target-mode
            :trigger-key (or trigger-key "C-j")))))

;;;;
;;;; Henkan Phase Generator
;;;;

;; Generate any valid (or nil) henkan phase
(nskk-register-generator 'henkan-phase
  (lambda (&rest _)
    (nskk-pbt--random-choice '(nil on active list registration))))

;; Generate a converting phase only (excludes nil and on)
(nskk-register-generator 'converting-phase
  (lambda (&rest _)
    (nskk-pbt--random-choice '(active list registration))))


;;;;
;;;; Okurigana Full-Pattern Generator
;;;;

;; Generate a romaji prefix + uppercase consonant marker (full okurigana input)
(nskk-register-generator 'okurigana-full-pattern
  (lambda (&optional size)
    (let* ((sz (nskk-pbt--resolve-size size 3))
           (prefix (nskk-pbt--generate-romaji-pattern sz 'basic))
           (consonant (nskk-pbt--random-choice
                       '(?K ?S ?T ?N ?H ?M ?Y ?R ?W ?G ?Z ?D ?B ?P))))
      (list :prefix prefix
            :consonant consonant
            :full (concat prefix (string consonant))))))

;; Generate only the uppercase consonant character
(nskk-register-generator 'okurigana-consonant-char
  (lambda (&rest _)
    (nskk-pbt--random-choice '(?K ?S ?T ?N ?H ?M ?Y ?R ?W ?G ?Z ?D ?B ?P))))


;;;;
;;;; Candidate Index Generator
;;;;

;; Generate a valid zero-based index into a candidate list of given size
(nskk-register-generator 'candidate-index
  (lambda (&optional size)
    (let ((n (max 1 (or size (1+ (nskk-pbt--random-int 0 9))))))
      (nskk-pbt--random-int 0 (1- n)))))

;; Generate (candidates . index) pair where index is valid for candidates
(nskk-register-generator 'candidates-with-valid-index
  (lambda (&optional size)
    (let* ((pool '("漢字" "感じ" "幹事" "日本" "二本" "変換" "桜" "山" "川"))
           (n (max 1 (nskk-pbt--resolve-size size 3)))
           (candidates (cl-loop repeat n
                                collect (nskk-pbt--random-choice pool)))
           (index (nskk-pbt--random-int 0 (1- n))))
      (list :candidates candidates :index index))))


;;;;
;;;; Multi-Buffer Scenario Generator
;;;;

;; Generate independent state descriptors for multiple buffers
(nskk-register-generator 'multi-buffer-scenario
  (lambda (&optional size)
    (let ((n (max 2 (nskk-pbt--resolve-size size 3))))
      (cl-loop repeat n
               collect (list :mode (nskk-pbt--generate-valid-mode)
                             :input (nskk-pbt--generate-romaji-pattern 3 'basic)
                             :henkan-phase nil)))))


;;;;
;;;; Utility Functions
;;;;

(defun nskk-pbt-shrink (value &optional type)
  "Shrink VALUE towards a simpler form for counterexample minimization.
TYPE specifies the expected type for proper shrinking."
  (pcase type
    ('string
     (when (> (length value) 0)
       (substring value 0 (1- (length value)))))
    ('list
     (when (> (length value) 0)
       (cdr value)))
    ('integer
     (floor (/ value 2)))
    (_
     ;; Generic shrinking: try to make value smaller
     (cond
      ((stringp value)
       (when (> (length value) 0)
         (substring value 0 (1- (length value)))))
      ((listp value)
       (when (> (length value) 0)
         (cdr value)))
      ((integerp value)
       (floor (/ value 2)))
      (t value)))))

(defun nskk-pbt-classify (value &optional type)
  "Classify VALUE into a category for test result analysis.
TYPE specifies the classification type."
  (pcase type
    ('mode
     (cond
      ((memq value '(hiragana katakana katakana-半角)) 'kana)
      ((eq value 'ascii) 'ascii)
      ((eq value 'latin) 'latin)
      ((eq value 'abbrev) 'abbrev)
      (t 'unknown)))
    ('romaji
     (cond
      ((member value nskk-pbt--basic-vowels) 'vowel)
      ((member value nskk-pbt--incomplete-patterns) 'incomplete)
      ((member value nskk-pbt--special-patterns) 'special)
      (t 'standard)))
    ('search-type
     value)
    (_
     (cond
      ((stringp value)
       (cond
        ((string-match-p "^[a-z]+$" value) 'lowercase)
        ((string-match-p "^[A-Z]+$" value) 'uppercase)
        ((string-match-p "^[ぁ-ん]+$" value) 'hiragana)
        ((string-match-p "^[ァ-ン]+$" value) 'katakana)
        (t 'mixed)))
      ((listp value) 'list)
      ((integerp value) 'integer)
      (t 'unknown)))))

;;;;
;;;; Statistics and Debugging
;;;;

(defvar nskk-pbt--generation-stats nil
  "Statistics about generated values.")

(defun nskk-pbt-reset-stats ()
  "Reset generation statistics."
  (setq nskk-pbt--generation-stats
        (make-hash-table :test 'equal)))

(defun nskk-pbt-record-generation (generator-name value)
  "Record generation of VALUE by GENERATOR-NAME for statistics."
  (let* ((key (cons generator-name (nskk-pbt-classify value)))
         (count (gethash key nskk-pbt--generation-stats 0)))
    (puthash key (1+ count) nskk-pbt--generation-stats)))

(defun nskk-pbt-get-stats ()
  "Get current generation statistics."
  (when nskk-pbt--generation-stats
    (let ((result nil))
      (maphash (lambda (key count)
                 (push (cons key count) result))
               nskk-pbt--generation-stats)
      result)))

(provide 'nskk-pbt-generators)

;;; nskk-pbt-generators.el ends here
