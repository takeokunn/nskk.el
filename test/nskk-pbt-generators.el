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

(defvar nskk--pbt-current-seed nil
  "Current random seed for deterministic generation.
When nil, uses system random.")

(defvar nskk--pbt-seed-state nil
  "Internal state for seeded random generation.")

(defun nskk-pbt-set-seed (seed)
  "Set the random SEED for deterministic generation.
SEED should be an integer. Use nil to reset to system random."
  (setq nskk--pbt-current-seed seed)
  (when seed
    ;; Initialize linear congruential generator state
    (setq nskk--pbt-seed-state (mod seed (expt 2 31)))))

(defun nskk--pbt-random (&optional limit)
  "Generate a random number with optional LIMIT.
Uses seeded generation if seed is set, otherwise uses system random."
  (if nskk--pbt-current-seed
      ;; Linear congruential generator (same algorithm as many languages)
      (let ((new-state (mod (+ (* 1103515245 nskk--pbt-seed-state) 12345)
                            (expt 2 31))))
        (setq nskk--pbt-seed-state new-state)
        (if limit
            (mod new-state limit)
          new-state))
    ;; Use system random
    (if limit
        (random limit)
      (random))))

(defun nskk--pbt-random-choice (list)
  "Choose a random element from LIST."
  (when list
    (nth (nskk--pbt-random (length list)) list)))

(defun nskk--pbt-random-bool ()
  "Generate a random boolean value."
  (= (nskk--pbt-random 2) 0))

(defun nskk--pbt-random-int (min max)
  "Generate a random integer between MIN and MAX (inclusive)."
  (+ min (nskk--pbt-random (1+ (- max min)))))

(defun nskk--pbt-random-string (chars length)
  "Generate a random string from CHARS with specified LENGTH."
  (apply #'string
         (cl-loop repeat length
                  collect (nskk--pbt-random-choice chars))))

;;;;
;;;; Size Parameter Helper
;;;;

(defun nskk--pbt-resolve-size (&optional size default)
  "Resolve SIZE parameter, using DEFAULT if nil.
DEFAULT defaults to 5 if not specified."
  (or size default 5))

;;;;
;;;; Generator 1: Keypress Sequence Generator
;;;;

(defconst nskk--pbt-lowercase-letters
  (string-to-list "abcdefghijklmnopqrstuvwxyz")
  "List of lowercase letter characters.")

(defconst nskk--pbt-uppercase-letters
  (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "List of uppercase letter characters.")

(defconst nskk--pbt-mode-switch-keys
  '(("C-j" . hiragana)
    ("q" . katakana)
    ("l" . latin)
    (";" . abbrev))
  "Mode switch key sequences with their target modes.")

(cl-defun nskk--pbt-generate-key-char (&key include-special include-uppercase)
  "Generate a single key character.
When INCLUDE-SPECIAL is non-nil, may generate special keys.
When INCLUDE-UPPERCASE is non-nil, may generate uppercase letters."
  (let ((choices (append nskk--pbt-lowercase-letters
                        (when include-uppercase nskk--pbt-uppercase-letters))))
    (if (and include-special (nskk--pbt-random-bool))
        ;; Generate special key (represented as string)
        (nskk--pbt-random-choice (mapcar #'car nskk--pbt-mode-switch-keys))
      ;; Generate regular character
      (string (nskk--pbt-random-choice choices)))))

(defun nskk--pbt-generate-key-sequence (&optional length)
  "Generate a sequence of key presses with optional LENGTH.
Default length is 1-20 characters if not specified."
  (let* ((len (or length (nskk--pbt-random-int 1 20)))
         (result nil))
    (dotimes (_ len)
      (push (nskk--pbt-generate-key-char
             :include-special (nskk--pbt-random-bool)
             :include-uppercase (nskk--pbt-random-bool))
            result))
    (nreverse result)))

(nskk-register-generator 'key-sequence
  (lambda (&rest args)
    (apply #'nskk--pbt-generate-key-sequence args)))

;; Composable key sequence with length parameter
(nskk-register-generator 'key-sequence-of-length
  (lambda (length)
    (nskk--pbt-generate-key-sequence length)))

;; Key sequence with only lowercase letters (excluding mode-switch keys: q, l, ;)
(defconst nskk--pbt-typing-letters
  (cl-remove-if (lambda (c) (memq c '(?q ?l ?\;)))
                (string-to-list "abcdefghijklmnopqrstuvwxyz"))
  "List of lowercase letter characters excluding mode-switch keys.")

(nskk-register-generator 'typing-key-sequence
  (lambda (&optional length)
    (let* ((len (or length (nskk--pbt-random-int 1 20))))
      (cl-loop repeat len
               collect (string (nskk--pbt-random-choice nskk--pbt-typing-letters))))))

;;;;
;;;; Generator 2: Valid Mode Generator
;;;;

(defconst nskk--pbt-valid-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin)
  "List of valid NSKK modes.")

(defun nskk--pbt-generate-valid-mode ()
  "Generate a random valid NSKK mode."
  (nskk--pbt-random-choice nskk--pbt-valid-modes))

(nskk-register-generator 'valid-mode
  (lambda (&rest _args)
    (nskk--pbt-generate-valid-mode)))

;;;;
;;;; Generator 3: State Object Generator
;;;;

(defun nskk--pbt-generate-input-buffer (&optional max-length)
  "Generate a random input buffer string.
MAX-LENGTH defaults to 20 if not specified."
  (let* ((len (nskk--pbt-random-int 0 (or max-length 20)))
         (chars (append nskk--pbt-lowercase-letters
                       (string-to-list "aeiou"))))
    (if (= len 0)
        ""
      (nskk--pbt-random-string chars len))))

(defun nskk--pbt-generate-candidates (&optional count)
  "Generate a list of random conversion candidates.
COUNT defaults to 0-10 if not specified."
  (let* ((num (or count (nskk--pbt-random-int 0 10)))
         (hiragana-chars '("あ" "い" "う" "え" "お" "か" "き" "く" "け" "こ"
                           "さ" "し" "す" "せ" "そ" "た" "ち" "つ" "て" "と"))
         (kanji-chars '("漢" "字" "変" "換" "日" "本" "語" "入" "力")))
    (cl-loop repeat num
             collect (let ((candidate-type (nskk--pbt-random 3)))
                       (cond
                        ((= candidate-type 0)
                         ;; Hiragana candidate
                         (mapconcat #'identity
                                   (cl-loop repeat (nskk--pbt-random-int 1 5)
                                            collect (nskk--pbt-random-choice hiragana-chars))
                                   ""))
                        ((= candidate-type 1)
                         ;; Kanji candidate
                         (mapconcat #'identity
                                   (cl-loop repeat (nskk--pbt-random-int 1 3)
                                            collect (nskk--pbt-random-choice kanji-chars))
                                   ""))
                        (t
                         ;; Mixed candidate
                         (concat (nskk--pbt-random-choice hiragana-chars)
                                (nskk--pbt-random-choice kanji-chars))))))))

;;;;
;;;; Generator 4: Romaji Pattern Generator
;;;;

(defconst nskk--pbt-basic-vowels
  '("a" "i" "u" "e" "o")
  "Basic vowel patterns.")

(defconst nskk--pbt-consonant-vowel-basic
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

(defconst nskk--pbt-consonant-vowel-dakuon
  '("ga" "gi" "gu" "ge" "go"
    "za" "ji" "zu" "ze" "zo"
    "da" "ji" "zu" "de" "do"
    "ba" "bi" "bu" "be" "bo"
    "pa" "pi" "pu" "pe" "po")
  "Dakuon (voiced) consonant+vowel combinations.")

(defconst nskk--pbt-special-patterns
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

(defconst nskk--pbt-incomplete-patterns
  '("k" "ky" "g" "gy" "s" "sh" "sy"
    "z" "j" "t" "ts" "ch" "ty" "d"
    "n" "ny" "h" "hy" "f" "b" "by"
    "p" "py" "m" "my" "y" "r" "ry" "w")
  "Incomplete romaji patterns (awaiting more input).")

(defun nskk--pbt-generate-romaji-pattern (&optional size type)
  "Generate a romaji pattern with optional SIZE and TYPE.
TYPE can be 'basic, 'extended, 'incomplete, or nil for random."
  (let* ((sz (nskk--pbt-resolve-size size 3))
         (pattern-type (or type
                          (nskk--pbt-random-choice '(basic extended mixed incomplete)))))
    (cl-case pattern-type
      (basic
       ;; Basic patterns only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk--pbt-random-choice
                                    (append nskk--pbt-basic-vowels
                                            nskk--pbt-consonant-vowel-basic)))
                  ""))
      (extended
       ;; Extended patterns including special
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk--pbt-random-choice
                                    (append nskk--pbt-consonant-vowel-basic
                                            nskk--pbt-consonant-vowel-dakuon
                                            nskk--pbt-special-patterns)))
                  ""))
      (incomplete
       ;; Incomplete pattern
       (if (nskk--pbt-random-bool)
           (nskk--pbt-random-choice nskk--pbt-incomplete-patterns)
         ;; Mixed: complete syllables + incomplete ending
         (concat (mapconcat #'identity
                           (cl-loop repeat (max 0 (1- sz))
                                    collect (nskk--pbt-random-choice
                                             (append nskk--pbt-consonant-vowel-basic
                                                     nskk--pbt-special-patterns)))
                           "")
                 (nskk--pbt-random-choice nskk--pbt-incomplete-patterns))))
      (t
       ;; Mixed: all types
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk--pbt-random-choice
                                    (append nskk--pbt-basic-vowels
                                            nskk--pbt-consonant-vowel-basic
                                            nskk--pbt-consonant-vowel-dakuon
                                            nskk--pbt-special-patterns)))
                  "")))))

(nskk-register-generator 'romaji-pattern
  (lambda (&rest args)
    (apply #'nskk--pbt-generate-romaji-pattern args)))

;; Specific romaji pattern types
(nskk-register-generator 'romaji-basic
  (lambda (&optional size)
    (nskk--pbt-generate-romaji-pattern size 'basic)))

;;;;
;;;; Generator 5: AZIK Rule Generator
;;;;

;; AZIK rule categories for stratified sampling
(defconst nskk--pbt-azik-categories
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
    ;; "sh", "th", "dh", "wh" are demoted to :incomplete by
    ;; nskk-azik-demote-shadow-keys (they shadow standard romaji prefixes).
    ;; They are excluded here to keep only patterns that produce real output.
    (diphthong-s . ("sq" "sw" "sp"))
    (diphthong-t . ("tq" "tw" "tp"))
    (diphthong-n . ("nq" "nh" "nw" "np"))
    (diphthong-h . ("hq" "hh" "hw" "hp"))
    (diphthong-m . ("mq" "mh" "mw" "mp"))
    (diphthong-y . ("yq" "yh" "yw" "yp"))
    (diphthong-r . ("rq" "rh" "rw" "rp"))
    (diphthong-w . ("wq" "ww" "wp"))
    (diphthong-g . ("gq" "gh" "gw" "gp"))
    (diphthong-z . ("zq" "zh" "zw" "zp"))
    (diphthong-d . ("dq" "dw" "dp"))
    (diphthong-b . ("bq" "bh" "bw" "bp"))
    (diphthong-p . ("pq" "ph" "pw" "pp"))
    (youon-ng . ("nga" "ngu" "nge" "ngo" "ngz" "ngk" "ngj" "ngd" "ngl" "ngq" "ngh" "ngw" "ngp"))
    (youon-kg . ("kga" "kgu" "kge" "kgo" "kgz" "kgk" "kgj" "kgd" "kgl" "kgq" "kgh" "kgw" "kgp"))
    (youon-hg . ("hga" "hgu" "hge" "hgo" "hgz" "hgk" "hgj" "hgd" "hgl" "hgq" "hgh" "hgw" "hgp"))
    (youon-mg . ("mga" "mgu" "mge" "mgo" "mgz" "mgk" "mgj" "mgd" "mgl" "mgq" "mgh" "mgw" "mgp"))
    (youon-rg . ("rga" "rgu" "rge" "rgo" "rgz" "rgk" "rgj" "rgd" "rgl" "rgq" "rgh" "rgw" "rgp"))
    (youon-gg . ("gga" "ggu" "gge" "ggo" "ggz" "ggk" "ggj" "ggd" "ggl" "ggq" "ggh" "ggw" "ggp"))
    (youon-jg . ("jga" "jgu" "jge" "jgo" "jgz" "jgk" "jgj" "jgd" "jgl" "jgq" "jgh" "jgw" "jgp"))
    (youon-bg . ("bga" "bgu" "bge" "bgo" "bgz" "bgk" "bgj" "bgd" "bgl" "bgq" "bgh" "bgw" "bgp"))
    (youon-pg . ("pga" "pgu" "pge" "pgo" "pgz" "pgk" "pgj" "pgd" "pgl" "pgq" "pgh" "pgw" "pgp"))
    (same-finger . ("kf" "nf" "mf" "gf" "pf" "rf" "yf"))
    (special-ext . ("km" "kr" "gr" "kt" "gt" "zr" "st" "ss" "sr" "tt" "dt"
                    "tb" "tm" "tr" "ds" "dm" "nr" "nt" "nb" "ht" "bt"
                    "ms" "mt" "mn" "yr" "rr" "wt" "wr"))
    (gairaigo . ("tgi" "tgu" "dci" "dcu" "wso"
                 "tgk" "tgj" "dck" "dcj" "wsok"
                 "tgq" "tgh" "tgw" "tgp"
                 "dcq" "dch" "dcw" "dcp"
                 "wsoq" "wsoh" "wsow" "wsop"))
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

(defun nskk--pbt-get-all-azik-patterns ()
  "Get all AZIK patterns as a flat list."
  (apply #'append (mapcar #'cdr nskk--pbt-azik-categories)))

(defun nskk--pbt-generate-azik-rule (&optional category)
  "Generate an AZIK rule test case.
If CATEGORY is specified, sample from that category only.
Uses stratified sampling for efficiency when CATEGORY is nil."
  (if category
      ;; Sample from specific category
      (let ((cat-patterns (cdr (assoc category nskk--pbt-azik-categories))))
        (if cat-patterns
            (nskk--pbt-random-choice cat-patterns)
          (error "Unknown AZIK category: %s" category)))
    ;; Stratified sampling across all categories
    (let* ((selected-category (nskk--pbt-random-choice nskk--pbt-azik-categories))
           (patterns (cdr selected-category)))
      (nskk--pbt-random-choice patterns))))

(nskk-register-generator 'azik-rule
  (lambda (&rest args)
    (apply #'nskk--pbt-generate-azik-rule args)))

;;;;
;;;; Generator 6: Search Query Generator
;;;;

(defconst nskk--pbt-hiragana-chars
  '("あ" "い" "う" "え" "お" "か" "き" "く" "け" "こ"
    "さ" "し" "す" "せ" "そ" "た" "ち" "つ" "て" "と"
    "な" "に" "ぬ" "ね" "の" "は" "ひ" "ふ" "へ" "ほ"
    "ま" "み" "む" "め" "も" "や" "ゆ" "よ"
    "ら" "り" "る" "れ" "ろ" "わ" "を" "ん")
  "Hiragana characters for dictionary query generation.")

(defconst nskk--pbt-katakana-chars
  '("ア" "イ" "ウ" "エ" "オ" "カ" "キ" "ク" "ケ" "コ"
    "サ" "シ" "ス" "セ" "ソ" "タ" "チ" "ツ" "テ" "ト"
    "ナ" "ニ" "ヌ" "ネ" "ノ" "ハ" "ヒ" "フ" "ヘ" "ホ"
    "マ" "ミ" "ム" "メ" "モ" "ヤ" "ユ" "ヨ"
    "ラ" "リ" "ル" "レ" "ロ" "ワ" "ヲ" "ン")
  "Katakana characters for dictionary query generation.")

(defun nskk--pbt-generate-search-query (&optional size)
  "Generate a dictionary search query string.
SIZE controls the length of the query."
  (let* ((sz (nskk--pbt-resolve-size size 3))
         (query-type (nskk--pbt-random 4)))
    (cl-case query-type
      (0 ;; Hiragana only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk--pbt-random-choice nskk--pbt-hiragana-chars))
                  ""))
      (1 ;; Katakana only
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (nskk--pbt-random-choice nskk--pbt-katakana-chars))
                  ""))
      (2 ;; Mixed hiragana/katakana
       (mapconcat #'identity
                  (cl-loop repeat sz
                           collect (if (nskk--pbt-random-bool)
                                       (nskk--pbt-random-choice nskk--pbt-hiragana-chars)
                                     (nskk--pbt-random-choice nskk--pbt-katakana-chars)))
                  ""))
      (t ;; With okurigana marker
       (let ((base (mapconcat #'identity
                             (cl-loop repeat (1- sz)
                                      collect (nskk--pbt-random-choice nskk--pbt-hiragana-chars))
                             "")))
         (concat base (nskk--pbt-random-choice '("か" "き" "く" "け" "こ"
                                                  "さ" "し" "す" "せ" "そ"))))))))

(nskk-register-generator 'search-query
  (lambda (&rest args)
    (apply #'nskk--pbt-generate-search-query args)))

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

(provide 'nskk-pbt-generators)

;;; nskk-pbt-generators.el ends here
