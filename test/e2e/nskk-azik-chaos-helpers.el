;;; nskk-azik-chaos-helpers.el --- Shared AZIK chaos test helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared event generation, dispatch, and reset helpers for AZIK stress tests.

;;; Code:

;;;;
;;;; Event Dispatch Helper
;;;;

(require 'cl-lib)

(require 'nskk-e2e-helpers)

(defun nskk--azik-chaos--dispatch-keys (keys-str)
  "Type KEYS-STR by dispatching each key event programmatically.
Mirrors `nskk-e2e-type' macro behavior as a callable function so it
can be used in loops with dynamic key values.
Silently ignores unknown key sequences.
Does NOT catch errors or quit signals — callers must handle those."
  (let ((key-vec (kbd keys-str)))
    (if (and (> (length keys-str) 0)
             (zerop (length key-vec)))
        ;; Fallback: kbd returned empty (e.g., ";;" parses as comment delimiter).
        ;; Dispatch each raw character code instead.
        (cl-loop for ch across keys-str
                 do (nskk-e2e--dispatch-event ch))
      (cl-loop for i from 0 below (length key-vec)
               do (nskk-e2e--dispatch-event (aref key-vec i))))))

;;;;
;;;; Event Pool
;;;;

(defconst nskk--azik-chaos--event-pool
  ;; Weighted pool: more entries → higher selection probability.
  ;;
  ;; Kana inputs (~55% of pool): core AZIK typing patterns
  '("ka" "ki" "ku" "ke" "ko"
    "sa"       "su" "se" "so"
    "ta"       "te" "to"
    "na" "ni"       "no"
    "ha" "hi"       "ho"
    "ma" "mi" "mu" "me" "mo"
    "ya"       "yu"       "yo"
    "ra" "ri" "ru" "re" "ro"
    "a"  "i"  "u"  "e"  "o"
    ;; AZIK hatsuon: consonant + n-trigger (~10% weight)
    ;; kz/sz/tz/nz already present; add hz mz gz dz bz pz (2 each)
    "kz" "sz" "tz" "nz"
    "hz" "hz" "mz" "mz" "gz" "gz" "dz" "dz" "bz" "bz" "pz" "pz"
    ;; AZIK diphthong: consonant + q-trigger (~10% weight)
    ;; kq already present; add sq tq nq hq mq gq dq bq pq (2 each)
    "kq" "kh" "kw"
    "sq" "sq" "tq" "tq" "nq" "nq" "hq" "hq" "mq" "mq"
    "gq" "gq" "dq" "dq" "bq" "bq" "pq" "pq"
    ;; AZIK vowel-shadow: consonant + shadow key (~10% weight)
    ;; sh th dh wh (2 each)
    "sh" "sh" "th" "th" "dh" "dh" "wh" "wh"
    ;; AZIK word shortcuts
    "sr" "ms"
    ;; AZIK special keys: っ and ー (doubled for realistic frequency)
    ";" ";" ":" ":"
    ;; Colon-okurigana trigger: additional ":" entries
    ":" ":"
    ;; JP106 sokuon-okurigana trigger (+) and preedit marker (*).
    ;; Lower probability than kana keys; "+" fires dict lookup in preedit,
    ;; "*" inserts the okurigana marker directly.
    "+" "+"
    "*"
    ;; Standalone q (katakana toggle in preedit)
    "q" "q"
    ;; Okurigana-starting: uppercase consonant + vowel (starts preedit)
    "Ka" "Sa" "Na" "Ha" "Ma" "Ra" "Ta"
    ;; Control keys (fewer entries → lower probability)
    ;; SPC: trigger conversion when preedit active, else pass-through
    "SPC" "SPC" "SPC" "SPC" "SPC"
    ;; C-g: cancel preedit/conversion
    "C-g" "C-g"
    ;; RET: commit current state
    "RET" "RET"
    ;; C-j: explicit commit / switch to hiragana
    "C-j" "C-j"
    ;; DEL: delete
    "DEL")
  "Weighted event pool for AZIK chaos testing.
Events are listed multiple times to bias random selection toward
realistic Japanese input patterns.")

(defconst nskk--azik-chaos--pool-size
  (length nskk--azik-chaos--event-pool)
  "Length of `nskk--azik-chaos--event-pool' for modulo indexing.")

(defun nskk--azik-chaos--pick-event ()
  "Pick a random event from the chaos event pool."
  (nth (random nskk--azik-chaos--pool-size)
       nskk--azik-chaos--event-pool))

(defun nskk--azik-chaos--generate-sequence (length)
  "Generate a random event sequence of LENGTH events from the pool."
  (cl-loop repeat length collect (nskk--azik-chaos--pick-event)))

;;;;
;;;; State Reset
;;;;

(defun nskk--azik-chaos--reset-to-idle ()
  "Reset NSKK to a known-clean hiragana-idle state within a live session.
Sends C-g twice to cancel any pending conversion or preedit, then C-j
to ensure hiragana mode, then erases the buffer and clears the romaji
buffer.  This allows multiple chaos scenarios to run inside one
`nskk-e2e-with-azik-buffer' session without N Prolog DB snapshots."
  ;; Cancel active conversion and preedit (two C-g to handle double nesting)
  (condition-case nil (nskk-e2e--dispatch-event 7) (error nil) (quit nil)) ; C-g
  (condition-case nil (nskk-e2e--dispatch-event 7) (error nil) (quit nil)) ; C-g again
  ;; Switch to hiragana mode
  (condition-case nil (nskk-e2e--dispatch-event ?\C-j) (error nil) (quit nil))
  ;; Wipe buffer content
  (erase-buffer)
  ;; Clear any partial romaji
  (nskk-state-set-romaji-buffer "")
  ;; Clear all AZIK deferred states
  (when (fboundp 'nskk-deferred-azik-state)
    (nskk-set-deferred-azik-state nil))
  (when (fboundp 'nskk-deferred-vowel-shadow-state)
    (nskk-set-deferred-vowel-shadow-state nil))
  (when (fboundp 'nskk-azik-colon-okuri-pending)
    (nskk-set-azik-colon-okuri-pending nil))
  (when (fboundp 'nskk-azik-colon-okuri-deferred)
    (nskk-set-azik-colon-okuri-deferred nil))
  (when (fboundp 'nskk-azik-sokuon-okuri-kana-pending)
    (nskk-set-azik-sokuon-okuri-kana-pending nil)))

(provide 'nskk-azik-chaos-helpers)

;;; nskk-azik-chaos-helpers.el ends here
