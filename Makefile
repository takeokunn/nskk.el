EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

LOAD_PATH = -L . -L test -L test/pbt -L test/unit

# All source files
SRC = nskk.el nskk-state.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-converter.el nskk-debug.el \
       nskk-search.el nskk-cache.el nskk-kana.el \
       nskk-henkan.el nskk-input.el \
       nskk-dictionary.el nskk-trie.el \
       nskk-azik.el nskk-macros.el nskk-prolog.el

# Unit test files
UNIT_SRC = $(wildcard test/unit/*-test.el)

# Property-based test files (PBT)
PBT_SRC = test/pbt/nskk-pbt-generators.el \
          test/pbt/nskk-pbt-shrink.el \
          test/pbt/nskk-state-machine-mode-test.el \
          test/pbt/nskk-state-machine-buffer-test.el \
          test/pbt/nskk-state-machine-candidate-test.el \
          test/pbt/nskk-sequence-test.el \
          test/pbt/nskk-layer-state-pbt-test.el \
          test/pbt/nskk-input-routing-pbt-test.el \
          test/pbt/nskk-conversion-flow-pbt-test.el \
          test/pbt/nskk-dictionary-integration-pbt-test.el \
          test/pbt/nskk-okurigana-pbt-test.el \
          test/pbt/nskk-multi-buffer-pbt-test.el \
          test/pbt/nskk-error-recovery-pbt-test.el

.PHONY: all compile test test-pbt test-unit lint package-lint clean

all: compile

compile:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  $(foreach f,$(SRC),--eval "(byte-compile-file \"$(f)\")")

test:
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  $(foreach f,$(PBT_SRC),-l $(f)) \
	  $(foreach f,$(UNIT_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

test-pbt: compile
	@echo "Running property-based tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  $(foreach f,$(PBT_SRC),-l $(f)) \
	  --eval "(ert-run-tests-batch-and-exit '\"nskk-property\")"

test-unit:
	@echo "Running unit tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  $(foreach f,$(UNIT_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) $(LOAD_PATH) \
	  $(foreach f,$(SRC),--eval "(checkdoc-file \"$(f)\")")

package-lint:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -l package-lint \
	  -f package-lint-batch-and-exit nskk.el

clean:
	rm -f *.elc test/*.elc test/pbt/*.elc test/unit/*.elc
