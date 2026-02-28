EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

LOAD_PATH = -L . -L test -L test/integration -L test/unit -L test/e2e

# All source files
SRC = nskk.el nskk-custom.el nskk-state.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-converter.el nskk-debug.el \
       nskk-search.el nskk-cache.el nskk-kana.el \
       nskk-henkan.el nskk-input.el \
       nskk-dictionary.el \
       nskk-azik.el nskk-prolog.el

# Unit test files
UNIT_SRC = $(wildcard test/unit/*-test.el)

# Integration test files (with PBT)
INTEGRATION_SRC = test/integration/nskk-integration-test.el \
                  test/integration/nskk-state-machine-mode-test.el \
                  test/integration/nskk-state-machine-buffer-test.el \
                  test/integration/nskk-state-machine-candidate-test.el \
                  test/integration/nskk-sequence-test.el \
                  test/integration/nskk-layer-state-pbt-test.el \
                  test/integration/nskk-input-routing-pbt-test.el \
                  test/integration/nskk-conversion-flow-pbt-test.el \
                  test/integration/nskk-dictionary-integration-pbt-test.el \
                  test/integration/nskk-okurigana-pbt-test.el \
                  test/integration/nskk-multi-buffer-pbt-test.el \
                  test/integration/nskk-error-recovery-pbt-test.el

# E2E test files (full nskk-mode activation + execute-kbd-macro)
E2E_SRC = test/e2e/nskk-buffer-e2e-test.el \
           test/e2e/nskk-mode-transition-e2e-test.el \
           test/e2e/nskk-modeline-e2e-test.el \
           test/e2e/nskk-registration-e2e-test.el

.PHONY: all compile test test-integration test-unit test-e2e lint lint-checkdoc lint-elsa package-lint clean

all: compile

compile:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  $(foreach f,$(SRC),--eval "(byte-compile-file \"$(f)\")")

test: compile
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  -l test/nskk-pbt-generators.el \
	  -l test/nskk-pbt-shrink.el \
	  -l test/nskk-e2e-helpers.el \
	  $(foreach f,$(INTEGRATION_SRC),-l $(f)) \
	  $(foreach f,$(UNIT_SRC),-l $(f)) \
	  $(foreach f,$(E2E_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

test-integration: compile
	@echo "Running integration tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  -l test/nskk-pbt-generators.el \
	  -l test/nskk-pbt-shrink.el \
	  $(foreach f,$(INTEGRATION_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

test-unit:
	@echo "Running unit tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  $(foreach f,$(UNIT_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

test-e2e: compile
	@echo "Running E2E tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  -l test/nskk-e2e-helpers.el \
	  $(foreach f,$(E2E_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

lint: lint-elsa lint-checkdoc

lint-checkdoc:
	$(BATCH) $(LOAD_PATH) \
	  $(foreach f,$(SRC),--eval "(checkdoc-file \"$(f)\")")

lint-elsa:
	$(BATCH) $(LOAD_PATH) \
	  -l elsa \
	  -f elsa-run \
	  -with-exit \
	  $(SRC) 2>&1 | perl -pe 's/\e\[[0-9;]*m//g' | grep -E "^[^:]+\.el:[0-9]+:[0-9]+: (error|warning):" || true

package-lint:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -l package-lint \
	  -f package-lint-batch-and-exit nskk.el

clean:
	rm -f *.elc test/*.elc test/integration/*.elc test/unit/*.elc test/e2e/*.elc
