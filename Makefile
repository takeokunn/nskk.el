EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

LOAD_PATH = -L . -L test -L test/integration -L test/unit -L test/e2e

# All source files (core + optional extension modules)
SRC = nskk-cps-macros.el nskk-prolog.el nskk-trie.el \
       nskk-custom.el nskk-debug.el \
       nskk-kana.el nskk-converter.el nskk-cache.el nskk-dictionary.el nskk-search.el \
       nskk-state.el nskk-henkan.el nskk-input.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-server.el nskk-program-dictionary.el nskk-azik.el \
       nskk-annotation.el nskk-context.el nskk-inline.el \
       nskk-isearch.el nskk-region.el nskk-show-mode.el \
       nskk.el

# Unit test files
UNIT_SRC = $(wildcard test/unit/*-test.el)

# Integration test files (with PBT)
INTEGRATION_SRC = test/integration/nskk-integration-test.el \
                  test/integration/nskk-sequence-test.el \
                  test/integration/nskk-prolog-integration-test.el \
                  test/integration/nskk-input-routing-pbt-test.el \
                  test/integration/nskk-conversion-flow-pbt-test.el \
                  test/integration/nskk-dictionary-integration-pbt-test.el \
                  test/integration/nskk-okurigana-pbt-test.el \
                  test/integration/nskk-henkan-pipeline-integration-test.el \
                  test/integration/nskk-azik-integration-test.el \
                  test/integration/nskk-search-cache-integration-test.el \
                  test/integration/nskk-dict-registration-integration-test.el \
                  test/integration/nskk-server-integration-test.el \
                  test/integration/nskk-modeline-state-integration-test.el \
                  test/integration/nskk-candidate-window-integration-test.el \
                  test/integration/nskk-server-henkan-integration-test.el \
                  test/integration/nskk-search-strategy-integration-test.el \
                  test/integration/nskk-initialization-integration-test.el \
                  test/integration/nskk-kana-integration-test.el \
                  test/integration/nskk-debug-integration-test.el \
                  test/integration/nskk-state-machine-mode-test.el \
                  test/integration/nskk-state-machine-candidate-test.el \
                  test/integration/nskk-state-machine-buffer-test.el \
                  test/integration/nskk-multi-buffer-pbt-test.el \
                  test/integration/nskk-error-recovery-pbt-test.el \
                  test/integration/nskk-layer-state-pbt-test.el \
                  test/integration/nskk-custom-integration-test.el

# E2E test files (full nskk-mode activation + execute-kbd-macro)
E2E_SRC = test/e2e/nskk-abbrev-e2e-test.el \
           test/e2e/nskk-azik-e2e-test.el \
           test/e2e/nskk-dcomp-e2e-test.el \
           test/e2e/nskk-henkan-e2e-test.el \
           test/e2e/nskk-kana-input-e2e-test.el \
           test/e2e/nskk-modeline-e2e-test.el \
           test/e2e/nskk-mode-transition-e2e-test.el \
           test/e2e/nskk-navigation-e2e-test.el \
           test/e2e/nskk-numeric-e2e-test.el \
           test/e2e/nskk-okurigana-e2e-test.el \
           test/e2e/nskk-registration-e2e-test.el \
           test/e2e/nskk-sticky-e2e-test.el

.PHONY: all compile test test-integration test-unit test-e2e bench lint lint-checkdoc package-lint clean

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

test-unit: compile
	@echo "Running unit tests..."
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  -l test/nskk-pbt-generators.el \
	  -l test/nskk-pbt-shrink.el \
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

bench: compile
	@echo "Running benchmarks..."
	$(BATCH) $(LOAD_PATH) -L test/bench \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  -l test/nskk-e2e-helpers.el \
	  -l test/bench/nskk-bench.el

lint: lint-checkdoc

lint-checkdoc:
	$(BATCH) $(LOAD_PATH) \
	  $(foreach f,$(SRC),--eval "(checkdoc-file \"$(f)\")")

package-lint:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -l package-lint \
	  -f package-lint-batch-and-exit nskk.el

clean:
	rm -f *.elc test/*.elc test/integration/*.elc test/unit/*.elc test/e2e/*.elc
