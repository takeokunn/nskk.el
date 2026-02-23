EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

LOAD_PATH = -L . -L test
# Core functionality files (working)
SRC_CORE = nskk.el nskk-custom.el nskk-state.el nskk-mode-switch.el nskk-events.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-converter.el nskk-cache.el nskk-core.el \
       nskk-thread-pool.el

# All source files (including those with issues)
SRC = nskk.el nskk-custom.el nskk-state.el nskk-mode-switch.el nskk-events.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-converter.el nskk-optimize.el nskk-native-compile.el \
       nskk-search.el nskk-cache.el nskk-thread-pool.el \
       nskk-layer-core.el nskk-layer-infrastructure.el \
       nskk-layer-application.el nskk-layer-extension.el nskk-layer-presentation.el nskk-layer-qa.el \
       nskk-architecture.el nskk-input-commands.el \
       nskk-dict-io.el nskk-dict-struct.el nskk-dict-errors.el nskk-trie.el

CORE_SRC = nskk.el nskk-custom.el nskk-state.el nskk-mode-switch.el nskk-events.el \
       nskk-keymap.el nskk-modeline.el nskk-candidate-window.el \
       nskk-converter.el nskk-optimize.el nskk-native-compile.el \
       nskk-search.el nskk-cache.el nskk-thread-pool.el
TEST_SRC = $(wildcard test/*-test.el)

.PHONY: all compile compile-core test lint package-lint clean

all: compile

compile:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  $(foreach f,$(SRC),--eval "(byte-compile-file \"$(f)\")")

test:
	$(BATCH) $(LOAD_PATH) \
	  -l test/nskk-test-macros.el \
	  -l test/nskk-test-framework.el \
	  $(foreach f,$(TEST_SRC),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) $(LOAD_PATH) \
	  $(foreach f,$(CORE_SRC),--eval "(checkdoc-file \"$(f)\")")

package-lint:
	$(BATCH) $(LOAD_PATH) \
	  --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -l package-lint \
	  -f package-lint-batch-and-exit nskk.el

clean:
	rm -f *.elc test/*.elc
