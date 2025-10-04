# Makefile for NSKK

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L . -L tests

# ソースファイル
SOURCES = $(wildcard nskk-*.el)
TEST_SOURCES = \
	tests/nskk-core-smoke-test.el \
	tests/nskk-runtime-integration-test.el \
	tests/nskk-runtime-integration-threadsafe-test.el \
	tests/nskk-advanced-integration-test.el

.PHONY: all test coverage clean help 

all: test

## テスト実行
test:
	@echo "=== Running NSKK Tests ==="
	$(BATCH) \
		-l tests/nskk-test-framework.el \
		$(foreach test,$(TEST_SOURCES),-l $(test)) \
		-f ert-run-tests-batch-and-exit

## カバレッジ測定とレポート生成
coverage:
	@echo "=== Running NSKK Tests with Coverage ==="
	$(BATCH) -l tests/run-coverage.el || true

## HTMLレポートをブラウザで開く
coverage-report: coverage
	@echo "Opening coverage report..."
	@if [ -f coverage/index.html ]; then \
		if command -v open > /dev/null; then \
			open coverage/index.html; \
		elif command -v xdg-open > /dev/null; then \
			xdg-open coverage/index.html; \
		else \
			echo "Coverage report generated at coverage/index.html"; \
		fi \
	else \
		echo "Error: coverage/index.html not found"; \
	fi

## カバレッジデータとレポートをクリーンアップ
clean:
	@echo "Cleaning coverage data..."
	rm -rf coverage/
	rm -f *.elc tests/*.elc

## ヘルプ表示
help:
	@echo "NSKK Makefile Commands:"
	@echo ""
	@echo "Testing:"
	@echo "  make test             - Run all tests"
	@echo "  make coverage         - Run tests with coverage measurement"
	@echo "  make coverage-report  - Generate and open HTML coverage report"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean            - Remove coverage data and compiled files"
	@echo "  make help             - Show this help message"

