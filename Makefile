# Makefile for NSKK

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L . -L tests

# ソースファイル
SOURCES = $(wildcard nskk-*.el)

# 全テストファイル自動検出
ALL_TEST_FILES = $(wildcard tests/*-test.el)

# 統合テスト
INTEGRATION_TESTS = \
	tests/nskk-core-smoke-test.el \
	tests/nskk-integration-test.el \
	tests/nskk-runtime-integration-test.el \
	tests/nskk-runtime-integration-threadsafe-test.el \
	tests/nskk-advanced-integration-test.el

# E2Eテスト
E2E_TESTS = \
	tests/nskk-e2e-basic-test.el \
	tests/nskk-e2e-mode-control-test.el

.PHONY: all test test-unit test-integration test-e2e test-scenarios test-scenarios-all test-scenarios-basic test-scenarios-beginner list-scenarios coverage clean help

all: test

## 全テスト実行
test:
	@echo "=== Running All NSKK Tests ==="
	@echo "Found $(words $(ALL_TEST_FILES)) test files"
	$(BATCH) \
		$(foreach test,$(ALL_TEST_FILES),-l $(test)) \
		-f ert-run-tests-batch-and-exit

## 統合テストのみ実行
test-integration:
	@echo "=== Running Integration Tests ==="
	$(BATCH) \
		$(foreach test,$(INTEGRATION_TESTS),-l $(test)) \
		-f ert-run-tests-batch-and-exit

## E2Eテストのみ実行
test-e2e:
	@echo "=== Running E2E Tests ==="
	$(BATCH) \
		$(foreach test,$(E2E_TESTS),-l $(test)) \
		-f ert-run-tests-batch-and-exit

## シナリオテスト実行
test-scenarios: test-scenarios-all

## 全シナリオテスト実行
test-scenarios-all:
	@echo "=== Running All Scenario Tests ==="
	$(BATCH) -L tests/scenarios/basic \
		-l tests/nskk-scenario-suite.el \
		-f nskk-scenario-run-all-batch

## 基本シナリオのみ実行
test-scenarios-basic:
	@echo "=== Running Basic Scenario Tests ==="
	$(BATCH) -L tests/scenarios/basic \
		-l tests/nskk-scenario-suite.el \
		-f nskk-scenario-run-basic-batch

## 初心者向けシナリオのみ実行
test-scenarios-beginner:
	@echo "=== Running Beginner Scenario Tests ==="
	$(BATCH) -L tests/scenarios/basic \
		-l tests/nskk-scenario-suite.el \
		--eval "(ert-run-tests-batch-and-exit '(tag :beginner))"

## シナリオリスト表示
list-scenarios:
	@echo "=== NSKK Scenario Tests ==="
	$(BATCH) -L tests/scenarios/basic \
		-l tests/nskk-scenario-suite.el \
		--eval "(nskk-scenario-list-all)" \
		--eval "(with-current-buffer \"*NSKK Scenarios*\" (princ (buffer-string)))"

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
	@echo "  make test                  - Run all tests (auto-detect all *-test.el)"
	@echo "  make test-integration      - Run integration tests only"
	@echo "  make test-e2e              - Run E2E tests only"
	@echo "  make test-scenarios        - Run all scenario tests"
	@echo "  make test-scenarios-basic  - Run basic scenario tests"
	@echo "  make test-scenarios-beginner - Run beginner scenario tests"
	@echo "  make list-scenarios        - List all available scenarios"
	@echo "  make coverage              - Run tests with coverage measurement"
	@echo "  make coverage-report       - Generate and open HTML coverage report"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean                 - Remove coverage data and compiled files"
	@echo "  make help                  - Show this help message"

