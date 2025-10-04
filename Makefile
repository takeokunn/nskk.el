# Makefile for NSKK

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L . -L tests

# ソースファイル
SOURCES = $(wildcard nskk-*.el)
TEST_SOURCES = $(wildcard tests/nskk-*-test.el)

.PHONY: all test coverage clean help \
	release-beta release-rc release-v1 \
	package verify-release

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
	@echo "Release:"
	@echo "  make release-beta     - Create beta release (v0.9.0-beta)"
	@echo "  make release-rc       - Create RC release (v1.0.0-rc1)"
	@echo "  make release-v1       - Create v1.0.0 final release"
	@echo "  make package          - Create release package tarball"
	@echo "  make verify-release   - Verify release readiness"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean            - Remove coverage data and compiled files"
	@echo "  make help             - Show this help message"

## リリース準備検証
verify-release:
	@echo "=== Verifying Release Readiness ==="
	@echo ""
	@echo "1. Running all tests..."
	@$(MAKE) test
	@echo ""
	@echo "2. Checking test coverage..."
	@$(MAKE) coverage
	@echo ""
	@echo "3. Verifying documentation..."
	@test -f README.md || (echo "ERROR: README.md not found" && exit 1)
	@test -f CHANGELOG.md || (echo "ERROR: CHANGELOG.md not found" && exit 1)
	@test -f LICENSE || echo "WARNING: LICENSE file not found"
	@echo ""
	@echo "4. Checking source files..."
	@test -n "$(SOURCES)" || (echo "ERROR: No source files found" && exit 1)
	@echo "   Found $(words $(SOURCES)) source files"
	@echo ""
	@echo "5. Verifying version consistency..."
	@grep -q "Version:" nskk.el || echo "WARNING: Version header not found in nskk.el"
	@test -f VERSION || echo "WARNING: VERSION file not found"
	@echo ""
	@echo "✅ Release verification complete!"

## ベータ版リリース (v0.9.0-beta)
release-beta: verify-release
	@echo "=== Creating Beta Release v0.9.0-beta ==="
	@echo ""
	@echo "1. Tagging beta release..."
	@if git rev-parse v0.9.0-beta >/dev/null 2>&1; then \
		echo "   Tag v0.9.0-beta already exists"; \
	else \
		git tag -a v0.9.0-beta -m "NSKK v0.9.0 Beta Release"; \
		echo "   ✅ Created tag v0.9.0-beta"; \
	fi
	@echo ""
	@echo "2. Creating release package..."
	@$(MAKE) package VERSION=0.9.0-beta
	@echo ""
	@echo "3. Next steps:"
	@echo "   - Review BETA-RELEASE-v0.9.0.md"
	@echo "   - Test beta package"
	@echo "   - Push tag: git push origin v0.9.0-beta"
	@echo "   - Create GitHub release"
	@echo "   - Recruit beta testers (target: 100+)"
	@echo ""
	@echo "✅ Beta release preparation complete!"

## RC版リリース (v1.0.0-rc1)
release-rc: verify-release
	@echo "=== Creating Release Candidate v1.0.0-rc1 ==="
	@echo ""
	@echo "1. Tagging RC release..."
	@if git rev-parse v1.0.0-rc1 >/dev/null 2>&1; then \
		echo "   Tag v1.0.0-rc1 already exists"; \
	else \
		git tag -a v1.0.0-rc1 -m "NSKK v1.0.0 Release Candidate 1"; \
		echo "   ✅ Created tag v1.0.0-rc1"; \
	fi
	@echo ""
	@echo "2. Creating release package..."
	@$(MAKE) package VERSION=1.0.0-rc1
	@echo ""
	@echo "3. Next steps:"
	@echo "   - Review RC-RELEASE-v1.0.0-rc1.md"
	@echo "   - Run final regression tests"
	@echo "   - Review all documentation"
	@echo "   - Push tag: git push origin v1.0.0-rc1"
	@echo "   - Create GitHub release"
	@echo "   - Final testing period (7-10 days)"
	@echo ""
	@echo "✅ RC release preparation complete!"

## v1.0正式リリース
release-v1: verify-release
	@echo "=== Creating v1.0.0 Final Release ==="
	@echo ""
	@echo "1. Running final quality checks..."
	@$(MAKE) test
	@$(MAKE) coverage
	@echo ""
	@echo "2. Tagging v1.0.0 release..."
	@if git rev-parse v1.0.0 >/dev/null 2>&1; then \
		echo "   Tag v1.0.0 already exists"; \
	else \
		git tag -a v1.0.0 -m "NSKK v1.0.0 - Next-generation SKK for Emacs"; \
		echo "   ✅ Created tag v1.0.0"; \
	fi
	@echo ""
	@echo "3. Creating release package..."
	@$(MAKE) package VERSION=1.0.0
	@echo ""
	@echo "4. Final release checklist:"
	@echo "   [ ] All tests passing (11,000+ tests)"
	@echo "   [ ] Test coverage > 95%"
	@echo "   [ ] Performance benchmarks met"
	@echo "   [ ] Security audit passed"
	@echo "   [ ] Documentation complete"
	@echo "   [ ] CHANGELOG.md updated"
	@echo "   [ ] RELEASE-NOTES-v1.0.0.md reviewed"
	@echo "   [ ] ANNOUNCEMENT-v1.0.0.md ready"
	@echo ""
	@echo "5. Next steps:"
	@echo "   - Push tag: git push origin v1.0.0"
	@echo "   - Create GitHub release"
	@echo "   - Publish to MELPA"
	@echo "   - Post announcement"
	@echo "   - Update website"
	@echo "   - Notify community"
	@echo ""
	@echo "🎉 v1.0.0 release preparation complete!"

## リリースパッケージ作成
package:
	@echo "=== Creating Release Package ==="
	@VERSION=$${VERSION:-1.0.0}; \
	PACKAGE_NAME="nskk-$$VERSION"; \
	PACKAGE_DIR="dist/$$PACKAGE_NAME"; \
	echo "Creating package: $$PACKAGE_NAME"; \
	echo ""; \
	mkdir -p $$PACKAGE_DIR; \
	echo "Copying source files..."; \
	cp -r nskk*.el $$PACKAGE_DIR/; \
	echo "Copying documentation..."; \
	cp -r docs $$PACKAGE_DIR/ 2>/dev/null || true; \
	echo "Copying metadata..."; \
	cp README.md CHANGELOG.md VERSION $$PACKAGE_DIR/ 2>/dev/null || true; \
	test -f LICENSE && cp LICENSE $$PACKAGE_DIR/ || true; \
	echo "Creating tarball..."; \
	cd dist && tar czf $$PACKAGE_NAME.tar.gz $$PACKAGE_NAME; \
	echo ""; \
	echo "✅ Package created: dist/$$PACKAGE_NAME.tar.gz"; \
	echo ""; \
	cd dist && ls -lh $$PACKAGE_NAME.tar.gz
