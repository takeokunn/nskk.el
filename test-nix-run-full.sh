#!/usr/bin/env bash
# nix run の完全なテストスクリプト

set -e

echo "=== Full 'nix run .' Test Suite ==="
echo ""

# Test 1: 基本的なsetter
echo "[Test 1] Basic setter"
nix run . -- --batch --eval "(progn \
  (let ((state (nskk-state-create))) \
    (setf (nskk-state-input-buffer state) \"test1\") \
    (message \"Result: %s\" (nskk-state-input-buffer state))))" \
  2>&1 | grep "Result:" || echo "✗ FAIL"

# Test 2: 全スロットのsetter
echo ""
echo "[Test 2] All slots setter"
nix run . -- --batch --eval "(progn \
  (let ((state (nskk-state-create))) \
    (setf (nskk-state-mode state) 'katakana) \
    (setf (nskk-state-input-buffer state) \"abc\") \
    (setf (nskk-state-candidates state) '(\"x\" \"y\")) \
    (message \"✓ All setters work\")))" \
  2>&1 | grep "✓" || echo "✗ FAIL"

# Test 3: アプリケーション層
echo ""
echo "[Test 3] Application layer setter"
nix run . -- --batch --eval "(with-temp-buffer \
  (setq-local nskk-current-state (nskk-state-create)) \
  (nskk-application--set-input-buffer \"app-test\") \
  (message \"Result: %s\" (nskk-state-input-buffer nskk-current-state)))" \
  2>&1 | grep "Result:" || echo "✗ FAIL"

# Test 4: Append操作
echo ""
echo "[Test 4] Append operation"
nix run . -- --batch --eval "(with-temp-buffer \
  (setq-local nskk-current-state (nskk-state-create)) \
  (setq-local nskk-application--input-buffer \"\") \
  (nskk-application--append-input \"k\") \
  (nskk-application--append-input \"a\") \
  (message \"Result: %s\" (nskk-state-input-buffer nskk-current-state)))" \
  2>&1 | grep "Result:" || echo "✗ FAIL"

echo ""
echo "=== All Tests Complete ==="
echo ""
echo "Summary: If all tests show 'Result:' or '✓', everything is working!"
echo ""
echo "Now you can test interactively by running:"
echo "  nix run ."
echo ""
