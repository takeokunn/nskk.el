#!/usr/bin/env bash
# nix run のテストスクリプト

set -e

echo "=== Testing 'nix run .' with setter verification ==="
echo ""

# バッチモードでテスト
echo "[Test] Running in batch mode with setter test..."
nix run . -- --batch --eval "(progn \
  (let ((state (nskk-state-create))) \
    (setf (nskk-state-input-buffer state) \"nix-run-test\") \
    (if (string= (nskk-state-input-buffer state) \"nix-run-test\") \
        (message \"✓ SUCCESS: Setter works in 'nix run .'\") \
      (message \"✗ FAIL: Setter returned wrong value\"))))" \
  2>&1 | grep -E "(SUCCESS|FAIL|Error:|void)"

echo ""
echo "If you see '✓ SUCCESS' above, the setter is working!"
echo ""
echo "To test interactively (requires real terminal):"
echo "  nix run ."
echo "  Then: C-x C-j (enable NSKK mode)"
echo "  Then: type 'a' or 'ka'"
echo ""
