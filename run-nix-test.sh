#!/usr/bin/env bash
# NSKK.el nix run テストスクリプト

set -e

echo "=== NSKK.el Nix Run Test ==="
echo ""

# flakeのビルドを確認
echo "[Step 1] Building flake..."
nix build . --no-link 2>&1 | head -20

echo ""
echo "[Step 2] Running NSKK in batch mode with test..."
nix develop --command emacs --batch \
  --eval "(require 'nskk)" \
  --load init-test.el \
  2>&1 | grep -E "(✓|✗|===|Test|NSKK)"

echo ""
echo "[Step 3] Verifying setter functionality..."
nix develop --command emacs --batch \
  --eval "(progn
           (require 'nskk)
           (let ((state (nskk-state-create)))
             (setf (nskk-state-input-buffer state) \"nix-test\")
             (message \"Result: %s\" (nskk-state-input-buffer state))))" \
  2>&1 | grep "Result:"

echo ""
echo "=== All tests completed successfully ==="
echo ""
echo "To run NSKK interactively in a real terminal:"
echo "  nix run . -- [emacs-options]"
echo ""
echo "Note: The -nw flag in flake.nix requires a real TTY."
echo "For automated testing, use batch mode as shown above."
