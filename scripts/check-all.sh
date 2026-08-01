#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

echo "[1/6] Check parentheses"
./scripts/check-parens.sh
echo "[2/6] Check indentation"
./scripts/check-indent.sh
echo "[3/6] Byte compile"
./scripts/check-byte-compile.sh
echo "[4/6] Checkdoc"
./scripts/check-checkdoc.sh
echo "[5/6] Package lint"
./scripts/check-package-lint.sh
echo "[6/6] ERT"
./scripts/check-ert.sh
echo "All checks passed."
