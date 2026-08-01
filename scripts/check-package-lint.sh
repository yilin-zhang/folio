#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACKAGE_LINT_COMMIT="1c37329703a507fa357302cf6fc29d4f2fe631a8"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT
PACKAGE_LINT_DIR="$TMP_DIR/package-lint"

git clone --quiet --filter=blob:none --no-checkout \
  https://github.com/purcell/package-lint.git "$PACKAGE_LINT_DIR"
git -C "$PACKAGE_LINT_DIR" fetch --quiet --depth 1 origin "$PACKAGE_LINT_COMMIT"
git -C "$PACKAGE_LINT_DIR" checkout --quiet --detach FETCH_HEAD

FORM="(progn
     (add-to-list 'load-path \"$PACKAGE_LINT_DIR\")
     (require 'package-lint)
     (with-temp-buffer
       (insert-file-contents \"$REPO_ROOT/folio.el\")
       (emacs-lisp-mode)
       (let ((issues (package-lint-buffer)))
         (if issues
             (error \"Package lint failed: %S\" issues)
           \"Package lint passed.\"))))"

if [[ -n "${FOLIO_EMACS_SERVER:-}" ]]; then
  emacsclient --socket-name "$FOLIO_EMACS_SERVER" --eval "$FORM"
else
  emacsclient --eval "$FORM"
fi
