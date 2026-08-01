#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHECK_FUNCTION="${1:?check function is required}"
FORM="(progn (load \"$REPO_ROOT/scripts/folio-check.el\" nil t) ($CHECK_FUNCTION))"

if [[ -n "${FOLIO_EMACS_SERVER:-}" ]]; then
  emacsclient --socket-name "$FOLIO_EMACS_SERVER" --eval "$FORM"
else
  emacsclient --eval "$FORM"
fi
