#!/usr/bin/env bash
set -euo pipefail

if [[ "${1:-}" == "--fix" ]]; then
  REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
  FORM="(progn (load \"$REPO_ROOT/scripts/folio-check.el\" nil t) (folio-check-indent t))"
  if [[ -n "${FOLIO_EMACS_SERVER:-}" ]]; then
    emacsclient --socket-name "$FOLIO_EMACS_SERVER" --eval "$FORM"
  else
    emacsclient --eval "$FORM"
  fi
  exit
fi

exec "$(dirname "${BASH_SOURCE[0]}")/run-check.sh" folio-check-indent
