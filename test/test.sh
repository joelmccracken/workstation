#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
# set -euox pipefail
set -euox pipefail

function assert_input() {
  local label=$1
  local expected=$2
  local actual
  read actual

  if [[ "$expected" == "$actual" ]]; then
    echo "$label is correct"
  else
    echo "$label is not correct, found '$actual', expected '$expected'"
    exit 1
  fi
}

echo "RUNNING TESTS"

# emacs
if which emacs; then
    echo found emacs
else
  echo EMACS NOT FOUND
  exit 1
fi

emacs -Q --batch --eval '(progn (princ emacs-version) (terpri))' | assert_input "emacs version" '27.2'
emacs --batch -l ~/.emacs.d/init.el --eval '(progn (princ doom-version) (terpri))' | assert_input "doom-version" '21.12.0-alpha'
emacs --batch -l ~/.emacs.d/init.el --eval '(progn (princ doom-core-version) (terpri))' | assert_input "doom-core-version" '3.0.0-alpha'

echo "TESTS COMPLETE"
