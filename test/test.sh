#!/usr/bin/env bash
# test.sh
# At this point in time, this test actually checks very little, but what it DOES check
# is things that indicate that everything went right. Specifically, checking the doom version means
# emacs, doom, and the whole doom setup process worked out.

# I plan to move this to a Haskell project at some point, probably do it with hspec instead.
# Or maybe that bats testing library. We'll see.

# [[file:../workstation.org::*test.sh][test.sh:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
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
# test.sh:1 ends here
