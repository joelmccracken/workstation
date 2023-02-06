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

ls -lah ~/.emacs.d/

emacs -Q --batch --eval '(progn (princ emacs-version) (terpri))' | {
  read actual
  if [[ "$actual" == "27.1" || "$actual" == "27.2" || "$actual" == "28.1" || "$actual" == "28.2" ]]; then
    echo "emacs version is correct"
  else
    echo "emacs version is not correct, found '$actual', expected 27.1, 27.2, 28.1, or 28.2"
    exit 1
  fi
}


emacs -l ~/.emacs.d/init.el --batch --eval '(progn (princ doom-version) (terpri))' | {
  read actual;
  if [[ "$actual" == "21.12.0-alpha" || "$actual" == "3.0.0-dev" || "$actual" == "3.0.0-pre" ]]; then
    echo "doom version is correct"
  else
    echo "doom version is not correct, found '$actual', expected 21.12.0-alpha, 3.0.0-dev, or 3.0.0-pre"
    exit 1
  fi
}

echo "TESTS COMPLETE"
# test.sh:1 ends here
