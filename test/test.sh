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

EMACS_VERSION=$(emacs -Q --batch --eval '(print emacs-version)')
echo $EMACS_VERSION | assert_input "emacs version" '"27.2"'

# TODO should i be checking out a specific doom sha so as to not have this problem so often?
foo=$(emacs --batch -l ~/.emacs.d/init.el --eval '(print doom-version)')
echo $foo |  assert_input "doom-version" '"21.12.0-alpha"'

foo2=$(emacs --batch -l ~/.emacs.d/init.el --eval '(print doom-core-version)')
echo $foo2 | assert_input "doom-core-version" '"3.0.0-alpha"'
