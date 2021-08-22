#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
set -euox pipefail

echo "RUNNING TESTS"

# emacs
if which emacs; then
    echo found emacs
else
  echo EMACS NOT FOUND
  exit 1
fi

EMACS_VERSION=$(emacs -Q --batch --eval '(princ emacs-version)')
if  [[ "$EMACS_VERSION" == "27.2" ]]; then
    echo emacs is correct version
else
    echo emacs is not correct version, found $EMACS_VERSION
    exit 1
fi

DOOM_VERSION=$(emacs --batch -l ~/.emacs.d/init.el --eval '(princ doom-version)')
if  [[ "$DOOM_VERSION" == "3.0.0-alpha" ]]; then
    echo doom is correct version
else
    echo doom is not reported to be correct version, found "$DOOM_VERSION"
    exit 1
fi
