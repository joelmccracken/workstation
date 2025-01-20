#!/usr/bin/env bash
# test.sh
# At this point in time, this test actually checks very little, but what it DOES check
# is things that indicate that everything went right. Specifically, checking the doom version means
# emacs, doom, and the whole doom setup process worked out.

# I plan to move this to a Haskell project at some point, probably do it with hspec instead.
# Or maybe that bats testing library. We'll see.

# [[file:../workstation.org::*test.sh][test.sh:1]]
set -euox pipefail

set +u
# evaluating this with set -u will cause an unbound variable error
source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
set -u

source ${WORKSTATION_DIR}/lib/shell/foundation.sh

function find_emacs_init() {
  init_file="";
  for x in "$WORKSTATION_EMACS_CONFIG_DIR/early-init.el" "$WORKSTATION_EMACS_CONFIG_DIR/init.el"; do
    if [[ -f "$x" ]]; then
      init_file="$x"
      break;
    fi;
  done;
  if [[ "$init_file" = "" ]]; then
    echo "Error: Could not find emacs init file" 1>&2
    exit 43
  else
    echo "$init_file"
  fi
}

emacs_init="$(find_emacs_init)"

function assert_input() {
  local label=$1
  local expected=$2
  local actual
  read -r actual

  if [[ "$expected" == "$actual" ]]; then
    echo "$label is correct"
  else
    echo "$label is not correct, found '$actual', expected '$expected'"
    exit 1
  fi
}

echo "RUNNING TESTS"

EMACS_PATH=$HOME/.nix-profile/bin/emacs
# emacs
if [ -x "$EMACS_PATH" ]; then
    echo found emacs
else
    echo EMACS NOT FOUND
    exit 1
fi

$EMACS_PATH -Q --batch --eval '(progn (princ emacs-version) (terpri))' | {
  read -r actual
  if [[ "$actual" == "27.1" || "$actual" == "27.2" || "$actual" == "28.1" || "$actual" == "28.2" ]]; then
    echo "emacs version is correct"
  else
    echo "emacs version is not correct, found '$actual', expected 27.1, 27.2, 28.1, or 28.2"
    exit 1
  fi
}

$EMACS_PATH -l "$emacs_init" --batch --eval '(progn (princ doom-version) (terpri))' | {
  read -r actual;
  if [[ "$actual" == "21.12.0-alpha" || "$actual" == "3.0.0-dev" || "$actual" == "3.0.0-pre" ]]; then
    echo "doom version is correct"
  else
    echo "doom version is not correct, found '$actual', expected 21.12.0-alpha, 3.0.0-dev, or 3.0.0-pre"
    exit 1
  fi
}

if $EMACS_PATH -l "$emacs_init" --batch --eval "(progn (require 'vterm-module nil t))"; then
  echo "emacs is able to load vterm-module, so vterm-module is compiled and ready to go";
else
  echo "error: emacs was not able to load vterm-module";
  exit 1
fi

if [ -f $HOME/secrets/test_secret ]; then
    echo "test secret file sucessfully synced"
    cat $HOME/secrets/test_secret
else
    echo "error: test secret file was missing"
fi

echo "TESTS COMPLETE"
# test.sh:1 ends here
