#!/usr/bin/env bash
set -euo pipefail


# emacs
checkEmacs () {
  if which emacs; then
      echo found emacs
      if which emacs | grep nix > /dev/null; then
          echo emacs installed by nix

          EMACS_VERSION=$(emacs -Q --batch --eval '(princ emacs-version)')
          if  [[ "$EMACS_VERSION" == "26.3" ]]; then
              echo emacs is correct version

              DOOM_VERSION=$(emacs --batch -l ~/.emacs.d/init.el --eval '(princ doom-version)')
              if  [[ "$DOOM_VERSION" == "2.0.9" ]]; then
                  echo doom is correct version
              else
                  echo doom is not reported to be correct version, found "$DOOM_VERSION"
              fi
          else
              echo emacs is not correct version, found $EMACS_VERSION
          fi
      else
          echo PROBLEM: emacs installed, but not installed by nix
      fi
  else
    echo EMACS NOT FOUND
    exit 1
  fi
}

checkEmacs
