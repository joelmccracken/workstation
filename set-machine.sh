#!/usr/bin/env bash
set -euo pipefail

if [[ "$@" == "" ]] ; then
    echo "you must provide a machine name"
else
    if [[ "$1" == "glamdring" ]] ; then
      ln -s glamdring.nix this-machine.nix
    else

      if [[ "$1" == "glamdring" ]] ; then
        ln -s gh-runner.nix this-machine.nix
      else
        echo "machine name unrecognized!"
        exit 1
      fi
    fi
fi
