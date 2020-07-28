#!/usr/bin/env bash
set -euo pipefail

if [[ "$1" == "glamdring" ]] ; then
    ln -s glamdring.nix this-machine.nix
fi
