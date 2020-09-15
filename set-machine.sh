#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

if [[ "$@" == "" ]] ; then
    echo "you must provide a machine name"
    exit 1
fi

if [[ "$1" == "glamdring" ]] ; then
  ln -s machines/glamdring.nix this-machine.nix
  exit 0
fi

if [[ "$1" == "gh-runner" ]] ; then
  ln -s machines/gh-runner.nix this-machine.nix
  exit 0
fi

if [[ "$1" == "sesco" ]] ; then
  ln -s machines/sesco.nix this-machine.nix
  exit 0
fi

echo "machine name unrecognized!"
exit 1
