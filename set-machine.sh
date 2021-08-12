#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

if [[ "$@" == "" ]] ; then
    echo "you must provide a machine name"
    exit 1
fi
rm this-machine.nix
ln -s machines/$1.nix this-machine.nix
rm this-machine.el
ln -s machines/$1.el this-machine.el
