#!/usr/bin/env bash
set -euo pipefail

# ln -h

# set up symlinks
mkdir -p $HOME/.config/nixpkgs/
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
