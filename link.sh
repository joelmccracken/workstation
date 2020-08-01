#!/usr/bin/env bash
set -euo pipefail

# set up symlinks
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
