#!/usr/bin/env bash
set -euo pipefail

# set up symlinks
ln -s $HOME/.config/nixpkgs/home.nix `pwd`/home.nix
