#!/usr/bin/env bash
set -euo pipefail

# set up symlinks
echo ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
