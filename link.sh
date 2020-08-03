#!/usr/bin/env bash
set -euo pipefail

ln -h

# set up symlinks
mkdir -p $HOME/.config/nixpkgs/
echo ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
