#!/usr/bin/env bash
set -euo pipefail

ln --help

# set up symlinks
mkdir -p $HOME/.config/nixpkgs/home.nix
echo ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
