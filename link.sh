#!/usr/bin/env bash
# WARNING: This file is managed by tangling worksation.org. Do not edit directly!
set -xeuo pipefail

# set up symlinks
mkdir -p $HOME/.config/nixpkgs/
ln -s `pwd`/home.nix $HOME/.config/nixpkgs/home.nix
