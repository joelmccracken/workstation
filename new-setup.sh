#!/usr/bin/env bash
set -euo pipefail
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew bundle

cd ~
git init
git remote add origin https://github.com/joelmccracken/real-dotfiles.git
git reset --mixed origin/master

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install

git status
