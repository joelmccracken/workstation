#!/usr/bin/env bash
set -xeuo pipefail
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew bundle

cd ~
git init
git remote add origin https://github.com/joelmccracken/workstation.git
git fetch
git reset --mixed origin/master
git status -s | grep -E ' D' | sed -E 's/^ D //' | xargs -n 1 -- git checkout

git remote set-url origin git@github.com:joelmccracken/workstation.git

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install

git status
