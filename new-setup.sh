#!/usr/bin/env bash
set -xeuo pipefail
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew bundle

cd ~
git init
git remote add origin https://github.com/joelmccracken/workstation.git
git fetch
git reset --soft origin/master

cd apps/
git checkout Unmount\ TimeMachine.app
cd ..

git checkout $(git status -s | grep -E ' D' | awk '{print $2}')

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install

git remote set-url origin git@github.com:joelmccracken/workstation.git

git status
