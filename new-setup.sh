#!/usr/bin/env bash
set -xeuo pipefail
cd ~

# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew bundle

# restore dotfiles
bash ~/bin/polite-git-checkout ~ https://github.com/joelmccracken/workstation.git
echo FINISHED CHCKING OUT DOTFILES

# update remote to one that can be pushed to
# (I first use the https version so that i can do the checkout even if ssh keys
# are missing on this machine)
git remote set-url origin git@github.com:joelmccracken/workstation.git

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
# install doom; `yes | ` answers yes for all prompts, however when doing this it
# does not reliably exit, it just blocks forever even after the install finishes
# so timeout kills it; currnetly doom install takes like 5 mins, so 10 mins is
# pretty good I think.
# I *believe* timeout is provided by homebrew coreutils
# TODO perhaps I should look into handling situations where doom install
# exits with an error?
timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0

echo DOOM INSTALL FINISHED

git status
