#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

if [ "$1" == "" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$1"
fi

sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'

# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
curl https://raw.githubusercontent.com/joelmccracken/workstation/$WORKSTATION_BOOTSTRAP_COMMIT/Brewfile > ~/Brewfile

# bundle has to happen here because git is required for next step
brew bundle

polite-git-checkout () {
    DIR=$1
    REPO=$2

    cd $DIR
    git init
    git remote add origin $REPO
    git fetch

    # wont work (it will have already been deleted from the index)
    git reset --mixed origin/master
    # This formulation of the checkout command seems to work most reliably
    git status -s | grep -E '^ D' | sed -E 's/^ D //' | xargs -n 1 -- git checkout
}

polite-git-checkout ~ https://github.com/joelmccracken/workstation.git

cd ~

if test -e ~/.emacs.d; then
  mv ~/.emacs.d ~/.emacs.d-$(date +"%s")
fi

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
# install doom; `yes | ` answers yes for all prompts, however when doing this it
# does not reliably exit, it just blocks forever even after the install finishes
# so timeout kills it; currnetly doom install takes like 5 mins, so 10 mins is
# pretty good I think.
# I *believe* timeout is provided by homebrew coreutils
# TODO perhaps I should look into handling situations where doom install
# exits with an error?
timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0

echo FINISHED INSTALLING DOOM
