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

# If I ever have issues w/ this, I can use this form:
# timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
~/.emacs.d/bin/doom -y install

echo FINISHED INSTALLING DOOM
