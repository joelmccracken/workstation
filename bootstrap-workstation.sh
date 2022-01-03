#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

if [ "$1" == "" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$1"
fi

WS_DIR="$HOME/workstation"

# TODO should be naturally idempontent
sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'

# TODO first check to see if brew is installed
# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# install git, necessary for next step
# TODO this should be safe to run even if its been run before
brew install git

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

function mv_dir_dated_backup() {
    local THEDIR="$1"
    if test -e "$THEDIR"; then
        mv "$THEDIR" "${THEDIR}-$(date +"%s")"
    fi
}

cd ~

# TODO first check to see if origin etc is correct?
polite-git-checkout ~ https://github.com/joelmccracken/dotfiles.git

brew bundle

# TODO check first to see if the desired contents have correct git
#      remote for origin etc. Can manually fix if this check is not right
mv_dir_dated_backup ~/workstation
git clone https://github.com/joelmccracken/workstation.git

# TODO is there some way to see if doom is already installed??
mv_dir_dated_backup ~/.emacs.d
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d

# If I ever have issues w/ this, I can use this form:
# timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
~/.emacs.d/bin/doom -y install

echo FINISHED INSTALLING DOOM

echo installing nix

# TODO see if nix is already installed
sh <(curl -L https://nixos.org/nix/install)

NIX_DAEMON_PATH='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

if [ -e "$NIX_DAEMON_PATH" ]; then
  (source "$NIX_DAEMON_PATH") || exit 0
fi

# TODO if stack is already installed, dont run this
curl -sSL https://get.haskellstack.org/ | sh
