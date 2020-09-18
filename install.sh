#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail
# Set up nix
# https://nixos.org/nix/manual/#sect-macos-installation
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
source $HOME/.nix-profile/etc/profile.d/nix.sh

# Set up home manager

# add channels for home manager
nix-channel --add https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz home-manager
nix-channel --update

# if any files that home manager replaces already exist, the install process will fail
# using environment variable here will get install to behave just like the switch command flag -b
export HOME_MANAGER_BACKUP_EXT=old
nix-shell '<home-manager>' -A install

# home-manager switch -b old
