#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail
# Set up nix
# https://nixos.org/nix/manual/#sect-macos-installation
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
source $HOME/.nix-profile/etc/profile.d/nix.sh

# Set up home manager

HOME_MANAGER_VERSION_SHA=a3d691c05308b43afe264a2165f60948cfd2963e

# add channels for home manager
nix-channel --add https://github.com/rycee/home-manager/archive/$HOME_MANAGER_VERSION_SHA.tar.gz home-manager
nix-channel --update

# Install also runs an initial `switch`.
# if any files that home manager replaces already exist, the install process will fail.
# So, using this environment variable here will get install to behave just like the switch command flag -b
# e.g. this: home-manager switch -b old
# Figurd this out via
# https://github.com/nix-community/home-manager/blob/7e5fee4268f53be8758598b7634dff8b3ad8a22b/home-manager/home-manager#L486

export HOME_MANAGER_BACKUP_EXT=old
nix-shell '<home-manager>' -A install
