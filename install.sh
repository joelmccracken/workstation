#!/usr/bin/env bash
set -xeuo pipefail
# https://nixos.org/nix/manual/#sect-macos-installation
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
source $HOME/.nix-profile/etc/profile.d/nix.sh
setupHomeManager () {
  # add channels for home manager
  nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
  nix-channel --update

  export HOME_MANAGER_BACKUP_EXT=old
  nix-shell '<home-manager>' -A install || {
    echo "first home manager install failed, but that is expected."
  }
  home-manager switch -b old
}
source ./setup-doom.sh
