#!/usr/bin/env bash
set -euo pipefail

setupNix () {
  # https://nixos.org/nix/manual/#sect-macos-installation
  sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
  source $HOME/.nix-profile/etc/profile.d/nix.sh
}

setupHomeManager () {
  # add channels for home manager
  nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
  nix-channel --update
  nix-shell '<home-manager>' -A install
  home-manager switch
}

setupNix
setupHomeManager
bash ./setup-doom.sh
