#!/usr/bin/env bash

# External Script:

# [[file:../../../workstation.org::*install nix configuration file][install nix configuration file:2]]
function emit_nix_conf_content () {
    cat - <<-EOF
# Generated at $(date)
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = https://cache.nixos.org https://cache.iog.io
experimental-features = nix-command flakes
trusted-users = root $(whoami) runner
build-users-group = nixbld
# END OF /etc/nix/nix.conf
EOF
}

function install_system_nix_conf() {
  emit_nix_conf_content | \
      sudo bash -c 'mkdir -p /etc/nix; cat > /etc/nix/nix.conf'
}
install_system_nix_conf
# install nix configuration file:2 ends here
