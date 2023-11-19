#!/usr/bin/env bash
# install nix configuration file
# I wish I could do this with a nix-like thing, but sadly, there are several
# complications.
# - for MacOS, this is nix-darwin.
# - for Ubuntu, there is nothing that can do it.
# - There _is_ a way to do something similar with home manager, but it sets the
#   _user_ nix settings, _not_ the system settings. This is not overly surprising,
#   but it does mean that it can't be the sole solution for setting
#   configurations, if you need to set up caches/substituters.
#   At the very least, I would need some *other* way besides home manager to
#   sepecify that my user is a trusted user. But, then, there becomes a question
#   of bootstrapping (nix settings needed before home manager ever runs), so I
#   think its overall easier to just hack a thing with bash.

# [[file:../../../workstation.org::*install nix configuration file][install nix configuration file:1]]
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

emit_nix_conf_content | \
    sudo bash -c 'mkdir -p /etc/nix; cat > /etc/nix/nix.conf'
# install nix configuration file:1 ends here
