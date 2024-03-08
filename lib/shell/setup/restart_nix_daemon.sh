#!/usr/bin/env bash
# restart nix daemons
# Sometimes we need to restart the nix daemons, e.g. after editing the nix config
# file.

# [[file:../../../workstation.org::*restart nix daemons][restart nix daemons:1]]
source ~/workstation/lib/shell/funcs.sh
function restart_nix_daemon_linux() {
    sudo systemctl restart nix-daemon.service;
}

function restart_nix_daemon_mac() {
    set +e
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
}

if is_mac; then  restart_nix_daemon_mac; fi
if is_linux; then restart_nix_daemon_linux; fi
# restart nix daemons:1 ends here
