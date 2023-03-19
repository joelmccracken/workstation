#!/usr/bin/env bash
# Passwordless sudo
# Occasionally, sudo is extremely annoying. Having to type "sudo" in the middle of a nix-darwin rebuild really interrupts the flow. So here are a couple of scripts
# to toggle passwordless sudo.

# [[file:../workstation.org::*Passwordless sudo][Passwordless sudo:1]]
set -eo pipefail

if [[ -z "$SUDO_USER" ]]; then
    echo ERROR: run as sudo
    exit 1
fi

TEMPFILE=$(mktemp)

cat > $TEMPFILE <<EOF
$SUDO_USER  ALL=(ALL) NOPASSWD: ALL
EOF

visudo -c $TEMPFILE

mv $TEMPFILE /etc/sudoers.d/me-passwordless-sudo
# Passwordless sudo:1 ends here
