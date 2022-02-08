#!/usr/bin/env bash
# [[file:../workstation.org::*Passwordless sudo][Passwordless sudo:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
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
