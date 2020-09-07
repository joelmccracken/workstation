#!/usr/bin/env bash
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
