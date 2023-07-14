#!/usr/bin/env sh

set -xu

LOG_DIR="build-log-$(date | sed 's/ /-/g')"
mkdir -p $LOG_DIR
BUILD_LOG="$LOG_DIR/flake-build.log"
REQUISITES_LOG="$LOG_DIR/requisites.log"
STRAIGHT_LOG="$LOG_DIR/straight-emacs-env.log"

bash bin/home-manager-flake-switch.sh > "$BUILD_LOG" 2>&1
DERIVATION_PATH=$(nix path-info ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage)

nix-store --query --requisites --include-outputs "$DERIVATION_PATH" > "$REQUISITES_LOG"

STRAIGHT_ENV_PATH=$(grep straight-emacs-env requisites.txt | tail -n 1)
nix log "$STRAIGHT_ENV_PATH" > "$STRAIGHT_LOG"
echo "============= STRAIGHT_LOG ==========="
cat "$STRAIGHT_LOG"
echo LOGS STORED TO "$LOG_DIR"

dialog.sh "workstation build finished" &
