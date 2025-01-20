#!/usr/bin/env bash

set -xeuo pipefail

rm -rf $HOME/.config/workstation

unset WORKSTATION_DIR

bash <(curl "https://raw.githubusercontent.com/joelmccracken/workstation/${WORKSTATION_VERSION}/ws_tool/ws_install.sh")

cd ~/.config/workstation/workstation_source/ws_tool

if [ "$RUNNER_OS" == "macOS" ]; then
    WORKSTATION_NAME=ci_macos
else
    WORKSTATION_NAME=ci_ubuntu
fi

./ws bootstrap -n "$WORKSTATION_NAME" --initial-config-dir ./test/test_config
