#!/usr/bin/env sh


rm -rf $HOME/.config/workstation

unset WORKSTATION_DIR
export WORKSTATION_NAME=angrist
export WORKSTATION_VERSION
WORKSTATION_VERSION="$(git log -n 1 --format="%H")"

bash ws_install.sh

~/.config/workstation/workstation_source/ws_tool/ws bootstrap -n angrist --initial-config-dir ./my_config
