export WORKSTATION_CONFIG_DIR=$HOME/.config/workstation
export WORKSTATION_DIR=$WORKSTATION_CONFIG_DIR/workstation_source

workstation_names=(ci_macos ci_ubuntu);
workstation_descriptions_ci_macos="profile for macos on CI"
workstation_descriptions_ci_ubuntu="profile for ubuntu on CI"

[ -f "${WORKSTATION_CONFIG_DIR}/settings.current.sh" ] && . "${WORKSTATION_CONFIG_DIR}/settings.current.sh" || return 0
