workstation_props__common=()
workstation_props__common+=(prop_ws_current_settings_symlink)
workstation_props__common+=(prop_ws_dotfiles_git_track)
workstation_props__common+=(prop_ws_nix_daemon_installed)
workstation_props__common+=(prop_ws_nix_global_config)

workstation_props_ci_ubuntu=()
workstation_props_ci_ubuntu+=("${workstation_props__common[@]}")

workstation_props_ci_macos=()
workstation_props_ci_macos+=("${workstation_props__common[@]}")
