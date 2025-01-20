#!/usr/bin/env bash

. "$WORKSTATION_DIR/ws_tool/lib/lib.bash"
. "$WORKSTATION_DIR/ws_tool/lib/properties/dotfiles.bash"

# writing properties
# for a given property foo, define function
# prop_foo
# that determines if property is fulfilled.
# return code 0 indicates its fulfilled,
# nonzero code indicates property is notfulfilled.
# if propery is not fulfilled,
# prop_foo_fix is executed to
# try to fix/fulfill the property.
# after prop_foo_fix completes, if it has zero exit code,
# assume it worked. run original prop function again to ensure
# if prop does not pass now, exit prop checking and fulfilling cycle
# as fix did not work
#
# propery functions can define that they depend upon other properties by
# setting the REPLY global to an array where the first argument is
# "additional_props" and subsequent arguments are those properties. for example,
# say above property foo should have other props bar and baz, then the following
# value would be appropriate:
#   REPLY=(additional_props prop_bar prop_baz)
# after prop foo returns with a zero exit code, these props are handled next.
# By default, prop_foo would not be checked again after the other props are fulfilled, but
# you could make it do this by for example
#   prop_foo() {
#    REPLY=(additional_props prop_bar prop_baz prop_foo)
#   }
# Note that prop_foo is included at the end of the additional properties list.
# Of course, you wouldn't want this exact example, otherwise it would imply
# that foo would be checked again and again, ad infinitum.

prop_ws_check_initial_tooling_setup()
{
  if is_mac; then
    REPLY=(additional_props \
      prop_ws_check_mac_cli_tools\
      prop_ws_check_mac_homebrew_installed\
      prop_ws_check_mac_git_installed\
      )
    return 0
  else
    if is_linux; then
      REPLY=(additional_props prop_ws_check_linux_git_installed)
      return 0
    else
      # TODO linux is assumed to be debian based
      error "unable to determine workstation system type (mac, linux)"
      return 1
    fi
  fi
}

# check to ensure that xcode cli tools are installed
# this command will tell without itself trying to install them
prop_ws_check_mac_cli_tools () {
  if pkgutil --pkg-info=com.apple.pkg.CLTools_Executables; then
    echo "cli tools package is installed";
    return 0
  else
    echo "cli tools package not installed" 1>&2
    return 1
  fi
}

prop_ws_check_mac_cli_tools_fix () {
  sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'
}

prop_ws_check_mac_homebrew_installed() {
  if which brew > /dev/null; then
    echo "homebrew is installed"
    return 0
  else
    echo "homebrew is not installed" 1>&2;
    return 1
  fi
}

prop_ws_check_mac_homebrew_installed_fix() {
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}

prop_ws_check_mac_git_installed() {
  if which git > /dev/null; then
    echo "git is detected"
  else
    echo "no git is detected"
  fi
}

prop_ws_check_mac_git_installed_fix() {
  brew install git
}

prop_ws_check_linux_git_installed() {
  if which git > /dev/null; then
    echo "git is detected"
  else
    echo "no git is detected"
  fi
}

prop_ws_check_linux_git_installed_fix() {
  sudo bash -c 'apt-get update && apt-get install git'
}

prop_ws_check_workstation_dir() {
  if [ -d "$WORKSTATION_DIR" ]; then
    echo "WORKSTATION_DIR exists"
    if [ -x "$WORKSTATION_DIR/ws_tool/ws" ]; then
      echo "WORKSTATION_DIR contains ws executable"
      return 0
    else
      echo "$WORKSTATION_DIR does not contain the ws tool" 1>&2
      return 2
    fi
  else
    echo "$WORKSTATION_DIR (WORKSTATION_DIR) is absent" 1>&2
    echo "(is workstation installed to a custom location? set WORKSTATION_DIR=path/to/workstation)" 1>&2
    return 1
  fi
}

prop_ws_check_workstation_dir_fix() {
  # TODO this is basically a copy/paste of ws_install.sh
  # somehow figure out another way to do this?
  TMPINST="$(mktemp -d "${TMPDIR:-/tmp}/ws-install-XXXXXXXXX")"
  # installer of ws tool/project
  ( cd "$TMPINST";
    curl -L https://github.com/joelmccracken/workstation/archive/${WORKSTATION_VERSION}.tar.gz | tar zx;

    mkdir -p "$WORKSTATION_DIR";
    mv "${TMPINST}"/workstation-*/{,.[^.]}* "$WORKSTATION_DIR";
  )
}

prop_ws_check_workstation_repo() {
  if [ -d "$WORKSTATION_DIR/.git" ]; then
    echo "WORKSTATION_DIR git directory exists"
    return 0
  else
    echo "$WORKSTATION_DIR/.git directory is absent" 1>&2
    return 1
  fi
}

prop_ws_check_workstation_repo_fix() {
  ( cd "$WORKSTATION_DIR";
    git init .;
    git remote add origin "$WORKSTATION_REPO_GIT_ORIGIN";
    git fetch;
    git reset --mixed "$WORKSTATION_VERSION";
  )
}

: "${WORKSTATION_DOTFILES_TRACK_GIT_DIR:=".git-dotfiles"}"
prop_ws_dotfiles_git_track() {
  if [ -d "$HOME/$WORKSTATION_DOTFILES_TRACK_GIT_DIR" ]; then
    echo "git directory at $HOME/$WORKSTATION_DOTFILES_TRACK_GIT_DIR exists"
    return 0
  else
    echo "git directory at $HOME/$WORKSTATION_DOTFILES_TRACK_GIT_DIR not found" 1>&2
    return 1
  fi
}

prop_ws_dotfiles_git_track_fix() {
  export GIT_DIR="$WORKSTATION_DOTFILES_TRACK_GIT_DIR"
  ( cd "$HOME";
    git init .
    git config --local --get-all core.bare true >/dev/null && \
      git config --local --replace-all core.bare false true
  )
  return 0
}

prop_ws_config_exists() {
  local settings_file="${WORKSTATION_CONFIG_DIR}/settings.sh"
  local config_file="${WORKSTATION_CONFIG_DIR}/config.sh"
  if [[ -f "$settings_file" ]] && [[ -f "$config_file" ]]; then
    echo "found settings and config file exist."
    return 0;
  else
    if ! [[ -f "$settings_file" ]]; then
      echo "ws: bootstrap: prop_ws_config_exists: missing settings file from '$settings_file'" 1>&2
    fi
    if ! [[ -f "$config_file" ]]; then
      echo "ws: bootstrap: prop_ws_config_exists: missing config file from '$config_file'" 1>&2
    fi
    return 1
  fi
}

# depends upon prop_ws_check_workstation_dir
# TODO automate/enforce this somehow?
prop_ws_config_exists_fix() {
  local src_dir="${WORKSTATION_DIR}/ws_tool/sample_config";
  if [[ -n "$workstation_initial_config_dir_arg" ]]; then
    src_dir="$workstation_initial_config_dir_arg";
  fi
  mkdir -p "$WORKSTATION_CONFIG_DIR"

  # hack, because if a relative dir is used for $workstation_initial_config_dir_arg
  # we want it to go back...
  ( cd "$ws_initial_pwd"; cd "$src_dir";
    # not perfect, but not worth making much more complicated
    for f in *; do
      if [[ -e "$WORKSTATION_CONFIG_DIR/$f" ]]; then
        echo "$WORKSTATION_CONFIG_DIR/$f: aleady exists, skipping"
      else
        echo "copying file to $WORKSTATION_CONFIG_DIR/$f"
        cp -r "$f" "$WORKSTATION_CONFIG_DIR/$f";
      fi
    done
  )
}

prop_ws_current_settings_symlink() {
  current_settings_file="$WORKSTATION_CONFIG_DIR/settings.current.sh"

  if [[ -L "$current_settings_file" ]]; then
    echo "symlink found at $current_settings_file"
    return 0
  fi

  if ! [[ -e "$current_settings_file" ]]; then
    echo "no file found at '$current_settings_file'" 1>&2
  fi

  if ! [[ -L "$current_settings_file" ]]; then
    {
      echo "Warning: File found at '$current_settings_file', but it was not a symlink."
      echo "  This will probably work, but its possible that something wonky"
      echo "  has happened."
    } 1>&2
  fi

  return 2
}

# depends upon prop_ws_config_exists
prop_ws_current_settings_symlink_fix() {
  current_settings_file="$WORKSTATION_CONFIG_DIR/settings.current.sh"
  src_settings_file="$WORKSTATION_CONFIG_DIR/settings.${WORKSTATION_NAME}.sh"

  prop_ws_config_exists

  if [[ -e "$current_settings_file" ]] && \
    ! [[ -L "$current_settings_file" ]]; then
    echo "non-symlink file exists at '$current_settings_file'. Cannot automatically fix." 1>&2
    return 10
  fi

  if ! [[ -e "$src_settings_file" ]]; then
    echo "Expecting to find file at '$src_settings_file' to use as symlink src. File not found." 1>&2
    return 3
  fi

  ln -s "$src_settings_file" "$current_settings_file"
}

prop_ws_nix_daemon_installed() {
  if which nix > /dev/null ; then
    echo "nix command found"
    return 0
  else
    echo "nix command not found" 1>&2
    return 1
  fi
}

: "${WORKSTATION_NIX_PM_VERSION:=nix-2.25.3}"
prop_ws_nix_daemon_installed_fix() {
  sh <(curl -L https://releases.nixos.org/nix/$WORKSTATION_NIX_PM_VERSION/install) --daemon;
  # load the needful after installing
  nix_daemon_profile='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  if [[ ! -e "$nix_daemon_profile" ]]; then
    echo "nix installed, but cannot find profile file to load" 1>&2
    return 8
  fi
  . "$nix_daemon_profile";
  ws_nix__restart_daemon
}

ws_nix__restart_daemon() {
  if is_mac; then
    set +e
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
  else
    sudo systemctl restart nix-daemon.service;
  fi
}

ws_nix__conf_filename() {
  printf "/etc/nix/nix.conf"
}

: "${WS_NIX_GLOBAL_CONFIG_LOCATION:=$(ws_nix__conf_filename)}"

ws_nix__global_conf_content() {
  cat <<-EOF
	# BEGIN prop_ws_nix_global_config
	# configuration from ws property prop_ws_nix_global_config
	# AUTOMATICALLY MANAGED: region edits will be overwritten in the future
	trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
	substituters = https://cache.nixos.org https://cache.iog.io
	experimental-features = nix-command flakes
	trusted-users = root $(whoami) runner
	build-users-group = nixbld
	# END prop_ws_nix_global_config
EOF
}

prop_ws_nix_global_config () {
  local conf="$WS_NIX_GLOBAL_CONFIG_LOCATION"
  local begin="# BEGIN prop_ws_nix_global_config"
  local end="# END prop_ws_nix_global_config"

  REPLY=()
  find_bracketed_content "$begin" "$end" < "$conf"
  local parts=("${REPLY[@]}")
  REPLY=()
  if [[ "${parts[1]}" == "$(ws_nix__global_conf_content)"$'\n' ]]; then
    echo "config file at '$conf' is up to date"
    return 0
  else
    echo "config file at '$conf' is out of date";
    return 1
  fi
}

prop_ws_nix_global_config_fix () {
  local conf="$WS_NIX_GLOBAL_CONFIG_LOCATION"
  local begin="# BEGIN prop_ws_nix_global_config"
  local end="# END prop_ws_nix_global_config"

  REPLY=(); find_bracketed_content "$begin" "$end" < "$conf";
  local parts=("${REPLY[@]}"); REPLY=();

  new_conf="$(_mktemp "nix-conf")/nix.conf"

  {
    echo "${parts[0]}";
    ws_nix__global_conf_content;
    echo "${parts[2]}"
  } > "$new_conf"

  # complicated because I don't want to deal with sudo when running tests
  maybe_sudo="bash" # null option (is there some better way?)
  if ! [[ -w "$WS_NIX_GLOBAL_CONFIG_LOCATION" ]]; then
    maybe_sudo="sudo"
  fi
  "$maybe_sudo" "${WORKSTATION_DIR}/ws_tool/bin/safe-overwrite" "$new_conf" "$WS_NIX_GLOBAL_CONFIG_LOCATION"
}

# prop_ws_nix_homemanager_install() {
#   export HOME_MANAGER_BACKUP_EXT
#   HOME_MANAGER_BACKUP_EXT="old-$(date +'%s')"
#   WORKSTATION_HOME_MANAGER_VERSION=0f4e5b4999fd6a42ece5da8a3a2439a50e48e486
#   nix run "home-manager/$WORKSTATION_HOME_MANAGER_VERSION" -- init "$WORKSTATION_DIR"
# }
