#!/usr/bin/env bash

# prototype of a new workstation tool. goal being an interactive tool that can be used anywhere.
# I'm much better at bash now, and think I can probably accomplish what I need to do.

# ensure macos bash used: (export PATH="/bin:$PATH"; ./ws -v)
# (ensures that any other bash processes will use builtin too)
set -euo pipefail

WS_VERBOSE=false
WS_LOG_LEVEL=error
WORKSTATION_NAME_ARG=
WORKSTATION_NAME=
WS_COMMAND=
WORKSTATION_CONFIG_DIR="$HOME/workstation/hosts/current"
WORKSTATION_SETTINGS_FILE="${WORKSTATION_CONFIG_DIR}/settings.shx"

export WORKSTATION_DIR="$HOME/workstation"
export WORKSTATION_EMACS_CONFIG_DIR=~/.config/emacs
export WORKSTATION_GIT_ORIGIN='git@github.com:joelmccracken/workstation.git'
export WORKSTATION_GIT_ORIGIN_PUB='https://github.com/joelmccracken/workstation.git'
export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current

function info() {
    echo "INFO ========= $(date) $@"
}

function error() {
    echo "ERROR ========= $(date) $@" 1>&2
}

usage_and_quit() {
    print_usage
    exit "$1"
}

print_usage() {
    echo "ws usage:"
    echo " global flags:"
    echo "   -v      | --verbose   : be verbose (i.e. set -x)"
    echo "   -h      | --help      : display this message"
    echo
    echo " commands:"
    echo "   bootstrap             : run the bootstrap process"
    echo "     flags:  "
    echo "       -n NAME | --name NAME : workstation name to use"
    echo "   help                  : display this message"
}

print_workstation_names() {
    echo "Possible workstation names are:"
    echo "  - glamdring     (primary laptop)"
    echo "  - belthronding  (cloud VM)"
    echo "  - aeglos        (work computer)"
}



process_cli_args() {
  for i in "$@"; do
    case $i in
      -v|--verbose)
        WS_VERBOSE=true
        shift;
        ;;
      -n|--name)
        WORKSTATION_NAME_ARG="$2";
        shift; shift;
        ;;
      -h|--help|help)
        usage_and_quit 0;
        ;;
      -*|--*)
        echo "Unknown option $i"
        exit 1
        ;;
      bootstrap)
        WS_COMMAND="$1";
        WORKSTATION_NAME="$2";
        shift; shift;
        ;;
      doctor)
        WS_COMMAND="$1"
    esac
  done

  if [[ "$WS_VERBOSE" == "true" ]]; then
     set -x;
  fi
}

bootstrap_command_setup() {
  if [[ -f "$WORKSTATION_SETTINGS_FILE" ]] ; then
      info "settings file exists, loading it";
      source "$WORKSTATION_SETTINGS_FILE";

      if [ -z "$WORKSTATION_NAME" ]; then
          echo "settings file loaded, but WORKSTATION_NAME not set."
      fi

      if [[ -z "$WORKSTATION_NAME" && -z "$WORKSTATION_NAME_ARG" ]]; then
          echo "workstation name unset in settings file and not provided as argument."
          echo "must provide -n or --name with workstation name."
          print_workstation_names
          usage_and_quit 1
      fi
  else
     echo "settings file does not exist"
     if [[ -z "$WORKSTATION_NAME" && -z "$WORKSTATION_NAME_ARG" ]]; then
          echo "workstation name is unset."
          echo "must provide -n or --name with workstation name."
          print_workstation_names
          usage_and_quit 1
      fi
  fi
}


props_ws_settings_file_check() {
   if [[ -f "$WORKSTATION_SETTINGS_FILE" ]] ; then
      echo "settings file exists";
      echo "no settings file found (expected at $WORKSTATION_SETTINGS_FILE)" 2>&1
   fi
}


boostrap_command() {
  echo BOOTSTRAP HERE:
}








process_cli_args "$@"

case "$WS_COMMAND" in
  bootstrap) bootstrap_command;;
  doctor) doctor_command;;
  *) error "unknown command $WS_COMMAND; how did we get here?"
esac





# # Script should be passed a single argument, which is name of this workstation.

# # When using script to set up a workstation, the "name" of the workstation should
# # be provided as the first argument. This is used to pick which settings should be
# # applied to this machine.
# if [ -z "${1+x}" ]; then
#     echo WORKSTATION_NAME must be provided as first argument
#     exit 2
# else
#     export WORKSTATION_NAME="$1"
# fi

# # This argument generally should not be used by the user, but it is needed for
# # the CI process.
# # When the CI process starts, we start out with a check out of the code for this
# # commit in a directory on the CI machine. However, this is not how workstation runs:
# # - part of the job of workstation is getting its own code from the server
# # - workstation expects the code to be in a specific directory, that is, ~/workstation
# # Because of this (and possibly other reasons that escape me now), even though the
# # source code of the current commit is checked out on the CI machine already,
# # the CI process re-downloads the code (via this script). The specific SHA to get
# # is passed via the argument below. However, if actually being used by a user,
# # generally user will always want to use the most up to date content of the master
# # branch, so this can be ignored.
# # I think probably this sha should just be passed in as an environment variable
# # instead of a CLI argument, as that seems a bit less confusing to me.
# if [ -z "${2+x}" ]; then
#     export WORKSTATION_BOOTSTRAP_COMMIT=origin/master
# else
#     export WORKSTATION_BOOTSTRAP_COMMIT="$2"
# fi
# # [[file:../../workstation.org::workstation_foundation][workstation_foundation]]

# export WORKSTATION_DIR="$HOME/workstation"
# export WORKSTATION_EMACS_CONFIG_DIR=~/.config/emacs
# export WORKSTATION_GIT_ORIGIN='git@github.com:joelmccracken/workstation.git'
# export WORKSTATION_GIT_ORIGIN_PUB='https://github.com/joelmccracken/workstation.git'
# export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current

# sourceIfExists () {
#     if [ -f "$1" ]; then
#         source "$1"
#     fi
# }

# if [ -z "${WORKSTATION_NAME+x}" ] ; then
#     sourceIfExists "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh"
# fi


# if [ -z "${WORKSTATION_NAME+x}" ] ; then
#     echo WARNING: no environment variable WORKSTATION_NAME provided.
#     echo This variable should be exported by a script at:
#     echo $WORKSTATION_DIR/hosts/current/settings.sh
#     echo see workstation.org for more information
# else
#     export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME
# fi
# # workstation_foundation ends here
# # These are the various versions of things that should be installed. Keeping them
# # in one place like this make them easier to keep track of.
# # [[file:../../../workstation.org::workstation_setup_versions][workstation_setup_versions]]
# export WORKSTATION_NIX_PM_VERSION=nix-2.11.1
# export WORKSTATION_NIX_DARWIN_VERSION=f6648ca0698d1611d7eadfa72b122252b833f86c
# export WORKSTATION_HOME_MANAGER_VERSION=0f4e5b4999fd6a42ece5da8a3a2439a50e48e486
# # workstation_setup_versions ends here
# # hereafter, we use many helper functions. Here they are defined up front,
# # as some of them are used throughout the other code.

# # [[file:workstation.org::is_mac_function][is_mac_function]]
# function is_mac() {
#     [[ "$(uname)" == 'Darwin' ]]
# }
# # is_mac_function ends here

# # [[file:workstation.org::is_linux_function][is_linux_function]]
# function is_linux() {
#     [[ "$(uname)" == 'Linux' ]]
# }
# # is_linux_function ends here

# # [[file:workstation.org::info_function][info_function]]
# function info() {
#     echo "INFO ========= $(date) $@"
# }
# # info_function ends here

# # [[file:workstation.org::polite_git_checkout_function][polite_git_checkout_function]]
# function polite-git-checkout () {
#     DIR=$1
#     REPO=$2
#     ORIGIN=$3

#     cd $DIR
#     git init
#     git remote add origin $REPO
#     git fetch

#     # wont work (it will have already been deleted from the index)
#     git reset --mixed origin/master
#     # This formulation of the checkout command seems to work most reliably
#     git status -s | grep -E '^ D' | sed -E 's/^ D //' | xargs -n 1 -- git checkout
#     # fixing; used public to start, but want to be able to push
#     git remote set-url origin $ORIGIN
# }
# # polite_git_checkout_function ends here

# # [[file:workstation.org::mv_dated_backup_function][mv_dated_backup_function]]
# function mv_dated_backup() {
#     local THEDIR="$1"
#     if test -e "$THEDIR"; then
#         mv "$THEDIR" "${THEDIR}-$(date +"%s")"
#     fi
# }
# # mv_dated_backup_function ends here

# # [[file:workstation.org::is_git_repo_cloned_at_function][is_git_repo_cloned_at_function]]
# function is_git_repo_cloned_at(){
#     cd $1 && [[ "$(git remote get-url origin)" == "$2" ]]
# }
# # is_git_repo_cloned_at_function ends here

# # [[file:workstation.org::clone_repo_and_checkout_at_function][clone_repo_and_checkout_at_function]]
# function clone_repo_and_checkout_at() {
#     mv_dated_backup $1
#     info cloning repo into $1
#     git clone $2 $1
#     cd $1
#     info checking out commit $3
#     git checkout $3
#     info setting origin
#     git remote set-url origin $4
# }
# # clone_repo_and_checkout_at_function ends here

# # [[file:workstation.org::xcode_setup_function][xcode_setup_function]]
# function xcode_setup() {
#     # this will accept the license that xcode requires from the command line
#     # and also install xcode if required.
#     sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'
# }
# # xcode_setup_function ends here

# # [[file:workstation.org::is_brew_installed_function][is_brew_installed_function]]
# function is_brew_installed() {
#     which brew > /dev/null
# }
# # is_brew_installed_function ends here

# # [[file:workstation.org::homebrew_setup_function][homebrew_setup_function]]
# function homebrew_setup() {
#     /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# }
# # homebrew_setup_function ends here

# # [[file:workstation.org::update_apt_install_git_function][update_apt_install_git_function]]
# function update_apt_install_git() {
#     sudo bash -c 'apt-get update && apt-get install git'
# }
# # update_apt_install_git_function ends here

# # [[file:workstation.org::*Bootstraping Script][is_git_repo_cloned_at_function]]
# function is_git_repo_cloned_at(){
#     cd $1 && [[ "$(git remote get-url origin)" == "$2" ]]
# }
# # is_git_repo_cloned_at_function ends here

# # [[file:workstation.org::*Bootstraping Script][clone_repo_and_checkout_at_function]]
# function clone_repo_and_checkout_at() {
#     mv_dated_backup $1
#     info cloning repo into $1
#     git clone $2 $1
#     cd $1
#     info checking out commit $3
#     git checkout $3
#     info setting origin
#     git remote set-url origin $4
# }
# # clone_repo_and_checkout_at_function ends here
# info starting workstation bootstrap
# is_mac && {
#     info ensuring xcode is installed
#     xcode_setup
#     info finished ensuring xcode is installed

#     info ensuring brew is installed
#     if ! is_brew_installed; then
#         homebrew_setup
#     fi
#     info finished ensuring brew is installed

#     info installing git
#     brew install git
#     info finished installing git

# }
# is_linux && {
#     info updating apt, installing git
#     update_apt_install_git
#     info finished updating apt, installing git
# }
# is_git_repo_cloned_at $WORKSTATION_DIR $WORKSTATION_GIT_ORIGIN || {
#     clone_repo_and_checkout_at $WORKSTATION_DIR $WORKSTATION_GIT_ORIGIN_PUB \
#         $WORKSTATION_BOOTSTRAP_COMMIT $WORKSTATION_GIT_ORIGIN
# }
# # at this point, this is hardly necessary; however, the gitignore file is handy
# # i may explore getting rid of this repo entirely and just having a fresh
# # repo without any origin in ~
# info ensuring dotfiles repo is checked out

# DOTFILES_ORIGIN='git@github.com:joelmccracken/dotfiles.git'

# is_git_repo_cloned_at ~ "$DOTFILES_ORIGIN" ||
#     polite-git-checkout ~ 'https://github.com/joelmccracken/dotfiles.git' \
#         "$DOTFILES_ORIGIN"

# info finished ensuring dotfiles repo is checked out
# # each workstaion host I use has different settings needs.
# # For example, my remote cloud hosted server has a different setup than
# # my mac laptop, which has a different set up from my work computer.
# # the way I have these settings specified is by having a directory in my home
# # directory which has all of the needed files I would need for such differences.
# # there are different directories for each host I maintain, but on a given host,
# # one of those directories are symlinked into 'current' host, which other things
# # can then refer to

# export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME

# info setting current host settings directory...
# info workstation host settings directory: $WORKSTATION_HOST_SETTINGS_SRC_DIR

# if [ -d $WORKSTATION_HOST_SETTINGS_SRC_DIR ]; then
#     info setting current host directory to $WORKSTATION_HOST_SETTINGS_SRC_DIR;
#     ln -s $WORKSTATION_HOST_SETTINGS_SRC_DIR $WORKSTATION_HOST_CURRENT_SETTINGS_DIR;
# else
#     echo ERROR $WORKSTATION_HOST_SETTINGS_SRC_DIR does not exist, must exit
#     exit 5
# fi

# info ensuring nix is installed
# ~/workstation/lib/shell/setup/ensure_nix_installed.sh

# info finished ensuring nix is installed

# info setting up nix.conf
# ~/workstation/lib/shell/setup/install_system_nix_conf.sh

# info restarting nix daemon
# ~/workstation/lib/shell/setup/restart_nix_daemon.sh
# info nix daemon restarted

# NIX_DAEMON_PATH='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
# set +u
# source "$NIX_DAEMON_PATH";
# set -u

# is_mac && {
#     info installing darwin-nix
#     ~/workstation/lib/shell/setup/install_nix_darwin.sh
#     info finished installing darwin-nix
# }


# ~/workstation/lib/shell/setup/install_home_manager.sh

# ~/workstation/lib/shell/setup/home-manager-flake-switch.sh

# set +u
# # evaluating this with set -u will cause an unbound variable error
# source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
# set -u

# ~/workstation/lib/shell/setup/install_doom_emacs_no_nix.sh
# info linking dotfiles that should be symlinked
# bash ~/workstation/lib/shell/setup/link-dotfiles.sh -f -c
# info finished linking dotfiles
# info "building the 'ws' script"
# ~/workstation/lib/shell/setup/build_ws_tool.sh

# info "running the 'ws install' process"
# ~/workstation/lib/shell/setup/ws_install.sh
# info "'ws install' process completed"

# info linking dotfiles that should be symlinked
# bash ~/workstation/lib/shell/setup/link-dotfiles.sh -f -c
# info finished linking dotfiles
# bash ~/workstation/lib/shell/setup/initial_bitwarden_sync.sh

# cat <<-EOF
# Success! However, there are some remaining manual set up steps required.
# # [[file:workstation.org::manual-setup-instructions][manual-setup-instructions]]
# There are unfortunately a number of things need to install and set up
# manually:
# - lastpass firefox extension
# - vimium-ff etension
# - dropbox
# - icloud
# - slack
# - spotify
# - install haskell language server in ~/bin (or somwewhere else?) for hls

# These are the settings I use for slack:
# - accessibility then at bottom changbe up arrow to move focus to last message
# - advanced
#   - when in markdown block backticks, enter should do a newline
#   - format messages with markup

# mac settings
# - enable screen sharing, _not_ remote management
# - enable remote login
# - configure hammerspoon
#   - open it
#   - enable accessability settings
#   - launch at login
# # manual-setup-instructions ends here
# EOF
# # Bootstraping Script:1 ends here
