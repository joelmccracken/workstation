#!/usr/bin/env bash

# prototype of a new workstation tool. goal being an interactive tool that can be used anywhere.
# I'm much better at bash now, and think I can probably accomplish what I need to do.

# ensure macos bash used: (export PATH="/bin:$PATH"; ./ws -v)
# (ensures that any other bash processes will use builtin too)
# set -euo pipefail

ws_script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

ws_initial_pwd="$PWD"

# set -x

. "${ws_script_dir}/lib/settings.bash"
. "${ws_script_dir}/lib/logging.bash"
. "${ws_script_dir}/lib/properties.bash"
. "${ws_script_dir}/lib/bootstrap_doctor.bash"

REPLY=() # global "out" var, hack to use return values
ws_command=help # show help if nothing provided
declare -a ws_command_arguments
workstation_initial_config_dir_arg=
workstation_name_arg=
workstation_interactive=

usage_and_quit() {
    print_usage
    exit "$1"
}

load_if_exists() {
  if [ -f "$1" ]; then
    . "$1"
  fi
}

load_expected() {
  if [ -f "$1" ]; then
    . "$1"
  else
    error "ws: init: expected to load file $1, but no file found"
  fi
}

print_usage() {
  echo "Workstation configuration tool."
  echo
  echo "Usage:"
  echo "  ws [options] bootstrap"
  echo "  ws [options] doctor"
  echo
  echo "Subcommands:"
  echo "  bootstrap     : run the bootstrap process"
  echo "  doctor        : Run checks on local setup"
  echo "  help          : display this message"
  echo
  echo "Options:"
  echo "-h --help                : Display this message and exit"
  echo "-v -verbose              : Be verbose"
  echo "-n NAME, --name NAME     : Specify the name of this workstation."
  echo "-i, --interactive        : Interactive mode."
  echo "-c, --initial-config-dir : Initial configuration directory."
  echo "       If user already has a workstation configuration, "
  echo "       specifies location for tool to use it."
  echo "       Installs this configuration at default configuration location."
}

process_cli_args() {
  ws_command_arguments=("$@")
  local args=("$@")
  local i;                      #
  for ((i=0;i < ${#args[@]}; i++)); do
    debug "iter:$i ; current:${args[i]} ; max:${#args[@]}"
    local current="${args[i]}"

    case "$current" in
      (-v|--verbose)
        WORKSTATION_VERBOSE=true
        WORKSTATION_LOG_LEVEL=info
        ;;
      (-n|--name)
        workstation_name_arg="${args[i+1]}";
        (( i+=1 ));
        ;;
      (-c|--initial-config-dir)
        workstation_initial_config_dir_arg="${args[i+1]}";
        (( i+=1 ));
        ;;
      (-h|--help|help)
        usage_and_quit 0;
        ;;
      (-i|--interactive)
        workstation_interactive=true;
        ;;
      (bootstrap)
        ws_command="$current";
        ;;
      (doctor)
        ws_command="$current";
        ;;
      (*)
        error "ws: argument parsing: unknown argument '$current'";
        exit 10;
        ;;
    esac
  done

  return 0
}

help_command() {
  usage_and_quit 0;
}

ws_main() {
  process_cli_args "$@"
  [ -n "$workstation_name_arg" ] && WORKSTATION_NAME="$workstation_name_arg"

  if [ "$WORKSTATION_VERBOSE" = "true" ]; then
    set -x
  fi

  if [[ -n "$workstation_initial_config_dir_arg" ]]; then
    load_expected "$workstation_initial_config_dir_arg/settings.sh"
    load_expected "$workstation_initial_config_dir_arg/config.sh"
  fi

  if [[ -d "$(ws_config_dir_default)" ]]; then
    load_expected "$(ws_config_dir_default)/settings.sh"
    load_expected "$(ws_config_dir_default)/config.sh"
  fi

  case "$ws_command" in
    (bootstrap) bootstrap_command;;
    (doctor) doctor_command;;
    ("help") help_command;;
    (*) error "unknown command $ws_command; how did we get here?"
  esac
}

# if being run directly, run main
(return 0 2>/dev/null) || ws_main "$@"

