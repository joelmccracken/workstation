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
      -h|--help)
        usage_and_quit 0;
        ;;
      -*|--*)
        echo "Unknown option $i"
        exit 1
        ;;
      *)
        WS_COMMAND="$1";
        shift;
        ;;
    esac
  done
}


process_cli_args "$@"

if [[ "$WS_VERBOSE" == "true" ]]; then
   set -x;
fi

if [[ -f "$WORKSTATION_SETTINGS_FILE" ]] ; then
    echo "settings file exists, loading it";
    source "$WORKSTATION_SETTINGS_FILE";

    if [ -z "$WORKSTATION_NAME" ]; then
        echo "settings file loaded, but WORKSTATION_NAME not set."
    fi

    if [[ -z "$WORKSTATION_NAME" && -z "$WORKSTATION_NAME_ARG" ]]; then
        echo "workstation name unset in settings file and not provided on CLI."
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

echo "bash path: $(which bash) $BASH_VERSION"

exit 0
