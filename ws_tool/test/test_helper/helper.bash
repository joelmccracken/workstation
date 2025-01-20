_setup_common() {
  PROJECT_ROOT="$( cd "$(dirname "${BASH_SOURCE[0]}")/../../../" &>/dev/null && pwd)"
  BATS_LIB_PATH="$PROJECT_ROOT/ws_tool/test/test_helper:$BATS_LIB_PATH"
  bats_load_library "bats-support"
  bats_load_library "bats-assert"

  ws_unset_settings
  # echo "$PROJECT_ROOT, $BATS_TEST_FILENAME" >&3
  PATH="$PROJECT_ROOT:/bin/:${PROJECT_ROOT}/ws_tool:$PATH"
  : "${WORKSTATION_DIR:="$PROJECT_ROOT"}"
  . "$PROJECT_ROOT/ws_tool/lib/lib.bash"
}

set_workstation_version_last_sha() {
  export WORKSTATION_VERSION
  WORKSTATION_VERSION="$(git log -n 1 --format="%H")"
}

retfunc() {
  # use set -o posix
  # plus saving and restoring sets
  # to make set print vars (and not functions)
  local orig_sets
  orig_sets=$(set +o)
  set -o posix
  "$@";
  set | while read -r i; do
      printf "VAR:%s" "$i";
  done
  # declare -p REPLY
  eval "$orig_sets"
}

dump_output() {
    echo "$output" 1>&3
}

ws_get_all_settings() {
   all_settings=()
   while read -r line; do
        if [[ "$line" == export*'META:workstation_setting'* ]]; then
            without_export="${line/#export /}"
            var_name="${without_export/%# META:workstation_setting/}"
            all_settings+=("$var_name")
        fi
   done < "$PROJECT_ROOT/ws_tool/lib/settings.bash"
   read -ra REPLY <<< "${all_settings[@]}"
}

ws_unset_settings() {
    REPLY=()
    ws_get_all_settings
    read -ra all_settings <<< "${REPLY[@]}"
    unset "${all_settings[@]}"
}

ws_reset_settings () {
    ws_unset_settings
    . "$PROJECT_ROOT/ws_tool/lib/settings.bash"
}
