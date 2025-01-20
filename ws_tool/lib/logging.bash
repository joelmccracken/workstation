log_level_num() {
  local lvl_num msg_lvl
  msg_lvl="$1"
  case "$msg_lvl" in
    (emerg)  lvl_num=1;;
    (alert)  lvl_num=2;;
    (crit)   lvl_num=3;;
    (error)  lvl_num=4;;
    (warn)   lvl_num=5;;
    (notice) lvl_num=6;;
    (info)   lvl_num=7;;
    (debug)  lvl_num=8;;
    (*) lvl_num=8;; # default at debug, something is wrong
  esac;
  REPLY=("$lvl_num");
  return 0
}

log() {
  local this_lvl="$1"
  shift;
  REPLY=()
  log_level_num "$WORKSTATION_LOG_LEVEL"
  local global_lvl_num="${REPLY[0]}";
  REPLY=()
  log_level_num "$this_lvl"
  local this_lvl_num="${REPLY[0]}";
  REPLY=()

  if (( this_lvl_num <= global_lvl_num )); then
  local msg
    msg="$this_lvl $(date): $*"
    echo "$msg" 1>&2
  fi
}

function error() {
  log error "$@"
}

function debug() {
  log debug "$@"
}

function info() {
  log info "$@"
}
