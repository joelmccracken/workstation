#!/usr/bin/env bash
setup (){
  load "../test_helper/helper"
  _setup_common
  . "$PROJECT_ROOT/ws_tool/lib/logging.bash"
}

@test "log_level_num gets level number from name" {
  export BATS_VERBOSE_RUN=true
  run retfunc log_level_num error
  assert_output --partial 'VAR:REPLY=([0]="4")'
}

@test "logs by log level" {
  WORKSTATION_LOG_LEVEL=debug
  run debug "hello world"
  assert_output --partial 'hello world'
}

@test "skips logs when out of log level" {
  WORKSTATION_LOG_LEVEL=error
  run debug "hello world"
  refute_output --partial 'hello world'
}
