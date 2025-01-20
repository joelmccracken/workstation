#!/usr/bin/env bash
setup (){
    load '../test_helper/helper'
    _setup_common
    ws_reset_settings
}

@test "ws_reset_settings works like i think it does" {
    ORIG_WORKSTATION_DIR="$WORKSTATION_DIR"
    WORKSTATION_DIR=poopy
    assert [ "$WORKSTATION_DIR" = "poopy" ]

    ws_reset_settings

    assert [ "$WORKSTATION_DIR" = "$ORIG_WORKSTATION_DIR" ]
}



@test "settings sets appropriate default values" {
    assert_regex "$WORKSTATION_CONFIG_DIR" .*/.config/workstation
    assert_regex "$WORKSTATION_DIR" .*/.config/workstation/workstation_source
    assert [ "$WORKSTATION_REPO_GIT_ORIGIN" = 'https://github.com/joelmccracken/workstation.git' ]
    assert [ "$WORKSTATION_VERBOSE" = false ]
    assert [ "$WORKSTATION_LOG_LEVEL" = error ]
}

@test "loading settings honors existing env vars" {
    ws_unset_settings
    WORKSTATION_CONFIG_DIR="$(_mktemp "ws-config-dir")"
    WORKSTATION_REPO_GIT_ORIGIN='git@github.com:some-other-user/workstation.git'
    MY_WORKSTATION_REPO_GIT_ORIGIN="$WORKSTATION_REPO_GIT_ORIGIN"
    . "$PROJECT_ROOT/ws_tool/lib/settings.bash"

    assert [ "$WORKSTATION_CONFIG_DIR" = "$WORKSTATION_CONFIG_DIR" ]
    assert [ "$WORKSTATION_DIR" = "$WORKSTATION_CONFIG_DIR/workstation_source" ]
    assert [ "$WORKSTATION_REPO_GIT_ORIGIN" = "$MY_WORKSTATION_REPO_GIT_ORIGIN" ]
    assert [ "$WORKSTATION_VERBOSE" = false ]
    assert [ "$WORKSTATION_LOG_LEVEL" = error ]
}
