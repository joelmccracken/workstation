#!/usr/bin/env bash

setup (){
    load '../test_helper/helper'
    _setup_common
}

@test "runs ws_install" {
    export WORKSTATION_DIR="$(_mktemp "ws-install")"
    set_workstation_version_last_sha
    # export TO_WORKSTATION_DIR="${TMPDIR}/workstation"
    run ws_install.sh
    assert [ -x "${WORKSTATION_DIR}/ws_tool/ws" ]
}
