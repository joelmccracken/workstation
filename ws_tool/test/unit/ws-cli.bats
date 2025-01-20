setup (){
  load "../test_helper/helper"
    _setup_common
    . "$PROJECT_ROOT/ws_tool/ws"
}

@test "supports help flag" {
    run ws -h
    assert_output --partial 'Usage:'
}

@test "process_cli_args parse verbose flag" {
    process_cli_args -v bootstrap
    assert_equal "$WORKSTATION_VERBOSE" true
}

@test "process_cli_args parse verbose flag (long)" {
    process_cli_args --verbose bootstrap
    assert_equal "$WORKSTATION_VERBOSE" true
}

@test "process_cli_args parse bootstrap subcommand" {
    process_cli_args bootstrap
    assert_equal "$ws_command" "bootstrap"
}

@test "process_cli_args parse bootstrap subcommand with name flag short" {
    process_cli_args -n glamdring bootstrap
    assert_equal "$ws_command" "bootstrap"
    assert_equal "$workstation_name_arg" "glamdring"
}

@test "process_cli_args parse bootstrap subcommand with workstation name long flag" {
    process_cli_args --name aeglos bootstrap
    assert_equal "$ws_command" "bootstrap"
    assert_equal "$workstation_name_arg" "aeglos"
}

@test "process_cli_args parse bootstrap command doctor" {
    process_cli_args doctor
    assert_equal "$ws_command" "doctor"
}

@test "fails with unknown long" {
    run process_cli_args --foo
    assert_failure
    assert_output --partial "unknown argument '--foo'"
}

@test "fails with unknown short" {
    run process_cli_args -z
    assert_failure
    assert_output --partial "ws: argument parsing: unknown argument '-z'"
}
