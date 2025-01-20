#!/usr/bin/env bash

setup (){
  load '../test_helper/helper'
  _setup_common
  . "$PROJECT_ROOT/ws_tool/lib/properties.bash"
  . "$PROJECT_ROOT/ws_tool/lib/bootstrap_doctor.bash"
}

@test "ensure props handles additional props correctly" {
  props_test_tmp_file=
  declare -a prop_exec_hist

  prop_a() {
    prop_exec_hist+=("a")
    echo "in prop_a"
    echo "a" >> "$props_test_tmp_file"
    REPLY=(additional_props prop_b prop_c)
    return 0
  }

  prop_b() {
    prop_exec_hist+=("b")
    echo "in prop_b"
    echo "b" >> "$props_test_tmp_file"
    return 0
  }

  prop_c() {
    prop_exec_hist+=("c")
    echo "in prop_c"
    echo "c" >> "$props_test_tmp_file"
    return 0
  }

  prop_f() {
    prop_exec_hist+=("f")
    echo "f" >> "$props_test_tmp_file"
    echo "in prop_f"
    return 0
  }

  props_test_tmp_file="$(_mktemp "props-test-tmp")/file"
  echo "0" >> "$props_test_tmp_file"
  prop_exec_hist=(0)
  ws_unset_settings
  run_props --fix true prop_a prop_f

  # tested two ways, just keweping for now bc I have a feeling i'll want to see this in the future
  assert_equal "${prop_exec_hist[*]}" "0 a b c f"
  declare -a 'content=($(cat "${props_test_tmp_file}"))'
  assert_equal "${content[*]}" "0 a b c f"
}

@test "run_all_props runs workstation specific props along with bootstrap" {
  props_test_tmp_file="$(_mktemp "props-test-tmp")/file"
  prop_a() {
    printf " a" >> "$props_test_tmp_file"
    return 0
  }

  prop_b() {
    printf " b" >> "$props_test_tmp_file"
    return 0
  }

  prop_f() {
    printf " f" >> "$props_test_tmp_file"
    return 0
  }

  (
    ws_unset_settings
    bootstrap_props=(prop_a prop_b)
    workstation_props_foo=(prop_f)
    WORKSTATION_NAME=foo
    printf "0" >> "$props_test_tmp_file"
    run_all_props --fix true --label "foo"
  )
  assert_equal "$(cat "${props_test_tmp_file}")" "0 a b f"
}

@test "run_all_props aborts if unable to satisfy a property " {
  local props_test_tmp_file
  props_test_tmp_file="$(_mktemp "props-test-tmp")/file"
  prop_a() {
    printf " a1" >> "$props_test_tmp_file"
    return 0
  }

  prop_b() {
    printf " b1" >> "$props_test_tmp_file"
    return 55
  }

  prop_b_fix() {
    printf " b1_fix" >> "$props_test_tmp_file"
    return 56
  }

  prop_f() {
    printf " f1" >> "$props_test_tmp_file"
    return 0
  }

  ws_unset_settings
  bootstrap_props=(prop_a prop_b)
  workstation_props_foo=(prop_f)
  WORKSTATION_NAME=foo
  printf "0" >> "$props_test_tmp_file"
  run run_all_props --fix true --label "foo"
  # echo "$output" 1>&3
  assert_failure

  assert_equal "$(cat "${props_test_tmp_file}")" "0 a1 b1 b1_fix"
}
