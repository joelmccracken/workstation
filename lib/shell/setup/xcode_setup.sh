#!/usr/bin/env bash

# External Script:

# [[file:../../../workstation.org::*setting up xcode (macos)][setting up xcode (macos):2]]
function xcode_setup() {
    # this will accept the license that xcode requires from the command line
    # and also install xcode if required.
    sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'
}
xcode_setup
# setting up xcode (macos):2 ends here
