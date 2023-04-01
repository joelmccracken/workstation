#!/usr/bin/env bash

# #+end_src
# External Script:

# [[file:../../../workstation.org::*check if brew is installed][check if brew is installed:2]]
function is_brew_installed() {
    which brew > /dev/null
}
is_brew_installed
# check if brew is installed:2 ends here
