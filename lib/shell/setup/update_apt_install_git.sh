#!/usr/bin/env bash

# External Script:

# [[file:../../../workstation.org::*update apt and install git][update apt and install git:2]]
function update_apt_install_git() {
    sudo bash -c 'apt-get update && apt-get install git'
}
update_apt_install_git
# update apt and install git:2 ends here
