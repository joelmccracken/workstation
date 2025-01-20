#!/usr/bin/env bash
# The environment setup script
# To run CI, we have a script which, thankfully, basically mirrors the install instructions.

# Importantly, this does a LOT of things, such as install nix, home-manager, etc, and eventually runs
# the test script.

# [[file:../workstation.org::*The environment setup script][The environment setup script:1]]
set -xeuo pipefail

# env # are there environment variables where I can get the commit sha?

cd $HOME

if [ "$WORKSTATION_VERSION" == "" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$WORKSTATION_VERSION"
fi

curl https://raw.githubusercontent.com/joelmccracken/workstation/$WORKSTATION_BOOTSTRAP_COMMIT/bootstrap-workstation.sh > bootstrap-workstation.sh

echo BEGINNING INITIAL INSTALL

# disable native compilation, too slow for CI
export DOOM_DISABLE_NATIVE_COMPILE=true

if [ "$RUNNER_OS" == "macOS" ]; then
    bash bootstrap-workstation.sh ci-macos $WORKSTATION_BOOTSTRAP_COMMIT
else
    bash bootstrap-workstation.sh ci-ubuntu $WORKSTATION_BOOTSTRAP_COMMIT
fi

echo INSTALL PROCESS COMPLETE, TESTING


pwd
sleep 10
ls -lah
sleep 10
ls -lah $HOME
sleep 10
env
sleep 10
ls -lah $HOME/workstation/
sleep 10
export WORKSTATION_DIR=$HOME/workstation
bash $HOME/workstation/test/test.sh

# The environment setup script:1 ends here
