#!/usr/bin/env bash
# The environment setup script
# To run CI, we have a script which, thankfully, basically mirrors the install instructions.

# Importantly, this does a LOT of things, such as install nix, home-manager, etc, and eventually runs
# the test script.

# [[file:../workstation.org::*The environment setup script][The environment setup script:1]]
set -xeuo pipefail

# env # are there environment variables where I can get the commit sha?

cd ~

if [ "$GITHUB_SHA" == "" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$GITHUB_SHA"
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

bash ${WORKSTATION_DIR}/test/test.sh
# The environment setup script:1 ends here
