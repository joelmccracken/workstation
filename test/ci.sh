#!/usr/bin/env bash
# [[file:../workstation.org::*The environment setup script][The environment setup script:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

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


if [ "$RUNNER_OS" == "macOS" ]; then
    bash bootstrap-workstation.sh ci-macos $WORKSTATION_BOOTSTRAP_COMMIT
else
    bash bootstrap-workstation.sh ci-ubuntu $WORKSTATION_BOOTSTRAP_COMMIT
fi

echo INSTALL PROCESS COMPLETE, TESTING

bash ~/workstation/test/test.sh
# The environment setup script:1 ends here
