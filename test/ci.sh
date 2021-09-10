#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

env # are there environment variables where I can get the commit sha?

cd ~

if [ "$GITHUB_SHA" == "" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$GITHUB_SHA"
fi

curl https://raw.githubusercontent.com/joelmccracken/workstation/$WORKSTATION_BOOTSTRAP_COMMIT/bin/bootstrap-workstation.sh > bootstrap-workstation.sh

echo BEGINNING INITIAL INSTALL

bash bootstrap-workstation.sh $WORKSTATION_BOOTSTRAP_COMMIT
# -bash bin/bootstrap-workstation.sh
echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
