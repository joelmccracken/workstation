#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

cd ~

curl https://raw.githubusercontent.com/joelmccracken/workstation/master/bin/bootstrap-workstation.sh > bootstrap-workstation.sh

echo BEGINNING INITIAL INSTALL

bash bootstrap-workstation.sh
# -bash bin/bootstrap-workstation.sh
echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
