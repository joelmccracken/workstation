#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

echo BEGINNING INITIAL INSTALL

bash bin/bootstrap-workstation.sh

echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
