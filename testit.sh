#!/usr/bin/env bash
set -xeuo pipefail

bash link.sh

bash set-machine.sh gh-runner

bash install.sh

echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
