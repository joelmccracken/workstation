#!/usr/bin/env bash
set -euo pipefail

bash link.sh

bash set-machine.sh gh-runner

bash install.sh

echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
