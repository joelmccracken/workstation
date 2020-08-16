#!/usr/bin/env bash
# WARNING: This file is managed by tangling worksation.org. Do not edit directly!

set -xeuo pipefail

bash link.sh

bash set-machine.sh gh-runner

bash install.sh

echo INSTALL PROCESS COMPLETE, TESTING

bash test/test.sh
