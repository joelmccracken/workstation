#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

bash new-setup.sh
bash set-machine.sh gh-runner

bash test/test.sh
