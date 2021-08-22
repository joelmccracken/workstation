#!/usr/bin/env bash
# ensures files that should be in secrets are

set -e

test -e ~/secrets/evernote-personal-auth-token
test -e ~/secrets/evernote_auth_token_docker_env_file
test -e ~/secrets/id_rsa
test -e ~/secrets/id_rsa.pub
test -e ~/secrets/id_rsa_lolboxen
test -e ~/secrets/id_rsa_lolboxen.pub
