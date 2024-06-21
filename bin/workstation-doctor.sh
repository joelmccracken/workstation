#!/usr/bin/env bash

# TODO idea for basic workstation sanity checking framework
# can KISS instead of making things too complicated


# check settings for current workstation, are
# workstation_name and workstation_dir set? is link at hosts/current?
# is there anything else that needs to be done? can I move anything from test script to here, and also
# invoke this script within the test scripty?
#
# check emacs works
# check secrets setup
# - use a dummy secrete + some sha for checking w.r.t real secrets setup correctly
# - check workstation origin for newer version?
# - see other notes about what to do here, have thought about doing this a million times
# - check somehow if pending migration to run?
# - check if nix stuff needs to be rebuilt?
# - maybe keep some of this stuff in ~/.config
#
#
#
# see here for more ideas https://unix.stackexchange.com/questions/312988/understanding-home-configuration-file-locations-config-and-local-sha#313001
#
# can replace var directory
#
# add standard sanity checking in front of all workstaiton scripts, and have standard bypass
# infra if I want to run script anyway.
# TODO run git diff in ~ and $WORKSTATION_DIR to check for any stray changes.
# TODO someday run reddup also
# TODO someday have this script run automatically and have action items output in
#      notable spots (bitbar, shell input notice)
