# the settings.sh file is used to define basic, global settings.

# location of config dir
# where your personal configuration should be
# (e.g. where this file and config.sh should be)
# the workstation tool needs to know some things to do its job, such as where its
# own file should be found, and the specifics of what the user wants with their
# workstation. Having a central file like this affords a central place to configure
# these things.

# you may wish to customize this if you want your configuration somewhere else.
export WORKSTATION_CONFIG_DIR=$HOME/.config/workstation

# location of workstation source. This is where the ws source code should live.
export WORKSTATION_DIR=$WORKSTATION_CONFIG_DIR/workstation_source

# Workstation name to use.
# Used to identify a machine, determine which settings it should have.
# if you just have one, you can use this here and set it to `default`
# export WORKSTATION_NAME=default

# workstation_names is a regular variable, though it is used
# for configuration. ws uses expected
workstation_names=(aeglos belthronding angrist);

workstation_descriptions_aeglos="parimary laptop"
workstation_descriptions_belthronding="cloud VM"
workstation_descriptions_angrist="work computer"

# at times it is necessary to specify settings differently on different workstations.
# the following can be used to load those workstation-specific settings if such is needed.
# [ -f "settings.${WORKSTATION_NAME}.sh"] && . "settings.${WORKSTATION_NAME}.sh"

# one way to deal with differing sets is having a specific file symlinked to the file
# for the current workstation config. For example, say I have two workstations, picard and janeway:
#   $ touch settings.picard.sh
#   $ touch settings.janeway.sh
# Now, when I am settingh up the "picard" machine, I can do:
#   $ ln -s settings.picard.sh settings.current.sh
#
# And, on the janeway machine, I can do:
#   $ ln -s settings.janeway.sh settings.current.sh
#
# Then, I can have the following in this sesttings file on both, and it will load
# the correct settings on each:
#   $ [ -f "settings.current.sh" ] && . "settings.current.sh"

# Oh yeah, be sure to ignore the settings.current.sh file. you do _not_ want to commit it.
#   $ echo "settings.current.sh" >> .gitignore

[ -f "${WORKSTATION_CONFIG_DIR}/settings.current.sh" ] && . "${WORKSTATION_CONFIG_DIR}/settings.current.sh" || return 0

# Oh, you may wish to add the following to your shell profile file:
# export WORKSTATION_CONFIG_DIR=/path/to/specific/location
# export PATH="${WORKSTATION_DIR}/ws_tool:$PATH"
