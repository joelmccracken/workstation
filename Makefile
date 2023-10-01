# Makefile
# Used to tangle ~workstation.org~. Tangling refers to the
# process of taking a literate program source and converting it to the target
# "source" file for execution.

# Formerly had some other targets, but now they are OBE. It may make sense to
# delete this makefile if it becomes clearly unnecessary.

# [[file:workstation.org::*Makefile][Makefile:1]]
all: tangle

tangle:
	bash bin/tangle.sh

.PHONY: tangle
# Makefile:1 ends here
