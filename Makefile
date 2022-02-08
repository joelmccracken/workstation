# [[file:workstation.org::*Makefile][Makefile:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
all: tangle bundle

tangle:
	emacs -l ~/.emacs.d/init.el -l lib/emacs/tangle-file.el --batch --tangle

bundle:
	cd ~
	brew bundle

.PHONY: tangle bundle
# Makefile:1 ends here
