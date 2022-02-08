# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
all: tangle bundle

tangle:
	emacs -q -l lib/emacs/tangle-file.el --batch --tangle

bundle:
	cd ~
	brew bundle

.PHONY: tangle bundle
