# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
all: tangle switch

tangle:
	emacs -q -l lib/tangle-file.el --batch --tangle

bundle:
	brew bundle

.PHONY: tangle bundle
