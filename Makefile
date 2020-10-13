# WARNING: This file is managed by tangling workstation.org. Do not edit directly!
all: tangle switch

tangle:
	emacs -q -l lib/tangle-file.el --batch --tangle

build:
	home-manager build

switch:
	home-manager switch -b old

.PHONY: tangle build switch all
