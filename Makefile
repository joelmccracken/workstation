tangle:
	emacs -q -l lib/tangle-file.el --batch --tangle

build:
	home-manager build

switch:
	home-manager build -b old

.PHONY: tangle build switch
