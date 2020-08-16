tangle:
	emacs -q -l lib/tangle-file.el --batch --tangle

run:
	echo foo


.PHONY: run tangle
