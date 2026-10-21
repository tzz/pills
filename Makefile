EMACS ?= emacs
EL := pills.el

.PHONY: compile test lint clean

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

test: compile
	$(EMACS) -Q --batch -L . -L test -l test/pills-tests.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q --batch -L . -l checkdoc --eval '(checkdoc-file "$(EL)")'

clean:
	rm -f *.elc test/*.elc
