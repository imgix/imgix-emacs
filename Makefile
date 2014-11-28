
EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile imgix.el

clean-elc:
	rm -f imgix.elc

.PHONY:	all test unit compile
