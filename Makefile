MAKE_ := $(MAKE) -j1 --no-print-directory

EMACS ?= emacs
FUNC =

.PHONY: emacs-batch md

emacs-batch:
	@echo ""
	@echo "$(ORG_FILE) ::"
	@env HOME=$(shell pwd)/../ $(EMACS) -l $(shell pwd)/init.el --batch \
		-f $(FUNC) \
		--kill

init:
	@$(MAKE_) emacs-batch FUNC=emacs-version
