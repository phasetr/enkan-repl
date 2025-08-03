# Makefile for claudemacs-client
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

.PHONY: help test compile checkdoc lint check clean install-deps

help: ## Show this help message
	@echo "claudemacs-client Quality Check Commands"
	@echo "========================================"
	@echo ""
	@echo "Available commands:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Setup (run once):"
	@echo "  make install-deps"
	@echo ""
	@echo "Quick start:"
	@echo "  make install-deps && make check"

install-deps: ## Install required dependencies (package-lint)
	$(BATCH) --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) (package-refresh-contents) (package-install 'package-lint))"

test: ## Run ERT tests
	$(BATCH) --eval "(add-to-list 'load-path \".\")" -l claudemacs-client.el -l test/claudemacs-client-test.el -f ert-run-tests-batch-and-exit

compile: ## Byte compile with warnings as errors
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" --eval "(byte-compile-file \"claudemacs-client.el\")"

checkdoc: ## Check documentation format
	$(BATCH) --eval "(checkdoc-file \"claudemacs-client.el\")"

lint: ## Run package-lint checks
	$(BATCH) -l package-lint --eval "(package-lint-file \"claudemacs-client.el\")"

check: test compile checkdoc lint ## Run all quality checks

clean: ## Remove compiled files
	rm -f *.elc