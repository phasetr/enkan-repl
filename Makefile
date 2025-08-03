# Makefile for claudemacs-client
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

.PHONY: help test compile checkdoc lint check clean install-deps format

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
	$(BATCH) --eval "(progn (package-initialize) (require 'package-lint) (with-temp-buffer (insert-file-contents \"claudemacs-client.el\") (emacs-lisp-mode) (package-lint-current-buffer)))"

format: ## Auto-format elisp files using built-in indent-region (spaces only)
	$(BATCH) --eval "(progn (find-file \"claudemacs-client.el\") (setq-local indent-tabs-mode nil) (untabify (point-min) (point-max)) (mark-whole-buffer) (indent-region (point-min) (point-max)) (save-buffer))"
	$(BATCH) --eval "(progn (find-file \"test/claudemacs-client-test.el\") (setq-local indent-tabs-mode nil) (untabify (point-min) (point-max)) (mark-whole-buffer) (indent-region (point-min) (point-max)) (save-buffer))"

check: test compile checkdoc lint format ## Run all quality checks including formatting

clean: ## Remove compiled files
	rm -f *.elc