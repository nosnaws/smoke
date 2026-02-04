.PHONY: all build install clean deps repl test status push pull amend eval

PREFIX ?= /usr/local/bin
SBCL := sbcl

# Default target
all: build

# Install dependencies via quicklisp
deps:
	$(SBCL) --non-interactive \
		--eval '(ql:quickload (quote (:alexandria :serapeum :str :cl-json :cl-ppcre)) :silent t)'

# Build the executable
build: deps
	$(SBCL) --non-interactive --load build.lisp

# Install to PREFIX
install: build
	install -m 755 smoke $(PREFIX)/smoke

# Clean build artifacts
clean:
	rm -f smoke
	rm -rf ~/.cache/common-lisp/*/$(shell pwd)

# Start a REPL with the system loaded
repl:
	$(SBCL) \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(in-package :smoke)'

# Run tests
test:
	$(SBCL) --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(format t "All systems loaded successfully.~%")'

#
# Development commands - run smoke commands via eval
#

# Show stack status
status:
	@$(SBCL) --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(smoke:status)'

# Push PRs
push:
	@$(SBCL) --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(smoke:push-stack)'

# Pull and rebase
pull:
	@$(SBCL) --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(smoke:pull-stack)'

# Interactive amend
amend:
	@$(SBCL) \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '(smoke:amend-stack)'

# Generic eval - usage: make eval EXPR='(smoke:status)'
eval:
	@$(SBCL) --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :smoke :silent t)' \
		--eval '$(EXPR)'

# Help
help:
	@echo "Smoke - Stacked Diffs CLI"
	@echo ""
	@echo "Build targets:"
	@echo "  make deps     - Install dependencies"
	@echo "  make build    - Build executable"
	@echo "  make install  - Install to $(PREFIX)"
	@echo "  make clean    - Remove build artifacts"
	@echo "  make repl     - Start REPL with smoke loaded"
	@echo "  make test     - Load and verify system"
	@echo ""
	@echo "Development (runs via eval):"
	@echo "  make status   - Show stack status"
	@echo "  make push     - Create/update PRs"
	@echo "  make pull     - Rebase and update"
	@echo "  make amend    - Interactive amend"
	@echo "  make eval EXPR='(expr)' - Evaluate expression"
