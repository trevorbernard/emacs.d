EMACS = emacs
EMACS_FLAGS = -Q --batch
COMPILE_SCRIPT = lisp/compile.el
GENERATED_FILES = init.elc configuration.el configuration.elc

.DEFAULT_GOAL := all

.PHONY: all clean compile help

all: compile

clean:
	@echo "Cleaning generated files..."
	@rm -f $(GENERATED_FILES)

compile: init.el configuration.org clean
	@echo "Compiling Emacs configuration..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'

help:
	@echo "Available targets:"
	@echo "  all      - Default target, same as 'compile'"
	@echo "  compile  - Compile Emacs configuration files"
	@echo "  clean    - Remove generated files"
	@echo "  help     - Display this help message"
