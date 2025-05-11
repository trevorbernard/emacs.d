EMACS = emacs
EMACS_FLAGS = -Q --batch
COMPILE_SCRIPT = lisp/compile.el
GENERATED_FILES = init.elc configuration.el configuration.elc
ELN_CACHE_DIR = $(HOME)/.emacs.d/eln-cache

.DEFAULT_GOAL := all

.PHONY: all clean compile compile-native check-native-comp tangle help

all: check-native-comp

check-native-comp:
	@if $(EMACS) $(EMACS_FLAGS) --eval "(if (fboundp 'native-comp-available-p) (kill-emacs 0) (kill-emacs 1))"; then \
		$(MAKE) compile-native; \
	else \
		$(MAKE) compile; \
		@echo "Native compilation not available, using regular byte compilation"; \
	fi

clean:
	@echo "Cleaning generated files..."
	@rm -f $(GENERATED_FILES)
	@find . -type f -name '*.eln' -delete

compile: init.el configuration.org clean
	@echo "Compiling Emacs configuration (byte compilation)..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'

compile-native: init.el configuration.org
	@echo "Compiling Emacs configuration with native compilation..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)' --eval "(setq comp-deferred-compilation t)"

tangle:
	@echo "Tangling configuration.org..."
	@$(EMACS) $(EMACS_FLAGS) --eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"configuration.org\")"

help:
	@echo "Available targets:"
	@echo "  all             - Default target, detects and uses native compilation if available"
	@echo "  compile         - Compile Emacs configuration files with byte compilation"
	@echo "  compile-native  - Compile Emacs configuration files with native compilation"
	@echo "  clean           - Remove generated files and native compilation cache"
	@echo "  tangle          - Tangle Emacs configuration org file"
	@echo "  help            - Display this help message"
