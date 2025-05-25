EMACS = emacs
EMACS_FLAGS = -Q --batch
COMPILE_SCRIPT = lisp/compile.el
GENERATED_FILES = init.elc configuration.el configuration.elc
ELN_CACHE_DIR = $(HOME)/.emacs.d/eln-cache

.DEFAULT_GOAL := setup

.PHONY: all setup install-packages clean compile compile-native check-native-comp tangle help check-deps

all: check-native-comp

setup: check-deps install-packages all
	@echo "\nEmacs setup complete! You can now start Emacs."
	@echo "Tip: Run 'make help' to see all available targets"

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

compile: init.el tangle
	@echo "Compiling Emacs configuration (byte compilation)..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'

compile-native: init.el tangle
	@echo "Compiling Emacs configuration with native compilation..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)' --eval "(setq comp-deferred-compilation t)"

tangle: configuration.org
	@echo "Tangling configuration.org..."
	@$(EMACS) $(EMACS_FLAGS) --eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"configuration.org\")"

check-deps:
	@echo "Checking system dependencies..."
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "Emacs not found. Please install Emacs 29+ first."; exit 1; }
	@$(EMACS) --version | head -1
	@echo "Emacs found"

install-packages: tangle
	@echo "Installing Emacs packages and Tree-sitter grammars..."
	@$(EMACS) $(EMACS_FLAGS) \
		--eval "(require 'package)" \
		--eval "(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") (\"melpa-stable\" . \"https://stable.melpa.org/packages/\") (\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(unless (package-installed-p 'use-package) (package-install 'use-package))" \
		--eval "(load-file \"configuration.el\")" \
		--eval "(when (fboundp 'os/setup-install-grammars) (os/setup-install-grammars))" || true
	@echo "Package installation complete"

help:
	@echo "Emacs Configuration Setup"
	@echo ""
	@echo "Available targets:"
	@echo "  setup           - Complete setup (recommended for first time)"
	@echo "  check-deps      - Check if Emacs is available"
	@echo "  install-packages- Install Emacs packages and Tree-sitter grammars"
	@echo "  all             - Tangle and compile configuration"
	@echo "  compile         - Compile Emacs configuration files (byte compilation)"
	@echo "  compile-native  - Compile Emacs configuration files (native compilation)"
	@echo "  clean           - Remove generated files and native compilation cache"
	@echo "  tangle          - Tangle Emacs configuration org file"
	@echo "  help            - Display this help message"
	@echo ""
	@echo "For a complete setup, run: make setup"
