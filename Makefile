EMACS = emacs
EMACS_FLAGS = -Q --batch
export LSP_USE_PLISTS = true
# macOS 26+ removed /usr/lib stubs; the native compiler's GCC driver needs the SDK path
export SDKROOT ?= $(shell xcrun --show-sdk-path 2>/dev/null)
COMPILE_SCRIPT = lisp/compile.el
GENERATED_FILES = init.elc configuration.el configuration.elc package-quickstart.el package-quickstart.elc
ELN_CACHE_DIR = $(CURDIR)/eln-cache

# Validate required files exist
CONFIGURATION_ORG = configuration.org
INIT_EL = init.el

.DEFAULT_GOAL := all
.DELETE_ON_ERROR:

.PHONY: all setup install-packages clean compile compile-native tangle help check-deps validate quickstart

all: compile

setup: check-deps install-packages all
	@echo ""
	@echo "Emacs setup complete! You can now start Emacs."
	@echo "Tip: Run 'make help' to see all available targets"

# Tangling emits both files from one invocation (grouped target, needs GNU make 4.3+).
configuration.el early-init.el &: $(CONFIGURATION_ORG)
	@echo "Tangling configuration.org..."
	@$(EMACS) $(EMACS_FLAGS) --eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"$(CONFIGURATION_ORG)\")"
	@# org-babel skips rewriting outputs whose content is unchanged, which would
	@# leave them older than configuration.org and make this rule fire forever.
	@touch configuration.el early-init.el

# early-init.el is a prerequisite because $(COMPILE_SCRIPT) loads it.
configuration.elc: configuration.el early-init.el $(COMPILE_SCRIPT)
	@echo "Byte-compiling Emacs configuration..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'

tangle: configuration.el

compile: configuration.elc

# Backwards-compatible alias: the config is byte-compiled (see lisp/compile.el).
compile-native: compile

clean:
	@echo "Cleaning generated files..."
	@rm -f $(GENERATED_FILES)
	@find . -type f -name '*.eln' -delete 2>/dev/null || true
	@if [ -d "$(ELN_CACHE_DIR)" ]; then \
		echo "Cleaning native compilation cache..."; \
		rm -rf "$(ELN_CACHE_DIR)"; \
	fi

validate:
	@echo "Validating required files..."
	@test -f $(CONFIGURATION_ORG) || { echo "Error: $(CONFIGURATION_ORG) not found"; exit 1; }
	@test -f $(INIT_EL) || { echo "Error: $(INIT_EL) not found"; exit 1; }
	@test -f $(COMPILE_SCRIPT) || { echo "Error: $(COMPILE_SCRIPT) not found"; exit 1; }
	@echo "All required files found"

check-deps: validate
	@echo "Checking system dependencies..."
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "Error: Emacs not found. Please install Emacs 29+ first."; exit 1; }
	@$(EMACS) --version | head -1
	@echo "Emacs found"

install-packages: configuration.el
	@echo "Installing Emacs packages and Tree-sitter grammars..."
	@$(EMACS) $(EMACS_FLAGS) \
		--eval "(require 'package)" \
		--eval "(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") (\"melpa-stable\" . \"https://stable.melpa.org/packages/\") (\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(unless (package-installed-p 'use-package) (package-install 'use-package))" \
		--eval "(load-file \"configuration.el\")" \
		--eval "(when (fboundp 'os/setup-install-grammars) (os/setup-install-grammars))" || { echo "Error: package installation failed"; exit 1; }
	@$(MAKE) quickstart
	@echo "Package installation complete"

quickstart:
	@echo "Refreshing package-quickstart.el..."
	@$(EMACS) $(EMACS_FLAGS) \
		--eval "(require 'package)" \
		--eval "(setq package-quickstart-file \"$(CURDIR)/package-quickstart.el\")" \
		--eval "(package-initialize)" \
		--eval "(package-quickstart-refresh)"

help:
	@echo "Emacs Configuration Setup"
	@echo ""
	@echo "Available targets:"
	@echo "  setup           - Complete setup (recommended for first time)"
	@echo "  validate        - Validate required files exist"
	@echo "  check-deps      - Check if Emacs is available and validate files"
	@echo "  install-packages- Install Emacs packages and Tree-sitter grammars"
	@echo "  quickstart      - Regenerate package-quickstart.el (after install/remove/vc-update)"
	@echo "  all             - Tangle and byte-compile configuration (incremental)"
	@echo "  compile         - Tangle and byte-compile configuration files"
	@echo "  compile-native  - Alias for compile (config is byte-compiled)"
	@echo "  clean           - Remove generated files and native compilation cache"
	@echo "  tangle          - Tangle Emacs configuration org file"
	@echo "  help            - Display this help message"
	@echo ""
	@echo "For a complete setup, run: make setup"
