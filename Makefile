EMACS = emacs
# --init-directory pins user-emacs-directory (and with it package-user-dir and
# eln-cache) to this checkout; -Q alone leaves it at ~/.emacs.d, so builds from a
# worktree or alternate clone would read and write the wrong tree.
EMACS_FLAGS = -Q --batch --init-directory=$(CURDIR)/
export LSP_USE_PLISTS = true
# macOS 26+ removed /usr/lib stubs; the native compiler's GCC driver needs the SDK
# path. Guarded and simply-expanded: an exported recursive variable would re-run
# xcrun for every recipe's environment, on every platform.
ifeq ($(shell uname -s),Darwin)
ifeq ($(origin SDKROOT),undefined)
export SDKROOT := $(shell xcrun --show-sdk-path 2>/dev/null)
endif
endif
COMPILE_SCRIPT = lisp/build.el
# init.elc is not produced by any rule (see lisp/build.el: init.el stays source);
# it is listed so `clean' sweeps one left by an older build.
GENERATED_FILES = init.elc early-init.el configuration.el configuration.elc package-quickstart.el package-quickstart.elc
ELN_CACHE_DIR = $(CURDIR)/eln-cache

# Validate required files exist
CONFIGURATION_ORG = configuration.org
INIT_EL = init.el

# The tangle rule below uses a grouped target, which silently degrades to
# independent targets (tangling twice) on the make 3.81 that macOS ships.
ifeq ($(filter grouped-target,$(.FEATURES)),)
$(error GNU make 4.3+ required; on macOS: brew install make, then use gmake)
endif

.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
# setup's prerequisites are order-dependent, and two targets can tangle the same
# files; -j would run them concurrently.
.NOTPARALLEL:

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
	@# Guard before touching: touch would otherwise create an empty stand-in for
	@# an output the tangle failed to emit, turning that into a green build.
	@for f in configuration.el early-init.el; do \
		test -s "$$f" || { echo "Error: tangling produced no $$f"; exit 1; }; \
	done
	@# org-babel skips rewriting outputs whose content is unchanged, which would
	@# leave them older than configuration.org and make this rule fire forever.
	@touch configuration.el early-init.el

# early-init.el is a prerequisite because $(COMPILE_SCRIPT) loads it.
configuration.elc: configuration.el early-init.el $(COMPILE_SCRIPT)
	@echo "Byte-compiling Emacs configuration..."
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'

tangle: configuration.el

compile: configuration.elc

# Backwards-compatible alias: the config is byte-compiled (see lisp/build.el).
compile-native: compile

clean:
	@echo "Cleaning generated files..."
	@rm -f $(GENERATED_FILES)
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
	@command -v $(EMACS) >/dev/null 2>&1 || { echo "Error: Emacs not found. Please install Emacs 30+ first."; exit 1; }
	@$(EMACS) --version | head -1
	@# 30+ for use-package :vc, which configuration.org relies on.
	@$(EMACS) $(EMACS_FLAGS) --eval "(when (< emacs-major-version 30) \
		(message \"Error: Emacs 30+ required, found %s\" emacs-version) (kill-emacs 1))"
	@echo "Emacs found"

# Load early-init.el rather than restating its package setup: it owns
# package-archives *and* package-pinned-packages, and a copy here silently
# dropped the pins, installing pinned packages from the wrong archive.
install-packages: configuration.el early-init.el
	@echo "Installing Emacs packages and Tree-sitter grammars..."
	@$(EMACS) $(EMACS_FLAGS) \
		--eval "(load-file \"early-init.el\")" \
		--eval "(package-refresh-contents)" \
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
