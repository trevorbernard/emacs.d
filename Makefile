EMACS ?= emacs
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
NATIVE_SCRIPT = lisp/native.el
GRAMMARS_SCRIPT = lisp/grammars.el
# init.elc is not produced by any rule (see lisp/build.el: init.el stays source);
# it is listed so `clean' sweeps one left by an older build.
GENERATED_FILES = init.elc early-init.el configuration.el configuration.elc package-quickstart.el package-quickstart.elc
ELN_CACHE_DIR = $(CURDIR)/eln-cache
CONFIGURATION_ORG = configuration.org

# The tangle rule below uses a grouped target, which silently degrades to
# independent targets (tangling twice) on the make 3.81 that macOS ships.
ifeq ($(filter grouped-target,$(.FEATURES)),)
$(error GNU make 4.3+ required; on macOS run: nix-shell -p gnumake --run 'make <target>')
endif

.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
# setup's prerequisites are order-dependent, and two targets can tangle the same
# files; -j would run them concurrently.
.NOTPARALLEL:

.PHONY: all setup clean compile native grammars tangle help check-deps quickstart smoke lint hooks

all: compile package-quickstart.el ## Tangle, byte-compile, install missing packages (incremental)

# `all' installs the packages, so setup only adds the dependency check and the
# grammars. There is no separate package-install step: it used to load the whole
# config from source, where a failed install is only a warning.
setup: check-deps all grammars ## First-time setup: packages, config, and tree-sitter grammars
	@echo ""
	@echo "Emacs setup complete. Start it with: emacs -nw"

# Tangling emits both files from one invocation (grouped target, needs GNU make 4.3+).
configuration.el early-init.el &: $(CONFIGURATION_ORG)
	@echo "Tangling configuration.org..."
	@$(EMACS) $(EMACS_FLAGS) --eval "(require 'org)" \
		--eval "(let ((inhibit-message t)) (org-babel-tangle-file \"$(CONFIGURATION_ORG)\"))"
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
	@# A configuration.eln from before the no-native-compile cookie would be
	@# loaded in place of the new .elc (see the Preamble in configuration.org).
	@rm -f "$(ELN_CACHE_DIR)"/*/configuration-*.eln
	@$(EMACS) $(EMACS_FLAGS) -l '$(COMPILE_SCRIPT)'
	@# The build above may have installed a package; compile it now so the
	@# first session doesn't have to. Native code is an optimization: the
	@# leading - keeps a failure here from deleting the .elc just built.
	-@$(EMACS) $(EMACS_FLAGS) -l '$(NATIVE_SCRIPT)'

# package.el rewrites this file itself whenever it installs a package, so the
# rule only fires when the file is missing, which is what `clean' leaves
# behind. Without it Emacs falls back to loading package.el and every
# package's autoloads at startup. Order-only on the .elc so that a fresh
# clone installs the packages first.
package-quickstart.el: | configuration.elc
	@$(MAKE) --no-print-directory quickstart

tangle: configuration.el ## Tangle configuration.org

compile: configuration.elc ## Tangle and byte-compile

native: early-init.el ## Native-compile package files that have no .eln yet
	@$(EMACS) $(EMACS_FLAGS) -l '$(NATIVE_SCRIPT)'

grammars: configuration.elc ## Install missing tree-sitter grammars (needs git and a C compiler)
	@$(EMACS) $(EMACS_FLAGS) -l '$(GRAMMARS_SCRIPT)'

# Byte-compiling only proves the config parses; this loads it and asserts the
# modes came up (see lisp/smoke.el).
smoke: lint all ## Load the compiled config and assert it came up
	@$(EMACS) $(EMACS_FLAGS) -l lisp/smoke.el

# Helpers are prefixed tb/ (os/ was the old prefix), and the org file uses
# no em-dashes.
lint: ## Reject the os/ helper prefix and em-dashes (run by smoke)
	@! grep -nE '\bos/[a-z-]+' $(CONFIGURATION_ORG) Makefile lisp/*.el || \
		{ echo "Error: os/ helper prefix; use tb/"; exit 1; }
	@! grep -n '—' $(CONFIGURATION_ORG) || \
		{ echo "Error: em-dash in $(CONFIGURATION_ORG)"; exit 1; }

hooks: ## Enable the pre-commit hook that runs smoke (once per clone)
	@git config core.hooksPath .githooks
	@echo "pre-commit hook enabled (.githooks/pre-commit runs make smoke)"

clean: ## Remove generated files and the native compilation cache
	@echo "Cleaning generated files..."
	@rm -f $(GENERATED_FILES)
	@if [ -d "$(ELN_CACHE_DIR)" ]; then \
		echo "Cleaning native compilation cache..."; \
		rm -rf "$(ELN_CACHE_DIR)"; \
	fi

# Everything is checked before failing, so one run lists all that is missing.
# 30+ for use-package :vc; tree-sitter for the -ts-mode remaps; git for :vc
# packages and grammar sources; a C compiler to build the grammars.
check-deps: ## Check for Emacs 30+ with tree-sitter, git, and a C compiler
	@ok=1; \
	if ! command -v $(EMACS) >/dev/null 2>&1; then \
		echo "Error: $(EMACS) not found; install Emacs 30+"; exit 1; \
	fi; \
	$(EMACS) $(EMACS_FLAGS) --eval "(when (< emacs-major-version 30) \
		(message \"Error: Emacs 30+ required, found %s\" emacs-version) (kill-emacs 1))" || ok=0; \
	$(EMACS) $(EMACS_FLAGS) --eval "(unless (and (fboundp 'treesit-available-p) (treesit-available-p)) \
		(message \"Error: this Emacs was built without tree-sitter\") (kill-emacs 1))" || ok=0; \
	$(EMACS) $(EMACS_FLAGS) --eval "(unless (native-comp-available-p) \
		(message \"Note: no native compilation; packages will run as bytecode\"))"; \
	command -v git >/dev/null 2>&1 || { echo "Error: git not found"; ok=0; }; \
	command -v cc >/dev/null 2>&1 || command -v gcc >/dev/null 2>&1 || command -v c99 >/dev/null 2>&1 || \
		{ echo "Error: no C compiler (cc, gcc or c99) to build tree-sitter grammars"; ok=0; }; \
	test $$ok = 1 || exit 1; \
	echo "Dependencies OK: $$($(EMACS) --version | head -1)"

# The default package-quickstart-file is already this checkout's, by way of
# --init-directory, so nothing from early-init.el is restated here. The file
# is byte-compiled as it is written; those warnings are about other
# people's autoloads.
quickstart: ## Regenerate package-quickstart.el (after removing a package)
	@echo "Refreshing package-quickstart.el..."
	@$(EMACS) $(EMACS_FLAGS) \
		--eval "(require 'package)" \
		--eval "(let ((inhibit-message t)) (package-quickstart-refresh))"

# Targets document themselves: the text after ## on a target line is its help.
help: ## Show this help
	@echo "Targets (set VERBOSE=1 for full install output):"
	@awk 'BEGIN {FS = ":.*## "} /^[a-z-]+:.*## / {printf "  %-12s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
