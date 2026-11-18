# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a literate Emacs configuration using Org-mode. The main configuration lives in `configuration.org` and gets tangled (extracted) to `configuration.el` and `early-init.el`.

## Build Commands

```bash
make setup              # First-time setup: check deps, install packages, compile, install grammars
make                    # Tangle and byte-compile (incremental)
make smoke              # Load the compiled config and assert key modes are on
make tangle             # Extract configuration.el + early-init.el from configuration.org
make grammars           # Build Tree-sitter grammars that are missing or whose pin changed
make help               # List every target
make quickstart         # Regenerate package-quickstart.el
make native             # Native-compile package files that have no .eln yet
make hooks              # Enable the pre-commit hook (once per clone)
make clean              # Remove generated files and eln-cache
```

The build is incremental: `tangle` and `compile` are aliases for real file
targets, so `make` does nothing when `configuration.org` is older than its
outputs. Use `make -B` to force a rebuild. Package and grammar installs
print one line each; `make VERBOSE=1` shows everything.

A green `make` only proves the config byte-compiles. Run `make smoke` after
any change to see that it loads; add an assertion to `lisp/smoke.el` when a
mode fails to come up silently. `.githooks/pre-commit` runs it on commits
that touch the config.

The Makefile requires GNU make 4.3+. macOS's `/usr/bin/make` is 3.81 and
is rejected, so on the Mac run every target as
`nix-shell -p gnumake --run 'make <target>'`.

With Nix: `nix develop` provides Emacs 30 with native compilation, GNU
make and cmake. It works on Linux (checked 2026-11). On the Mac it failed
building Emacs (libgccjit check) as of 2026-10.

## Architecture

**Boot sequence:**
1. `early-init.el` - GC tuning, native-comp settings, package bootstrap, frame setup
2. `init.el` - Loads `configuration.el` (compiled), or warns to run `make setup` if it hasn't been built
3. `configuration.el` - Generated from `configuration.org`, contains all packages and settings

**Key directories:**
- `lisp/` - Build scripts (`build.el` compiles, `native.el` native-compiles `elpa/`, `grammars.el` installs grammars, `smoke.el` is the smoke test)
- `.githooks/` - Tracked git hooks, enabled by `make hooks`
- `.claude/skills/elisp-expert/` - Elisp conventions for this config
- `elpa/` - Installed packages (auto-generated)
- `eln-cache/` - Native compilation cache (auto-generated)
- `snippets/` - YASnippet templates

## Navigation

- `grep -n '^\*' configuration.org` prints the section outline with line numbers; read the section you need rather than the whole file
- `README.org` is a symlink to `configuration.org`. Search `configuration.org` directly, or a recursive grep reports every hit twice

## Editing Guidelines

- **Edit `configuration.org`, not `configuration.el` or `early-init.el`** - Both are generated
- Adding a package: `make` installs any missing `:ensure` package during byte-compilation (and refreshes `package-quickstart.el`). `lisp/build.el` retries once against a fresh archive index and fails the build if the install still fails, so a green `make` means the package is installed
- Native compilation: `make` native-compiles any package file with no `.eln` whenever it rebuilds the config (`lisp/native.el`, about a minute from an empty cache, 0.15s when there is nothing to do). Run `make native` by hand after an Emacs upgrade or a package upgrade made from inside Emacs. Files that define no functions are left as bytecode on purpose
- Tree-sitter grammars: every entry in `tb/treesit-grammar-sources` is pinned to a tag (Emacs 30 clones with `git clone -b`, so not a commit hash). `lisp/grammars.el` records what each grammar was built from in `tree-sitter/pins.eld` and rebuilds on a pin change. After adding or bumping one, run `make grammars` then `make smoke`, which fontifies a sample per mode at font-lock level 4; a new grammar needs a sample in `lisp/smoke.el`
- Removing a package: delete its `elpa/` directory, then run `make quickstart`. `package-quickstart.el` is not regenerated on removal, and its stale autoloads keep loading the removed package at startup. Check other packages' `*-pkg.el` before deleting a shared dependency
- Run `make`, not bare `make tangle`, after org changes. `init.el` sets `load-prefer-newer`, so a tangled-but-uncompiled `configuration.el` shadows the stale `.elc` and Emacs loads the slower interpreted config
