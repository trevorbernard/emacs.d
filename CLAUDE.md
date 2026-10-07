# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a literate Emacs configuration using Org-mode. The main configuration lives in `configuration.org` and gets tangled (extracted) to `configuration.el`.

## Build Commands

```bash
make setup              # First-time setup: install packages + compile
make                    # Tangle and byte-compile (incremental)
make tangle             # Extract configuration.el + early-init.el from configuration.org
make compile            # Byte compile
make compile-native     # Alias for compile (config is byte-compiled only)
make install-packages   # Install packages and Tree-sitter grammars
make clean              # Remove generated files and eln-cache
```

The build is incremental: `tangle` and `compile` are aliases for real file
targets, so `make` does nothing when `configuration.org` is older than its
outputs. Use `make -B` to force a rebuild.

The Makefile requires GNU make 4.3+. macOS's `/usr/bin/make` is 3.81 and
is rejected, so on the Mac run every target as
`nix-shell -p gnumake --run 'make <target>'`.

With Nix: `nix develop` is meant to provide Emacs with native compilation
and cmake, but as of 2026-10 it fails building Emacs (libgccjit check) on
the Mac.

## Architecture

**Boot sequence:**
1. `early-init.el` - GC tuning, native-comp settings, package bootstrap, frame setup
2. `init.el` - Loads `configuration.el` (compiled) or tangles from org if missing
3. `configuration.el` - Generated from `configuration.org`, contains all packages and settings

**Key directories:**
- `lisp/` - Custom elisp (`build.el` for build)
- `elpa/` - Installed packages (auto-generated)
- `eln-cache/` - Native compilation cache (auto-generated)
- `snippets/` - YASnippet templates

## Editing Guidelines

- **Edit `configuration.org`, not `configuration.el`** - The .el file is generated
- Adding a package: `make` installs any missing `:ensure` package during byte-compilation (and refreshes `package-quickstart.el`). `lisp/build.el` retries once against a fresh archive index and fails the build if the install still fails, so a green `make` means the package is installed
- Removing a package: delete its `elpa/` directory, then run `make quickstart`. `package-quickstart.el` is not regenerated on removal, and its stale autoloads keep loading the removed package at startup. Check other packages' `*-pkg.el` before deleting a shared dependency
- Run `make` after org changes so the config is tangled **and** recompiled. Bare `make tangle` only regenerates `configuration.el`; because `init.el` sets `load-prefer-newer`, the now-newer source shadows the stale `.elc` and Emacs loads interpreted (slower) config until you `make compile`. With the incremental build, plain `make` always leaves the `.elc` up to date, so this only bites if you stop at `make tangle`.
- Package declarations use `use-package` with deferred loading by default
- Tree-sitter modes are configured to auto-remap from traditional major modes
