# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a literate Emacs configuration using Org-mode. The main configuration lives in `configuration.org` and gets tangled (extracted) to `configuration.el`.

## Build Commands

```bash
make setup              # First-time setup: install packages + compile
make                    # Tangle and compile (auto-selects native if available)
make tangle             # Extract configuration.el from configuration.org
make compile            # Byte compile
make compile-native     # Native compile
make install-packages   # Install packages and Tree-sitter grammars
make clean              # Remove generated files and eln-cache
```

With Nix: `nix develop` provides Emacs with native compilation and cmake.

## Architecture

**Boot sequence:**
1. `early-init.el` - GC tuning, native-comp settings, package bootstrap, frame setup
2. `init.el` - Loads `configuration.el` (compiled) or tangles from org if missing
3. `configuration.el` - Generated from `configuration.org`, contains all packages and settings

**Key directories:**
- `lisp/` - Custom elisp (`compile.el` for build, `org-journal.el` for journaling)
- `elpa/` - Installed packages (auto-generated)
- `eln-cache/` - Native compilation cache (auto-generated)
- `snippets/` - YASnippet templates

## Editing Guidelines

- **Edit `configuration.org`, not `configuration.el`** - The .el file is generated
- Run `make tangle` after org changes, or just restart Emacs (it tangles automatically if .el is missing)
- Package declarations use `use-package` with deferred loading by default
- Tree-sitter modes are configured to auto-remap from traditional major modes
