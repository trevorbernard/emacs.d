;;; compile.el --- Compile configuration files -*- lexical-binding: t -*-
(require 'org)

;; Load early-init.el to set up package system for compilation
(load-file (expand-file-name "early-init.el" user-emacs-directory))

;; use-package :ensure t installs missing packages at macro-expansion time (during
;; byte-compilation), which needs package-archive-contents populated. early-init's
;; package-quickstart path activates installed packages but never reads the archive
;; index into memory, so read the on-disk elpa/archives/ cache here (no network).
;; Only fall back to a network refresh when that cache is genuinely absent, e.g. on
;; a fresh install.
(package-read-all-archive-contents)
(unless package-archive-contents
  (package-refresh-contents))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Byte-compile configuration.el so a basename `load' in init.el finds a .elc
;; (≈2x faster than loading source). We deliberately do NOT native-compile:
;; config code runs once at startup, byte vs native load time is identical, and
;; the .eln is not loaded at interactive startup. Installed packages keep their
;; own .eln. init.el is left as source: it is tiny and compiling it risks a
;; stale init.elc shadowing edits (the bootstrap loads before load-prefer-newer).
(byte-compile-file "configuration.el")

(provide 'compile)

;;; compile.el ends here
