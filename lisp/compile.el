;;; compile.el --- Compile configuration files -*- lexical-binding: t -*-
(require 'org)

;; Load early-init.el to set up package system for compilation
(load-file (expand-file-name "early-init.el" user-emacs-directory))

;; On a fresh install there is no elpa/archives/ cache, so package-install
;; called by use-package :ensure t at macro-expansion time would fail.
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
