;;; build.el --- Compile configuration files -*- lexical-binding: t -*-
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

;; The cached index can still list a package at a version the archive no longer
;; serves; `use-package-ensure-elpa' only refreshes when the package is missing
;; from the index outright, so the download 404s. And the use-package macro
;; downgrades every :ensure error to a warning, so the build exited 0 with the
;; package uninstalled. Retry once against a fresh index, record what still
;; fails, and fail the build after compiling.
(defvar build--install-failures nil)

(defun build--ensure-package (name args _state &optional _no-refresh)
  (dolist (ensure args)
    (let ((package (or (and (eq ensure t) (use-package-as-symbol name))
                       ensure)))
      (when (consp package)
        (use-package-pin-package (car package) (cdr package))
        (setq package (car package)))
      (when (and package (not (package-installed-p package)))
        (condition-case err
            (condition-case nil
                (package-install package)
              (error
               (package-refresh-contents)
               (package-install package)))
          (error
           (push (format "%s: %s" package (error-message-string err))
                 build--install-failures)))))))

(setq use-package-ensure-function #'build--ensure-package)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Byte-compile configuration.el so a basename `load' in init.el finds a .elc
;; (≈2x faster than loading source). We deliberately do NOT native-compile:
;; config code runs once at startup, byte vs native load time is identical, and
;; the .eln is not loaded at interactive startup. Installed packages keep their
;; own .eln. init.el is left as source: it is tiny and compiling it risks a
;; stale init.elc shadowing edits (the bootstrap loads before load-prefer-newer).
;; byte-compile-file returns nil on failure without signalling, and batch Emacs
;; would still exit 0 — exit non-zero so make actually stops.
(unless (byte-compile-file "configuration.el")
  (kill-emacs 1))

(when build--install-failures
  (message "Failed to install:\n  %s"
           (string-join (nreverse build--install-failures) "\n  "))
  (kill-emacs 1))

(provide 'build)

;;; build.el ends here
