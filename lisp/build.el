;;; build.el --- Compile configuration files -*- lexical-binding: t -*-
(require 'org)
;; early-init.el loads neither at startup; the build needs both.
(require 'package)
(require 'use-package)

;; Load early-init.el to set up package system for compilation
(load (expand-file-name "early-init.el" user-emacs-directory) nil t)

;; Installing a package prints dozens of lines: the download, every file it
;; byte-compiles, and warnings about other people's code. A fresh clone
;; printed 2,600 of them and buried the one line that mattered. Print one
;; line per package instead; `make VERBOSE=1' brings the rest back. A failed
;; install still signals, and is reported at the end.
(defvar build--verbose (not (member (getenv "VERBOSE") '(nil "" "0"))))

(defun build--quietly (fn &rest args)
  (let ((inhibit-message (not build--verbose)))
    (apply fn args)))

(defun build--vc-install (fn package &rest args)
  (message "Installing %s from source..."
           (if (consp package) (car package) package))
  (apply #'build--quietly fn package args))

(advice-add 'package-refresh-contents :around #'build--quietly)
(advice-add 'package-install :around #'build--quietly)
(advice-add 'package-vc-install :around #'build--vc-install)

;; use-package :ensure t installs missing packages at macro-expansion time (during
;; byte-compilation), which needs package-archive-contents populated. early-init's
;; package-quickstart path activates installed packages but never reads the archive
;; index into memory, so read the on-disk elpa/archives/ cache here (no network).
;; Only fall back to a network refresh when that cache is genuinely absent, e.g. on
;; a fresh install.
(package-read-all-archive-contents)
(unless package-archive-contents
  (message "Fetching package archives...")
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
        (message "Installing %s..." package)
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
;; (≈2x faster than loading source). We deliberately do NOT native-compile, and
;; configuration.el carries `no-native-compile' so the JIT doesn't either: its
;; subprocess never loads early-init.el, so use-package expanded without
;; `use-package-always-defer' and the .eln, loaded in place of this .elc,
;; required every untriggered package at startup. Installed packages keep
;; their own .eln. init.el is left as source: it is tiny and compiling it risks a
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
