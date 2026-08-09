;;; -*- lexical-binding: t -*-

;; Native compilation is configured in early-init.el.

;; Prefer a newer configuration.el over a stale configuration.elc, so a bare
;; `make tangle' (without recompiling) still loads the current configuration.
(setq load-prefer-newer t)

(let ((file-name-handler-alist nil)
      (config (expand-file-name "configuration" user-emacs-directory)))
  ;; Load the byte-compiled configuration.elc when present (≈2x faster than
  ;; loading source); fall back to source, or tangle from org on a fresh clone.
  (if (or (file-exists-p (concat config ".elc"))
          (file-exists-p (concat config ".el")))
      (load config nil t)
    (require 'org)
    (org-babel-load-file (concat config ".org"))))

;; Keep Custom's writes (safe-local-variable-values, M-x customize) out of
;; this git-tracked file. custom.el is gitignored, machine-local state.
;; No `package-vc-selected-packages' should reappear in it: the :vc
;; declarations in configuration.org own the vc packages, and a copy here
;; recorded indent-bars without its pinned :rev, so anything driving
;; installation from Custom would have quietly resolved the pin to newest.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))
