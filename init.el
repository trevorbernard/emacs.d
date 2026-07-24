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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((indent-bars :url "https://github.com/jdtsmith/indent-bars")))
 '(safe-local-variable-values
   '((eval setq lsp-yaml-max-items-computed 10000)
     (eval progn
           (when
               (and (fboundp 'lsp-workspace-folders-remove) (lsp-workspace-root))
             (lsp-workspace-folders-remove (lsp-workspace-root)))
           (when (fboundp 'lsp-workspace-folders-add)
             (lsp-workspace-folders-add default-directory)))
     (eval with-eval-after-load 'lsp-mode
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\ops\\'")
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\docs\\'")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
