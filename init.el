(require 'org)

(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "configuration.elc" user-emacs-directory))
      (load-file (expand-file-name "configuration.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs mood-line lsp-ivy yafolding robe exec-path-from-shell yasnippet string-inflection ag htmlize bnf-mode yaml-mode dockerfile-mode just-mode csv-mode terraform-mode nix-mode nixpkgs-fmt tuareg rustic lsp-ui lsp-mode ivy markdown-mode ox-gfm ob-rust paredit cider inf-clojure clojure-mode company rainbow-delimiters magit projectile timu-spacegrey-theme hurl-mode))
 '(safe-local-variable-values
   '((eval with-eval-after-load 'lsp-mode
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\ops\\'")
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\docs\\'")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
