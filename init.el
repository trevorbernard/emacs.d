(require 'org)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mood-line lsp-ivy yafolding robe exec-path-from-shell yasnippet string-inflection ag htmlize bnf-mode yaml-mode dockerfile-mode just-mode csv-mode terraform-mode nix-mode nixpkgs-fmt tuareg rustic lsp-ui lsp-mode ivy markdown-mode ox-gfm ob-rust paredit cider inf-clojure clojure-mode company rainbow-delimiters magit projectile timu-spacegrey-theme hurl-mode))
 '(rustic-ansi-faces
   ["black" "#bf616a" "#a3be8c" "#ecbe7b" "#2257a0" "#b48ead" "#4db5bd" "white"])
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
