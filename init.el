(require 'org)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(direnv benchmark-init hurl-mode request treemacs tree-sitter-langs graphql-ts-mode nixpkgs-fmt timu-spacegrey-theme string-inflection csv-mode just-mode yaml-mode tuareg terraform-mode rustic rainbow-delimiters protobuf-mode projectile ox-gfm nix-mode magit lsp-ui lsp-mode htmlize dockerfile-mode company cider transient bnf-mode))
 '(package-vc-selected-packages
   '((hurl-mode :vc-backend Git :url "https://github.com/JasZhe/hurl-mode")))
 '(rustic-ansi-faces
   ["black" "#bf616a" "#a3be8c" "#ecbe7b" "#2257a0" "#b48ead" "#4db5bd" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
