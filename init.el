(require 'org)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(lsp-ui ox-gfm just-mode yasnippet-snippets graphql-mode go-mode lsp-mode rustic edit-indirect tuareg ## nix-mode terraform-mode sbt-mode elixir-mode bnf-mode eclim ag gradle-mode elpy htmlize dockerfile-mode clj-refactor yaml-mode rainbow-delimiters protobuf-mode projectile paredit markdown-mode dracula-theme company cider))
 '(safe-local-variable-values '((cider-shadow-cljs-default-options . "app"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
