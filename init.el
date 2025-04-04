;;; -*- lexical-binding: t -*-
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-speed 2)
  (setq native-comp-jit-compilation t)
  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path
                 (expand-file-name "eln-cache/" user-emacs-directory)))
  (setq native-comp-async-jobs-number 4))

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
   '(ag bnf-mode cider clojure-mode company csv-mode dhall-mode direnv
        dockerfile-mode dotenv-mode exec-path-from-shell htmlize hurl-mode
        inf-clojure ivy just-ts-mode lsp-ivy lsp-mode lsp-ui magit markdown-mode
        mood-line nix-mode nixfmt nixpkgs-fmt ob-rust org-bullets ox-gfm paredit
        projectile rainbow-delimiters robe rustic string-inflection
        terraform-mode timu-spacegrey-theme treemacs tuareg vterm which-key
        yafolding yaml-mode yasnippet))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2b303b" :foreground "#c0c5ce" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 200 :width normal :foundry "nil" :family "Fira Code"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo")))))
