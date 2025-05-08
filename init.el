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
   '(ag bnf-mode cider company counsel csv-mode direnv dockerfile-mode dotenv-mode
        exec-path-from-shell flycheck htmlize hurl-mode inf-clojure just-ts-mode
        lsp-ivy lsp-ui magit mood-line nix-mode ob-rust org-bullets ox-gfm
        paredit projectile rainbow-delimiters rustic string-inflection
        terraform-mode timu-spacegrey-theme tuareg vterm yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((hurl-mode :vc-backend Git :url "https://github.com/JasZhe/hurl-mode")))
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
 '(org-document-title ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "black" :family "Sans Serif")))))
