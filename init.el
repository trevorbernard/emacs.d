;;; -*- lexical-binding: t -*-

;; Native compilation is now configured in early-init.el

(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "configuration.el" user-emacs-directory))
      (load-file (expand-file-name "configuration.el" user-emacs-directory))
    (require 'org)
    (org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((hurl-mode :vc-backend Git :url "https://github.com/JasZhe/hurl-mode")
     (timu-spacegrey-theme :vc-backend Git :url
                           "https://github.com/trevorbernard/timu-spacegrey-theme.git")))
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
