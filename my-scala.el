(add-to-list 'load-path "~/.emacs.d/vendor/scala-mode")

(setq scala-mode-indent:step 2)

(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
;;(add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp/")

;; (add-to-list 'load-path "/Users/tbernard/ensime/dist_2.9.2-SNAPSHOT/elisp")

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
