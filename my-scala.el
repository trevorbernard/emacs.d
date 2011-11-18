(add-to-list 'load-path "~/.emacs.d/vendor/scala-mode")

(setq scala-mode-indent:step 2)

(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp/")

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
