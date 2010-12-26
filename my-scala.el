(add-to-list 'load-path "~/.emacs.d/vendor/scala-mode")

(require 'scala-mode-auto)
;;(setq yas/my-directory "~/.emacs.d/vendor/scala-mode/contrib/yasnippet/snippets")
;;(yas/load-directory yas/my-directory)

(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path "vendor/ensime/dist/elisp/")

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'scala-mode-hook
          '(lambda ()
             (setq scala-mode-indent:step 2)
;;             (yas/minor-mode-on)
             ))
