;; C

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "K&R")
             (setq tab-width 8)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)))

(add-hook 'css-mode
          '(lambda ()
             (setq css-indent-offset 2)))

(add-to-list 'load-path "~/.emacs.d/vendor/thrift.el")
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))
(provide 'modes)