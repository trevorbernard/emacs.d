;; C

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "K&R")
             (setq tab-width 8)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)))

(provide 'modes)

(add-hook 'css-mode
          '(lambda ()
             (setq css-indent-offset 2)))