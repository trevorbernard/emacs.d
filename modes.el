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

;; Thrift mode
(add-to-list 'load-path "~/.emacs.d/vendor/thrift.el")
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

;; Less mode
(load "~/.emacs.d/vendor/less-css-mode.el")

;; Markdown mode
(add-hook 'markdown-mode '(lambda ()
                            (setq fill-column 80)))
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\|\\.md\\|\\.markdown" . markdown-mode))

(provide 'modes)
