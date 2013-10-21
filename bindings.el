;; Ignore minimize functionality only in GUI
(when window-system
  (global-set-key "\C-z" 'ignore)
  (global-set-key "\C-x\C-z" 'ignore))

;; Allow M-x to be accessed by ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c C-j") 'cider-jack-in)
;;(global-set-key '[f3] 'nrepl-eval-print-last-expression)
(global-set-key '[f3] 'cider-eval-expression-at-point-in-repl)

(provide 'bindings)
