(require 'clojure-mode)
;; clojure-mode
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(require 'clojure-mode)
(require 'paredit)

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (setq inferior-lisp-program "lein repl")))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

(add-hook 'slime-mode-hook
          (lambda ()
            (paredit-mode 1)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (paredit-mode 1)))
(provide 'my-clojure)
