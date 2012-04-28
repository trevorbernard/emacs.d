(require 'clojure-mode)
(require 'paredit)
(require 'ac-slime)
(require 'eldoc)

;; Hush fontifying compilation message in emacs23 that slows down compile
(setq font-lock-verbose nil)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (eldoc-mode 1)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (setq show-trailing-whitespace 1)
            (setq inferior-lisp-program "lein2 repl")))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

(add-hook 'slime-mode-hook
          (lambda ()
            (set-up-slime-ac)
            (auto-complete-mode 1)
            (paredit-mode 1)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))
            (paredit-mode 1)
            (auto-complete-mode 1)))

(provide 'my-clojure)
