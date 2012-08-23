(require 'clojure-mode)
(require 'paredit)
(require 'auto-complete-config)
(require 'ac-slime)
(require 'eldoc)
(require 'nrepl)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))

;; Hush fontifying compilation message in emacs23 that slows down compile
(setq font-lock-verbose nil
      slime-kill-without-query-p t)


(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-default fill-column 80)
            ;; (auto-fill-mode 1)
            (paredit-mode 1)
            (eldoc-mode 1)
            (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
            (setq show-trailing-whitespace 1)
            (setq inferior-lisp-program "lein repl")))

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
