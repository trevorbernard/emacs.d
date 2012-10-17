(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(solarized-theme
                      clojure-mode
                      paredit
                      nrepl
                      auto-complete
                      ac-nrepl)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d")

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (equal system-type 'darwin)
  (load-library "mac.el"))

(require 'preferences)
(require 'bindings)
(require 'nrepl)
(require 'auto-complete)
(require 'ac-nrepl)

(defun repl-modes ()
  (auto-complete-mode)
  (ac-nrepl-setup)
  (paredit-mode))

(add-to-list 'same-window-buffer-names "*nrepl*")
(setq nrepl-popup-stacktraces nil)


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

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-default fill-column 80)
            (auto-complete-mode 1)
            (paredit-mode 1)
            (eldoc-mode 1)
            (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
            (setq show-trailing-whitespace 1)
            (setq inferior-lisp-program "lein repl")))

(add-hook 'nrepl-mode-hook (lambda ()
                             (ac-nrepl-setup)
                             (paredit-mode 1)
                             (eldoc-mode 1)
                             (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;; (add-hook 'nrepl-mode-hook (lambda () (repl-modes)) t)
;; (add-hook 'clojure-nrepl-mode-hook (lambda () (repl-modes)) t)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(load-theme 'solarized-dark t)

(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
