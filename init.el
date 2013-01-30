(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(solarized-theme
                      clojure-mode
                      clojure-test-mode
                      paredit
                      nrepl
                      auto-complete
                      ac-nrepl
                      markdown-mode
                      haskell-mode)
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
(require 'clojure-mode)

(setq nrepl-popup-stacktraces nil)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (setq-default fill-column 80)
            (paredit-mode t)
            (subword-mode t)
            (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
            (setq inferior-lisp-program "lein repl")))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode 1)
                                  (eldoc-mode 1)))
(load-theme 'solarized-dark t)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(setq js-indent-level 2)

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(transparency 93)
