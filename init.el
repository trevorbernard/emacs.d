(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(color-theme-sanityinc-tomorrow
                      clojure-mode
                      clojure-test-mode
                      mmm-mode
                      paredit
                      nrepl
                      nrepl-decompile
                      auto-complete
                      ac-nrepl
                      markdown-mode
                      pandoc-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d")

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (equal system-type 'darwin)
  (load-library "mac.el"))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "Transparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(transparency 95)

(require 'preferences)
(require 'bindings)
(require 'nrepl)
(require 'auto-complete)
(require 'ac-nrepl)
(require 'clojure-mode)

;; Clojure
;; (setq nrepl-popup-stacktraces nil)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'nrepl-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-repl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (setq-default fill-column 80)
            (paredit-mode 1)
;;            (subword-mode t)
            (setq show-trailing-whitespace 1)
            (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
;;            (setq inferior-lisp-program "lein repl")
            ))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode 1)
                                  (eldoc-mode 1)))

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(defun nrepl-execute-in-current-repl (expr)
  (if (not (get-buffer (nrepl-current-connection-buffer)))
      (message "No active nREPL connection.")
    (progn
      (set-buffer (nrepl-find-or-create-repl-buffer))
      (goto-char (point-max))
      (insert expr)
      (nrepl-return))))

(defun nrepl-refresh ()
  (interactive)
  (nrepl-execute-in-current-repl
   "(clojure.tools.namespace.repl/refresh)"))

(defun nrepl-reset ()
  (interactive)
  (nrepl-execute-in-current-repl
   "(user/reset)"))

(defun nrepl-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (nrepl-expression-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (nrepl-execute-in-current-repl form)))

(setq js-indent-level 2)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))
(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun markdown-hook ()
  (setq-default fill-column 80)
  (auto-fill-mode t)
  (pandoc-mode t)
  (flyspell-mode t))

(require 'mmm-auto)

(mmm-add-classes
 '((markdown-clojure
    :submode clojure-mode
    :face mmm-declaration-submode-face
    :front "^```clj[\n\r]+"
    :back "^```$")))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-clojure)

(add-hook 'markdown-mode-hook 'markdown-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
