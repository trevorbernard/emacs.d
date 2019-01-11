(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(paredit
                      clojure-mode
                      dracula-theme
                      projectile
                      cider
                      company
                      rainbow-delimiters
                      markdown-mode
                      protobuf-mode
                      yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'dracula t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (equal system-type 'darwin)
  (load-library "mac.el"))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "Transparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; (transparency 90)

(require 'preferences)
(require 'bindings)
(require 'cider)
(require 'clojure-mode)
(require 'company)

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq nrepl-log-messages t)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)))

(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-display-help-banner nil)

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (linum-mode t)
            (setq-default fill-column 80)
            (paredit-mode 1)
            (subword-mode t)
            (setq show-trailing-whitespace 1)
            (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode 1)
                                  (eldoc-mode 1)))

(global-set-key (kbd "C-c C-j") 'cider-jack-in)
(global-set-key '[f3] 'cider-eval-expression-at-point-in-repl)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (GET* 2)
  (POST 2)
  (POST* 2)
  (OPTIONS 2)
  (OPTIONS* 2)
  (PUT 2)
  (PUT* 2)
  (DELETE 2)
  (DELETE* 2)
  (HEAD 2)
  (HEAD* 2)
  (ANY 2)
  (ANY* 2)
  (context 2))

(defun my-last-expression ()
  "Return the last sexp."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun cider-execute-in-current-repl (expr)
  (if (not (get-buffer (nrepl-current-connection-buffer)))
      (message "No active nREPL connection.")
    (progn
      (set-buffer (cider-get-repl-buffer))
      (goto-char (point-max))
      (insert expr)
      (cider-repl-return))))

(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (my-last-expression)))
    ;; Eat white
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (cider-execute-in-current-repl form)))

(setq js-indent-level 2)

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
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(defun markdown-hook ()
 (setq-default fill-column 80)
 (auto-fill-mode t)
 (flyspell-mode t))

(add-hook 'markdown-mode-hook 'markdown-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode protobuf-mode markdown-mode rainbow-delimiters company cider projectile dracula-theme clojure-mode paredit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
