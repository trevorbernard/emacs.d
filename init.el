(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(paredit
                      clojure-mode
		      clojure-test-mode
		      cider
                      auto-complete
                      ac-nrepl
                      markdown-mode
                      ;; pandoc-mode
                      mmm-mode)
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
(require 'cider)
(require 'auto-complete)
(require 'ac-nrepl)
(require 'clojure-mode)

;; Clojure
(setq nrepl-popup-stacktraces nil)
;; (setq nrepl-hide-special-buffers t)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))

;; (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'cider-repl-mode))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (auto-complete-mode 1)))
(add-hook 'clojure-mode-hook
          (lambda ()
            (auto-complete-mode 1)
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
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
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
      (set-buffer (cider-find-or-create-repl-buffer))
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
 ;; (pandoc-mode t)
 (flyspell-mode t))

;; (require 'mmm-auto)

;; (mmm-add-classes
;; '((markdown-clojure
;;    :submode clojure-mode
;;    :face mmm-declaration-submode-face
;;   :front "^```clj[\n\r]+"
;;    :back "^```$")))

;;(setq mmm-global-mode 'maybe)
;;(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-clojure)

(add-hook 'markdown-mode-hook 'markdown-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(fci-rule-color "#282a2e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#cc6666") (40 . "#de935f") (60 . "#f0c674") (80 . "#b5bd68") (100 . "#8abeb7") (120 . "#81a2be") (140 . "#b294bb") (160 . "#cc6666") (180 . "#de935f") (200 . "#f0c674") (220 . "#b5bd68") (240 . "#8abeb7") (260 . "#81a2be") (280 . "#b294bb") (300 . "#cc6666") (320 . "#de935f") (340 . "#f0c674") (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
