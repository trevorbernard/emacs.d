(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'load-path "~/.emacs.d")

;Add all top-level subdirectories of .emacs.d to the load path
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))
;I like to keep third party libraries seperate in ~/.emacs.d/vendor
(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))

;;(require 'themes)
(require 'platform)
(require 'fonts)
(require 'preferences)
(require 'bindings)
(require 'defuns)
(require 'modes)

(load-library "my-scala.el")
(load-library "my-nxml.el")
(load-library "my-javascript.el")
(load-library "my-php.el")
;;(load-library "my-clojure.el")
(load-library "my-protobuf.el")

;; Sending mail
(setq user-full-name "Trevor Bernard")
(setq user-full-mail-address "trevor.bernard@gmail.com")

;; (add-to-list 'load-path "~/.emacs.d/packages/")
;; (require 'package)
;; (add-to-list 'package-archives
;; '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;; Temporary clojure/slime setup
;; clojure-mode
(add-to-list 'load-path "~/clojure-mode")
(require 'clojure-mode)

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

;; paredit
;;(add-to-list 'load-path "~/opt/paredit")
(require 'paredit)

;; slime
;; (eval-after-load "slime" 
;;   '(progn (slime-setup '(slime-repl))	
;; 	(defun paredit-mode-enable () (paredit-mode 1))	
;; 	(add-hook 'slime-mode-hook 'paredit-mode-enable)	
;; 	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
;; 	(setq slime-protocol-version 'ignore)))

;; (add-to-list 'load-path "~/slime")
;; (require 'slime)
;; (slime-setup)
