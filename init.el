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
(require 'themes)
(require 'fonts)
(require 'preferences)
(require 'bindings)
(require 'defuns)
(require 'modes)

(load-library "my-scala.el")
(load-library "my-nxml.el")
(load-library "my-javascript.el")
(load-library "my-php.el")

;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")

;; Sending mail
(setq user-full-name "Trevor Bernard")
(setq user-full-mail-address "trevor.bernard@gmail.com")
