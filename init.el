(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))


(add-to-list 'load-path "~/.emacs.d")
;Add all top-level subdirectories of .emacs.d to the load path
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))

;I like to keep third party libraries seperate in ~/.emacs.d/vendor
(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))

(when (equal system-type 'darwin)
  (load-library "mac.el")
)

(require 'preferences)
(require 'bindings)
(require 'defuns)
(require 'modes)
(require 'themes)

(add-to-list 'load-path "~/.emacs/vendor/magit")
(require 'magit)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(load-library "my-scala.el")
(load-library "my-nxml.el")
(load-library "my-javascript.el")
(load-library "my-php.el")
(load-library "my-clojure.el")
(load-library "my-protobuf.el")
(load-library "vendor/thrift.el")

;; Sending mail
(setq user-full-name "Trevor Bernard")
(setq user-full-mail-address "trevor.bernard@gmail.com")
