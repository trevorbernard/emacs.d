;;; compile.el --- tangle configuration.org -*- lexical-binding: t -*-
(require 'org)

;; Load early-init.el to set up package system for compilation
(load-file (expand-file-name "early-init.el" user-emacs-directory))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Using native compilation")
      (setq native-comp-speed 2)
      (when (fboundp 'native-compile)
        (native-compile "configuration.el")
        (native-compile "init.el")))
  (message "Native compilation not available, using byte compilation")
  (byte-compile-file "configuration.el")
  (byte-compile-file "init.el"))

(provide 'compile)

;;; compile.el ends here
