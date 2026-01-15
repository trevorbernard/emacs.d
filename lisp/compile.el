;;; compile.el --- Compile configuration files -*- lexical-binding: t -*-
(require 'org)

;; Load early-init.el to set up package system for compilation
(load-file (expand-file-name "early-init.el" user-emacs-directory))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(let ((files '("configuration.el" "init.el")))
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Using native compilation")
        (setq native-comp-speed 2)
        (dolist (file files)
          (native-compile file)))
    (message "Native compilation not available, using byte compilation")
    (dolist (file files)
      (byte-compile-file file))))

(provide 'compile)

;;; compile.el ends here
