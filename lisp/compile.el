  ;;; -*- lexical-binding: t -*-
(require 'org)

(org-babel-tangle-file "configuration.org")

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Using native compilation")
      (setq native-comp-speed 2)
      (setq native-comp-async-report-warnings-errors nil)
      (when (fboundp 'native-compile-async)
        (native-compile-async "configuration.el" t)
        (native-compile-async "early-init.el" t)
        (native-compile-async "init.el" t)))
  (message "Native compilation not available, using byte compilation")
  (byte-compile-file "configuration.el")
  (byte-compile-file "early-init.el")
  (byte-compile-file "init.el"))

(provide 'compile)
