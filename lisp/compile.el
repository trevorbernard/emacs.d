(require 'org)

(org-babel-tangle-file "configuration.org")
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(byte-compile-file "configuration.el")
(byte-compile-file "init.el")
(provide 'compile)
