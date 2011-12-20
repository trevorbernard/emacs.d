(add-to-list 'load-path "~/.emacs.d/vendor/protobuf-mode.el")
;; (defconst my-protobuf-style
;;   '((c-basic-offset . 2)
;;     (indent-tabs-mode . nil)))

;; (add-hook 'protobuf-mode-hook
;;           (lambda () (c-add-style "my-style" my-protobuf-style t)))

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

(require 'protobuf-mode)
