;;; smoke.el --- Load the compiled config and assert it came up -*- lexical-binding: t -*-

;; A green byte-compile only proves the config parses. This loads it the way
;; init.el does, runs the startup hooks, opens one buffer per corfu host mode,
;; and checks the modes that have silently failed to enable before.

(setq debug-on-error t)

(load-file (expand-file-name "early-init.el" user-emacs-directory))
(load (expand-file-name "configuration" user-emacs-directory) nil t)
(run-hooks 'after-init-hook 'emacs-startup-hook)

(defvar smoke--failures nil)

(defmacro smoke-check (form)
  `(unless (ignore-errors ,form)
     (push ,(format "%S" form) smoke--failures)))

(smoke-check (bound-and-true-p vertico-mode))
(smoke-check (bound-and-true-p marginalia-mode))
(smoke-check (bound-and-true-p savehist-mode))
(smoke-check (eq (car completion-styles) 'orderless))
(smoke-check (null completion-category-defaults))

(dolist (mode '(emacs-lisp-mode text-mode conf-mode))
  (with-temp-buffer
    (funcall mode)
    (unless (bound-and-true-p corfu-mode)
      (push (format "corfu-mode in %s" mode) smoke--failures))
    (unless (eq (car completion-styles) 'tb/orderless-in-buffer)
      (push (format "tb/orderless-in-buffer in %s" mode) smoke--failures))))

;; Skip kill-emacs-hook: savehist, projectile and friends would write this
;; batch session's state over the real files in the checkout.
(let ((kill-emacs-hook nil))
  (when smoke--failures
    (message "Smoke test failed:\n  %s"
             (string-join (nreverse smoke--failures) "\n  "))
    (kill-emacs 1))
  (message "Smoke test passed")
  (kill-emacs 0))

;;; smoke.el ends here
