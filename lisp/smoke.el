;;; smoke.el --- Load the compiled config and assert it came up -*- lexical-binding: t -*-

;; A green byte-compile only proves the config parses. This loads it the way
;; init.el does, runs the startup hooks, opens one buffer per corfu host mode,
;; and checks the modes that have silently failed to enable before.

(setq debug-on-error t)

(load (expand-file-name "early-init.el" user-emacs-directory) nil t)
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

;; These load at startup only when the config was expanded without
;; `use-package-always-defer' (a JIT-built configuration.eln did that) or
;; early-init.el went back to requiring package.el.
(dolist (feature '(package cape rust-mode js css-mode just-ts-mode
                   ghostel claude-code-ide lsp-java dap-mode))
  (when (featurep feature)
    (push (format "%s loaded at startup" feature) smoke--failures)))

(dolist (mode '(emacs-lisp-mode text-mode conf-mode))
  (with-temp-buffer
    (funcall mode)
    (unless (bound-and-true-p corfu-mode)
      (push (format "corfu-mode in %s" mode) smoke--failures))
    (unless (eq (car completion-styles) 'tb/orderless-in-buffer)
      (push (format "tb/orderless-in-buffer in %s" mode) smoke--failures))))

;; Without its grammar a -ts-mode remap fails when the file is opened. Plain
;; `make' installs packages but not grammars.
(let ((missing (seq-remove #'treesit-language-available-p
                            (mapcar #'car tb/treesit-grammar-sources))))
  (when missing
    (push (format "tree-sitter grammars missing (run make grammars): %s"
                  (mapconcat #'symbol-name missing " "))
          smoke--failures)))

;; flycheck-mode came up in pest buffers with no pest checker registered.
(with-temp-buffer
  (pest-mode)
  (smoke-check (bound-and-true-p flycheck-mode))
  (smoke-check (memq 'pest flycheck-checkers)))

;; rustic must come up with its cargo and LSP modules but without org
;; (see the Rust section of configuration.org). Nothing above loads org.
(require 'rustic)
(smoke-check (featurep 'rustic-cargo))
(smoke-check (featurep 'rustic-lsp))
(smoke-check (not (featurep 'org)))

;; lsp-mode's default prefix is s-l, which a terminal can't send.
(require 'lsp-mode)
(smoke-check (keymapp (keymap-lookup lsp-mode-map "C-c l")))

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
