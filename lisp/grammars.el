;;; grammars.el --- Install missing tree-sitter grammars -*- lexical-binding: t -*-

;; The grammar list and its installer live in configuration.org, next to the
;; modes that need them. This loads the compiled config, runs the installer,
;; and then checks the result itself: `treesit-install-language-grammar' only
;; warns when a clone or a compile fails, so without the check a machine with
;; no C compiler came out of `make setup' green and with no grammars.

(load (expand-file-name "early-init.el" user-emacs-directory) nil t)
(load (expand-file-name "configuration" user-emacs-directory) nil t)

(defun grammars--missing ()
  (seq-remove #'treesit-language-available-p
              (mapcar #'car tb/treesit-grammar-sources)))

(let ((missing (grammars--missing))
      (verbose (not (member (getenv "VERBOSE") '(nil "" "0")))))
  (if (null missing)
      (message "Tree-sitter grammars: nothing to do")
    (message "Installing %d tree-sitter grammars: %s"
             (length missing) (mapconcat #'symbol-name missing " "))
    (let ((inhibit-message (not verbose)))
      (tb/setup-install-grammars))
    (setq missing (grammars--missing))))

;; Skip kill-emacs-hook: savehist would write this batch session's state over
;; the real history file.
(let ((kill-emacs-hook nil)
      (missing (grammars--missing)))
  (when missing
    (message "Failed to install tree-sitter grammars: %s\nThey need git, a C compiler and network access. Run with VERBOSE=1 for details."
             (mapconcat #'symbol-name missing " "))
    (kill-emacs 1))
  (kill-emacs 0))

;;; grammars.el ends here
