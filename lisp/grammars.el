;;; grammars.el --- Build tree-sitter grammars at their pinned revisions -*- lexical-binding: t -*-

;; The grammar list lives in configuration.org, next to the modes that need
;; it. This builds every grammar that is missing or was built from a
;; different revision than the list now names. A built library doesn't say
;; what it was built from, so that is recorded in tree-sitter/pins.eld.
;; Without the record, changing a pin did nothing on a machine that already
;; had the grammar.
;;
;; Each grammar is built into a scratch directory and moved into place only
;; if the library appears. `treesit-install-language-grammar' just warns when
;; a clone or a compile fails, so a machine with no C compiler came out of
;; `make setup' green and with no grammars.

(load (expand-file-name "early-init.el" user-emacs-directory) nil t)
(load (expand-file-name "configuration" user-emacs-directory) nil t)
(require 'treesit)

(defvar grammars--dir (expand-file-name "tree-sitter" user-emacs-directory))
(defvar grammars--pins-file (expand-file-name "pins.eld" grammars--dir))

(defun grammars--pins ()
  (when (file-exists-p grammars--pins-file)
    (with-temp-buffer
      (insert-file-contents grammars--pins-file)
      (read (current-buffer)))))

(defun grammars--outdated (pins)
  (seq-remove (lambda (entry)
                (and (equal (cdr entry) (alist-get (car entry) pins))
                     (treesit-language-available-p (car entry))))
              tb/treesit-grammar-sources))

;; The scratch directory sits inside tree-sitter/ so the move is a rename on
;; one filesystem. That replaces the directory entry and leaves the old
;; library's contents alone, which matters when a running Emacs has it mapped.
(defun grammars--build (entry)
  "Build the grammar ENTRY describes. Return non-nil if it was installed."
  (let ((tmp (make-temp-file (expand-file-name "build" grammars--dir) t))
        (treesit-language-source-alist (list entry)))
    (unwind-protect
        (progn
          (treesit-install-language-grammar (car entry) tmp)
          (when-let ((lib (car (directory-files tmp t "\\`libtree-sitter-"))))
            (rename-file lib (file-name-as-directory grammars--dir) t)
            t))
      (delete-directory tmp t))))

(let* ((pins (grammars--pins))
       (outdated (grammars--outdated pins))
       (verbose (not (member (getenv "VERBOSE") '(nil "" "0"))))
       (failed nil))
  (if (null outdated)
      (message "Tree-sitter grammars: nothing to do")
    (message "Building %d tree-sitter grammars: %s"
             (length outdated) (mapconcat (lambda (e) (symbol-name (car e))) outdated " "))
    (make-directory grammars--dir t)
    (dolist (entry outdated)
      (if (let ((inhibit-message (not verbose)))
            (grammars--build entry))
          (setf (alist-get (car entry) pins) (cdr entry))
        (push (car entry) failed)))
    (with-temp-file grammars--pins-file
      (pp pins (current-buffer))))
  ;; Skip kill-emacs-hook: savehist would write this batch session's state
  ;; over the real history file.
  (let ((kill-emacs-hook nil))
    (when failed
      (message "Failed to build tree-sitter grammars: %s\nThey need git, a C compiler and network access. Run with VERBOSE=1 for details."
               (mapconcat #'symbol-name (nreverse failed) " "))
      (kill-emacs 1))
    (kill-emacs 0)))

;;; grammars.el ends here
