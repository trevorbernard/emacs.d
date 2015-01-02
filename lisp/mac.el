(set-default-font "Inconsolata-16")

(setq default-frame-alist
      `((font . "Inconsolata-16")))

;; make sure path is correct when launched as application
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(provide 'mac)
