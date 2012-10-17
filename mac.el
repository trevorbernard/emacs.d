;; change command to meta, and ignore option to use weird Norwegian keyboard
;;(setq mac-option-modifier 'none)
;;(setq mac-command-modifier 'meta)
;;(setq ns-function-modifier 'hyper)

(set-default-font "Inconsolata-16")

(setq default-frame-alist
      `((font . "Inconsolata-16")))

;; make sure path is correct when launched as application
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; (push "/usr/local/bin" exec-path)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
;; (add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(provide 'mac)
