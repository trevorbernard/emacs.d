;; (setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(global-auto-revert-mode t)             ;; Auto revert buffers
(ido-mode 1)
(setq iswitchb-prompt-newbuffer nil)	;;
(column-number-mode 1)			;; Show column number
(delete-selection-mode 1)		;; Allow delete of selection
(fset 'yes-or-no-p 'y-or-n-p)		;; Shorten confirmation message
(global-font-lock-mode 1)		;; Syntax Highlighting
(setq inhibit-startup-message t)	;; No splash screen
(setq-default set-fill-column 80)	;; Set fill to 80 chars
(setq-default indent-tabs-mode nil)	;; Use Spaces
(setq default-tab-width 8)		;; Tab width: 8 chars
(show-paren-mode 1)			;; Highlight parenthesis
(transient-mark-mode 1)			;; Highlight selected Regions
(setq scroll-preserve-screen-position t);; make pgup/dn remember current line
(setq-default truncate-lines nil)	;; Wrap Lines
;;(setq-default show-trailing-whitespace t)

;; (if (fboundp 'blink-cursor-mode)
;;     (blink-cursor-mode -1))

(setq default-directory "~/")		;; Set default directory
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)

;; Allow functions w/o warnings
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(setq-default c-basic-offset 4)

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar my-term-shell "/bin/zsh")

(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste)
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (setq ansi-term-color-vector
          (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue
                                 ,magenta ,cyan ,base2)))))

(defun indent-buffer ()
  (indent-region (point-min) (point-max)))

;; (add-hook 'term-mode-hook 'my-term-hook)

(provide 'preferences)
