(iswitchb-mode 1)			;; Switch browswer viewing
(setq iswitchb-prompt-newbuffer nil)
(column-number-mode 1)			;; Show column number
(delete-selection-mode 1)		;; Allow delete of selection
(fset 'yes-or-no-p 'y-or-n-p)		;; Shorten confirmation message
(global-font-lock-mode 1)		;; Syntax Highlighting
(setq inhibit-startup-message t)	;; No splash screen
(setq-default set-fill-column 79)	;; Set fill to 79 chars
(setq-default indent-tabs-mode nil)	;; Use Spaces
(setq default-tab-width 8)		;; Tab width: 8 chars
(show-paren-mode 1)			;; Highlight parenthesis
(transient-mark-mode 1)			;; Highlight selected Regions
(setq scroll-preserve-screen-position t);; make pgup/dn remember current line
(setq-default truncate-lines nil)	;; Wrap Lines

(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

(setq default-directory "~/")		;; Set default directory
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq x-select-enable-clipboard t)
;; Allow functions w/o warnings
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'preferences)