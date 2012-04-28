;; My clojure environment customizations

(require 'clojure-mode)
(require 'paredit)
(require 'ac-slime)
(require 'eldoc)
;; Hush fontifying compilation message in emacs23 that slows down compile
(setq font-lock-verbose nil)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (eldoc-mode 1)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)
            (setq inferior-lisp-program "lein2 repl")))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))

(add-hook 'slime-mode-hook
          (lambda ()
            (set-up-slime-ac)
            (auto-complete-mode 1)
            (paredit-mode 1)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))
            (paredit-mode 1)
            (auto-complete-mode 1)))

(provide 'my-clojure)
