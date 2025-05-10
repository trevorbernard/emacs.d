;;; -*- lexical-binding: t -*-

;; Set frame parameters before frame creation
(setq default-frame-alist
      (append default-frame-alist
              '((background-color . "#2b303b")
                (foreground-color . "#c0c5ce")
                (fullscreen . maximized))))

;; Set font based on system type
(cond
 ((eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Fira Code Retina-20")))
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Fira Code-16"))))

;; Set the background to so we don't see a flicker
(set-face-attribute 'default nil :background "#2b303b" :foreground "#c0c5ce")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here
