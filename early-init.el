;;; -*- lexical-binding: t -*-
(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Fira Code Retina-20"))
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Fira Code" :height 160)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here
