;;; -*- lexical-binding: t -*-
(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Fira Code Retina-20"))
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Fira Code-11")))

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      set-fringe-mode nil)

;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (set-fringe-mode nil)

(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(provide 'early-init)

;;; early-init.el ends here
