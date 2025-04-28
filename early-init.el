;;; -*- lexical-binding: t -*-
(when window-system
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (cond
   ((eq system-type 'darwin)
    (set-frame-font "Fira Code Retina 20" nil t))
   ((eq system-type 'gnu/linux)
    (set-frame-font "Fira Code 11" nil t))))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
