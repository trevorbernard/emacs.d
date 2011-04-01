(require 'color-theme)

(defun os-x-theme ()
  (setq default-frame-alist
        '((left . 22) (top . 44)
          (width . 100) (height . 50)))
  (setq mac-allow-anti-aliasing t)
  (set-face-font
   'default
   "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-"))


(color-theme-initialize)
;; (load-library "color-theme-underwater.el")
(load-library "color-theme-sunburst.el")
(color-theme-tm)
(os-x-theme)

(provide 'theme)
