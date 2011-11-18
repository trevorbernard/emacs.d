(require 'color-theme)

(defun os-x-theme ()
  ;; (setq default-frame-alist
  ;;       '((left . 22) (top . 44)
  ;;         (width . 100) (height . 50)))
)


(color-theme-initialize)
;; (load-library "color-theme-underwater.el")
(load-library "color-theme-sunburst.el")
(color-theme-tm)
(if (eq system-type 'darwin)
    (progn
      (setq mac-allow-anti-aliasing t)
      (set-face-font
       'default
       "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-")))

(if (eq system-type 'darwin)
    (progn
      (os-x-theme)
      (setenv "PATH" "/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/scala/bin:/usr/local/sbt:~/bin")
      (setq exec-path (append exec-path (list "/usr/local/scala/bin" "/usr/local/sbt" "~/bin")))))

(provide 'themes)
