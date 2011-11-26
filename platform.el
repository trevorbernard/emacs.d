(if (eq system-type 'darwin)
    (progn
      (setenv "PATH" "/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/scala/bin")
      (setq exec-path (append exec-path (list "/usr/local/bin" "/usr/local/scala/bin" "~/bin")))))

(provide 'platform)
