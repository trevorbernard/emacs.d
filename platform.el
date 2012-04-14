(when (equal system-type 'darwin)
  ;; I'm going to eventually clean up this list
  (setenv "PATH" "/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/scala/bin")
  (setq exec-path (append exec-path (list "/usr/local/bin" "/usr/local/scala/bin" "~/bin")))
  ;; (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  ;; (push "/opt/local/bin" exec-path)
  )

(provide 'platform)
