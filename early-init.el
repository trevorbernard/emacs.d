;;; -*- lexical-binding: t -*-

(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %s with %d garbage collections."
                              (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                              gcs-done)))

(setq gc-cons-threshold (* 1024 1024 256))

(set-display-table-slot standard-display-table 'vertical-border ?|)

;; Set frame parameters before frame creation
(setq default-frame-alist
      (append default-frame-alist
              '((background-color . "#2b303b")
                (foreground-color . "#c0c5ce")
                (fullscreen . maximized)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars)
                (horizontal-scroll-bars)
                (internal-border-width . 0))))


(set-face-attribute 'mode-line nil :background 'unspecified)

;; Set font based on system type
(cond
 ((eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Fira Code Retina-20")))
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Fira Code-16"))))

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defun display-startup-echo-area-message ())  ; suppresses the message

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "tbernard")

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; enable smooth scrolling
(pixel-scroll-precision-mode t)

;; Native compilation configuration - consolidated here to avoid race conditions
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (let ((eln-cache-dir (expand-file-name "eln-cache/" user-emacs-directory)))
    (condition-case err
        (progn
          ;; Ensure eln-cache directory exists and is writable
          (unless (file-directory-p eln-cache-dir)
            (make-directory eln-cache-dir t))

          ;; Only add to path if directory is actually writable
          (when (and (boundp 'native-comp-eln-load-path)
                     (file-writable-p eln-cache-dir))
            (add-to-list 'native-comp-eln-load-path eln-cache-dir))

          ;; Configure native compilation settings
          (setq native-comp-speed 2
                comp-deferred-compilation t
                native-comp-jit-compilation t
                ;; Dynamic async jobs based on CPU cores (max 4, min 1)
                native-comp-async-jobs-number (min 4 (max 1 (/ (num-processors) 2)))
                ;; Enable warnings only for debugging - set to t if compilation fails
                native-comp-async-report-warnings-errors nil)

          (message "Native compilation configured: %d async jobs, cache: %s"
                   native-comp-async-jobs-number eln-cache-dir))

      (error
       (message "Native compilation setup failed: %s - falling back to defaults" err)
       ;; Minimal fallback configuration
       (setq native-comp-speed 1
             comp-deferred-compilation nil
             native-comp-async-report-warnings-errors t)))))

(setenv "LSP_USE_PLISTS" "true")

(setq read-process-output-max (* 10 1024 1024)) ; 10MB

;; Bootstrap package system and use-package for better startup performance
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-always-defer t
        use-package-verbose nil  ; Set to t for debugging, nil for performance
        use-package-minimum-reported-time 0.1)
  (require 'use-package))

(provide 'early-init)

;;; early-init.el ends here
