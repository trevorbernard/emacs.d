;;; native.el --- Native-compile installed packages ahead of time -*- lexical-binding: t -*-

;; The JIT only compiles a package file once a session has loaded it, and a
;; session that exits first kills the job. Opening and closing Emacs kept
;; restarting the same compiles and left zero-byte .eln.tmp files behind,
;; while the package went on loading as bytecode. This compiles every
;; byte-compiled file under elpa/ that has no .eln yet, so a session finds
;; them ready. The JIT stays on as the fallback for anything installed or
;; upgraded from inside Emacs.

;; early-init.el redirects the eln cache into this checkout and activates the
;; packages, which the compile jobs need on `load-path' to expand macros.
(load-file (expand-file-name "early-init.el" user-emacs-directory))

(unless (native-comp-available-p)
  (message "Native compilation is not available; skipping")
  (kill-emacs 0))

(require 'comp-run)

;; A file that fails to compile would be retried on every run. Its expected
;; .eln name goes in this list instead. The name hashes the file's content,
;; so a new version of the file gets another try.
(defvar native--skip-file
  (expand-file-name "aot-skip"
                    (expand-file-name comp-native-version-dir
                                      (car native-comp-eln-load-path))))

(defun native--skips ()
  (when (file-exists-p native--skip-file)
    (with-temp-buffer
      (insert-file-contents native--skip-file)
      (split-string (buffer-string) "\n" t))))

;; The JIT is triggered by a compiled function being defined, so it never
;; compiles a file that only holds data (nerd-icons' glyph tables, face
;; definitions). Follow it: a data-only .eln loads slower than its .elc, and
;; compiling the ones doom-modeline loads added 6ms to every start.
(defun native--defines-functions-p (elc)
  (with-temp-buffer
    (insert-file-contents-literally elc)
    (search-forward "defalias" nil t)))

(defun native--pending ()
  "Return the byte-compiled files under elpa/ worth compiling that have no .eln."
  (let ((skips (native--skips)))
    (seq-filter
     (lambda (el)
       (and (file-exists-p (concat el "c"))
            (not (comp-lookup-eln el))
            (native--defines-functions-p (concat el "c"))
            (not (member (comp-el-to-eln-rel-filename el) skips))))
     (directory-files-recursively
      (expand-file-name "elpa" user-emacs-directory) "\\.el\\'"))))

(let* ((pending (native--pending))
       (native-comp-async-jobs-number (num-processors))
       (reported 0))
  (if (null pending)
      (message "Native compilation: nothing to do")
    (message "Native-compiling %d package files with %d jobs..."
             (length pending) native-comp-async-jobs-number)
    (native-compile-async pending)
    (while (or comp-files-queue (> (comp--async-runnings) 0))
      (sleep-for 1)
      (when (>= (setq reported (1+ reported)) 15)
        (setq reported 0)
        (message "  %d left" (+ (length comp-files-queue) (comp--async-runnings)))))
    (let ((failed (seq-remove #'comp-lookup-eln pending)))
      (message "Native compilation: %d compiled, %d failed"
               (- (length pending) (length failed)) (length failed))
      (when failed
        (write-region (mapconcat #'comp-el-to-eln-rel-filename failed "\n")
                      nil native--skip-file t 'silent)
        (write-region "\n" nil native--skip-file t 'silent)
        (message "Left as bytecode, and skipped until they change:\n  %s"
                 (mapconcat (lambda (el) (file-relative-name el user-emacs-directory))
                            failed "\n  "))))))

;;; native.el ends here
