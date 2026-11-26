;;; smoke.el --- Load the compiled config and assert it came up -*- lexical-binding: t -*-

;; A green byte-compile only proves the config parses. This loads it the way
;; init.el does, runs the startup hooks, opens one buffer per corfu host mode,
;; and checks the modes that have silently failed to enable before.

(setq debug-on-error t)

(load (expand-file-name "early-init.el" user-emacs-directory) nil t)
(load (expand-file-name "configuration" user-emacs-directory) nil t)
(run-hooks 'after-init-hook 'emacs-startup-hook)

(defvar smoke--failures nil)

(defmacro smoke-check (form)
  `(unless (ignore-errors ,form)
     (push ,(format "%S" form) smoke--failures)))

(smoke-check (bound-and-true-p vertico-mode))
(smoke-check (bound-and-true-p marginalia-mode))
(smoke-check (bound-and-true-p savehist-mode))
(smoke-check (eq (car completion-styles) 'orderless))
(smoke-check (null completion-category-defaults))

;; These load at startup only when the config was expanded without
;; `use-package-always-defer' (a JIT-built configuration.eln did that) or
;; early-init.el went back to requiring package.el.
(dolist (feature '(package cape rust-mode js css-mode just-ts-mode
                   ghostel claude-code-ide lsp-java dap-mode))
  (when (featurep feature)
    (push (format "%s loaded at startup" feature) smoke--failures)))

(dolist (mode '(emacs-lisp-mode text-mode conf-mode))
  (with-temp-buffer
    (funcall mode)
    (unless (bound-and-true-p corfu-mode)
      (push (format "corfu-mode in %s" mode) smoke--failures))
    (unless (eq (car completion-styles) 'tb/orderless-in-buffer)
      (push (format "tb/orderless-in-buffer in %s" mode) smoke--failures))))

;; Without its grammar a -ts-mode remap fails when the file is opened. Plain
;; `make' installs packages but not grammars, and lisp/grammars.el records
;; what each one was built from, so a changed pin shows up here too.
;;
;; A grammar that loads can still be the wrong one for the mode: its queries
;; are only compiled when a buffer is fontified, and a mismatch is an error
;; in every buffer of that mode. Level 4 enables every query. The sample
;; text hardly matters, since a query is checked against the grammar, not
;; the buffer.
(require 'treesit)

(defvar smoke--treesit-samples
  '((bash-ts-mode       . "f() { echo \"$1\"; }\n")
    (c-ts-mode          . "int main(void) { return 0; }\n")
    (c++-ts-mode        . "class A { public: int f() const { return 1; } };\n")
    (cmake-ts-mode      . "project(x)\nif(WIN32)\n  set(A \"${B}\")\nendif()\n")
    (css-ts-mode        . "a:hover { color: #fff; }\n")
    (dockerfile-ts-mode . "FROM alpine:3 AS b\nRUN echo hi\n")
    (go-ts-mode         . "package main\nfunc f(x int) int { return x }\n")
    (go-mod-ts-mode     . "module example.com/x\n\ngo 1.22\n")
    (html-ts-mode       . "<p class=\"a\">hi</p>\n")
    (java-ts-mode       . "class A { int f() { return 1; } }\n")
    (js-ts-mode         . "const f = (a) => <b>{a}</b>;\n")
    (json-ts-mode       . "{\"a\": [1, true, null]}\n")
    (just-ts-mode       . "build target='all':\n    echo {{target}}\n")
    (lua-ts-mode        . "local function f(a) return a .. 'x' end\n")
    (python-ts-mode     . "def f(a: int) -> str:\n    return f\"{a}\"\n")
    (ruby-ts-mode       . "def f(x)\n  \"#{x}\"\nend\n")
    (rust-ts-mode       . "fn f(x: u8) -> u8 { x + 1 }\n")
    (toml-ts-mode       . "[a]\nb = \"c\"\n")
    (tsx-ts-mode        . "const C = (p: {a: string}) => <b>{p.a}</b>;\n")
    (typescript-ts-mode . "const f = (a: number): string => `${a}`;\n")
    (yaml-ts-mode       . "a:\n  - b: 1\n")))

(let* ((pins-file (expand-file-name "tree-sitter/pins.eld" user-emacs-directory))
       (pins (when (file-exists-p pins-file)
               (with-temp-buffer
                 (insert-file-contents pins-file)
                 (read (current-buffer)))))
       (outdated (seq-remove
                  (lambda (entry)
                    (and (equal (cdr entry) (alist-get (car entry) pins))
                         (treesit-language-available-p (car entry))))
                  tb/treesit-grammar-sources))
       (reached nil))
  (if outdated
      (push (format "tree-sitter grammars missing or built from another revision (run make grammars): %s"
                    (mapconcat (lambda (entry) (symbol-name (car entry))) outdated " "))
            smoke--failures)
    (pcase-dolist (`(,mode . ,text) smoke--treesit-samples)
      (condition-case err
          (with-temp-buffer
            (insert text)
            (let ((treesit-font-lock-level 4)
                  (inhibit-message t))
              (funcall mode)
              (dolist (parser (treesit-parser-list))
                (push (treesit-parser-language parser) reached))
              (font-lock-ensure)
              (indent-region (point-min) (point-max))))
        (error (push (format "%s: %s" mode (error-message-string err))
                     smoke--failures))))
    (when-let ((unreached (seq-remove (lambda (entry) (memq (car entry) reached))
                                      tb/treesit-grammar-sources)))
      (push (format "no sample in lisp/smoke.el reaches these grammars: %s"
                    (mapconcat (lambda (entry) (symbol-name (car entry))) unreached " "))
            smoke--failures))))

;; Flyspell is started from an idle timer, which batch never fires. Find the
;; timer the hook made for this buffer and run it by hand. Skipped where
;; aspell isn't installed.
(when (executable-find "aspell")
  (with-temp-buffer
    (gfm-mode)
    (let ((timer (seq-find (lambda (timer)
                             (memq (current-buffer) (timer--args timer)))
                           timer-idle-list)))
      (if (null timer)
          (push "no flyspell timer in a markdown buffer" smoke--failures)
        (let ((inhibit-message t))
          (timer-event-handler timer))
        (smoke-check (bound-and-true-p flyspell-mode))))))

;; flycheck-mode came up in pest buffers with no pest checker registered.
(with-temp-buffer
  (pest-mode)
  (smoke-check (bound-and-true-p flycheck-mode))
  (smoke-check (memq 'pest flycheck-checkers)))

;; rustic must come up with its cargo and LSP modules but without org
;; (see the Rust section of configuration.org). Nothing above loads org.
(require 'rustic)
(smoke-check (featurep 'rustic-cargo))
(smoke-check (featurep 'rustic-lsp))
(smoke-check (not (featurep 'org)))

;; lsp-mode's default prefix is s-l, which a terminal can't send.
(require 'lsp-mode)
(smoke-check (keymapp (keymap-lookup lsp-mode-map "C-c l")))

;; tsgo fails initialize on a null client capability; lsp-mode sends
;; textDocument.inlineCompletion as one unless the config's advice is on.
(smoke-check (not (string-search
                   "\"inlineCompletion\":null"
                   (json-serialize (lsp--client-capabilities)
                                   :null-object nil :false-object :json-false))))

;; tsgo must outrank ts-ls, or it only starts when ts-ls is missing.
(require 'lsp-javascript)
(smoke-check (> (lsp--client-priority (gethash 'tsgo lsp-clients))
                (lsp--client-priority (gethash 'ts-ls lsp-clients))))

;; Skip kill-emacs-hook: savehist, projectile and friends would write this
;; batch session's state over the real files in the checkout.
(let ((kill-emacs-hook nil))
  (when smoke--failures
    (message "Smoke test failed:\n  %s"
             (string-join (nreverse smoke--failures) "\n  "))
    (kill-emacs 1))
  (message "Smoke test passed")
  (kill-emacs 0))

;;; smoke.el ends here
