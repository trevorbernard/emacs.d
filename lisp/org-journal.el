;;; org-journal.el --- Daily journal entry management -*- lexical-binding: t -*-

(defcustom org-journal-dir "~/journal/"
  "Directory containing journal entries."
  :type 'directory
  :risky t)

(defun org-journal--file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (format "%s.org" daily-name) org-journal-dir)))

(defun org-journal-open-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (org-journal--file-today)))

(global-set-key (kbd "C-c f j") 'org-journal-open-today)

(provide 'org-journal)
