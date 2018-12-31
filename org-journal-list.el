;;; org-journal-list.el --- Org mode Journal List

;;; Commentary:
;;;  This package does not provide any key binding.
;;;  You will have to do it yourself.

;;; Code:
(defcustom org-journal-list-display-alist
  '((side . left)
    (window-width . 40)
    (slot . -1))
  "Alist used to display notes buffer."
  :type 'alist)

(defcustom org-journal-list-default-directory
  "~/notes/journal/"
  "Default directory of your journal."
  :type 'directory)

(defcustom org-journal-list-default-suffix
  ".journal.org"
  "Default suffix of your journal files."
  :type 'string)

(defun org-journal-list--read-file (path)
  "Read all org files in a given PATH."
  (with-temp-buffer
    (insert-file-contents (concat org-journal-list-default-directory path))
    (split-string (buffer-string) "\n" t)))

(defun org-journal-list--read-first-few-lines (list)
  "Read the first few lines of the documents as a given LIST."
  (cond ((>= (length list) 6) (subseq list 1 5))
        ((>= (length list) 1) (nthcdr 1 list))
        (t list)))

(defun org-journal-list--read-journal-heads (path)
  "Read the specific note at PATH as a list, returning the first few lines."
  (mapconcat (function (lambda (line) (format "  %s" line)))
             (org-journal-list--read-first-few-lines (org-journal-list--read-file path)) "\n"))

(defun org-journal-list--start ()
  "Start org-journal-list mode, this function should be binded to a keystroke."
  (interactive)
  ;(setq split-width-threshold 0)
  (setq journal-list-buffer (get-buffer-create (generate-new-buffer-name "*journals*")))
  (with-current-buffer journal-list-buffer
    (org-mode)
    (setq org-journal-list--file-list (directory-files org-journal-list-default-directory nil "\\.org$"))
    (setq org-journal-list--file-list
          (mapcar (lambda (item)
                    (format "* [[file:%s%s][%s]]\n%s\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                            org-journal-list-default-directory
                            item
                            item
                            (org-journal-list--read-journal-heads item))
                    ) org-journal-list--file-list))
    (insert (mapconcat 'identity org-journal-list--file-list "\n"))
    (beginning-of-buffer)
    (toggle-read-only t))
  (find-file (concat org-journal-list-default-directory (format-time-string "%Y-%m-%d") org-journal-list-default-suffix))
  (display-buffer-in-side-window journal-list-buffer org-journal-list-display-alist))

(provide 'org-journal-list)
;;; org-journal-list.el ends here
