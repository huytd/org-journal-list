;;; org-journal-list.el --- Org mode Journal List

;;; Commentary:
;;;  This package does not provide any key binding. You will
;;;  have to do it yourself.
;;;  For example, to bind Super-J as a keystroke to open journal
;;;  list, do this:
;;;    (global-set-key (kbd "s-j") 'org-journal-list--start)

;;; Version: 1.0
;;; URL: https://github.com/huytd/org-journal-list
;;; Package-Requires: ((emacs "24.3"))

;;; Code:
(require 'cl-extra)

(defgroup org-journal-list nil
  "Customization group for org-journal-list."
  :prefix "org-journal-list-"
  :group 'comint
  :link '(url-link :tag "GitHub" "https://github.com/huytd/org-journal-list"))

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
  (cond ((>= (length list) 6) (cl-subseq list 1 5))
        ((>= (length list) 1) (nthcdr 1 list))
        (t list)))

(defun org-journal-list--read-journal-heads (path)
  "Read the specific note at PATH as a list, returning the first few lines."
  (mapconcat (function (lambda (line) (format "  %s" line)))
             (org-journal-list--read-first-few-lines (org-journal-list--read-file path)) "\n"))

(defun org-journal-list--format-item-string (item)
  "Generate the string for each ITEM on the sidebar."
  (format "* [[file:%s%s][%s]]\n%s\n%s"
          org-journal-list-default-directory
          item
          item
          (org-journal-list--read-journal-heads item)
          (make-string 35 ?━)))

(defun org-journal-list--start ()
  "Start org-journal-list mode, this function should be binded to a keystroke."
  (interactive)
  (let ((journal-list-buffer (get-buffer-create (generate-new-buffer-name "*journals*"))))
    (with-current-buffer journal-list-buffer
      (org-mode)
      (let ((file-list (directory-files org-journal-list-default-directory nil "\\.org$")))
        (let ((file-list-text (mapcar 'org-journal-list--format-item-string file-list)))
          (insert (mapconcat 'identity file-list-text "\n"))
          (goto-char (point-min))
          (read-only-mode t))))
    (find-file (concat org-journal-list-default-directory (format-time-string "%Y-%m-%d") org-journal-list-default-suffix))
    (display-buffer-in-side-window journal-list-buffer org-journal-list-display-alist)))

(provide 'org-journal-list)
;;; org-journal-list.el ends here
