;;; my-journal.el --- Simple journal management -*- lexical-binding: t -*-

;; Copyright (C) 2025 Felix Chang

;; Author: Felix Chang <felix.profecia@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: journal, notes
;; URL: https://github.com/felix125/my-journal

;;; Commentary:

;; A simple journal management package based on org-mode.
;;

;;; Code:

(defgroup my-journal nil
  "Settings for my-journal."
  :group 'org)

(defcustom my-journal-directory "~/journal/"
  "Directory for journal files."
  :type 'directory
  :group 'my-journal)

(defcustom my-journal-date-format "* %Y-%m-%d, %A"
  "Format for level 1 heading date.
See `format-time-string' for formatting options."
  :type 'string
  :group 'my-journal)

(defcustom my-journal-time-format "%H%M"
  "Format for level 2 heading time.
See `format-time-string' for formatting options."
  :type 'string
  :group 'my-journal)

(defcustom my-journal-file-format "%Y-%m.org"
  "Format for journal file names.
See `format-time-string' for formatting options."
  :type 'string
  :group 'my-journal)

(defcustom my-journal-title-format "#+TITLE: Journal %Y-%m"
  "Format for journal file title.
See `format-time-string' for formatting options."
  :type 'string
  :group 'my-journal)

;; 修改相關函數來使用這些變數
(defun my-journal-file-path ()
  "Return journal file path for current month."
  (let ((now (current-time)))
    (expand-file-name
     (format-time-string my-journal-file-format now)
     my-journal-directory)))

(defun my-journal-ensure-date-heading ()
  "Ensure level 1 heading exists for today's date with creation timestamp in properties."
  (let ((date-string (format-time-string my-journal-date-format))
        (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (goto-char (point-min))
    (unless (search-forward date-string nil t)
      (goto-char (point-max))
      (insert "\n" date-string "\n"
              ":PROPERTIES:\n"
              ":CREATED:  " timestamp "\n"
              ":END:\n"))))


;;;###autoload
(put 'my-journal-new-entry 'function-documentation
     "Create a new journal entry for current time.")

;;;###autoload
(defun my-journal-new-entry ()
  "Create a new journal entry for current time."
  (interactive)
  (let ((file (my-journal-file-path)))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format-time-string my-journal-title-format) "\n\n"))
    (my-journal-ensure-date-heading)
    (goto-char (point-max))
    (insert "** " (format-time-string my-journal-time-format))
    (just-one-space)))


;;;###autoload
(put 'my-journal-read-last-entry 'function-documentation
     "Open journal and move cursor to the last entry.
If no entry exists for today, create a new one.")

;;;###autoload
(defun my-journal-read-last-entry ()
  "Open journal and move cursor to the last entry.
If no entry exists for today, create a new one."
  (interactive)
  (let ((file (my-journal-file-path)))
    (if (file-exists-p file)
        (progn
          (find-file file)
          (goto-char (point-max))
          (if (re-search-backward "^\\*\\* " nil t)
              (let ((last-entry-pos (point)))  ; 記住最後一個 entry 的位置
                (beginning-of-line)
                (re-search-backward "^\\* " nil t)  ; 找到當天的 level 1 heading
                (org-show-children 1)              ; 展開一層
                (goto-char last-entry-pos))        ; 回到最後一個 entry
            (my-journal-new-entry)))
      (my-journal-new-entry))))


(provide 'my-journal)
;;; my-journal.el ends here
