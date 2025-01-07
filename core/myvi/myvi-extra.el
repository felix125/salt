;;; -*- coding: utf-8; lexical-binding: t -*-

(defun myvi-quit-buffer (&optional force)
  "Kill current buffer and switch to *scratch*.
If FORCE is non-nil, abandon any modifications."
  (interactive)
  (let ((current (current-buffer)))
    (condition-case err
        (progn
          ;; Check if there are unsaved modifications
          (when (and (not force)
                     (buffer-modified-p))
            (error "Buffer modified; use :q! to force quit"))

          ;; Prepare to switch to *scratch*
          (if (get-buffer "*scratch*")
              (switch-to-buffer "*scratch*")
            (switch-to-buffer (get-buffer-create "*scratch*"))
            (lisp-interaction-mode))

          ;; Close the original buffer
          (kill-buffer current))
      (error (message "Error: %s" (error-message-string err))))))

;; Another read-char
(defun myvi-read-char ()
  "Wrapper around `read-char`."
  (let ((event (read-event)))
    (if (characterp event) 
        event
      (progn (message "Unknown command")
             nil))))

;; deal with the Numeric Arguments
(defun myvi-read-number (first-digit)
  "Read number starting with FIRST-DIGIT. Return (number . next-char)."
  (let ((num (- first-digit ?0))
        (limit 10000)
        (ch (myvi-read-char)))
    (while (and ch (<= ?0 ch ?9) (<= num limit))
      (setq num (+ (* num 10) (- ch ?0)))
      (setq ch (myvi-read-char)))
    (when (> num limit)
      (message "Number too large, using %d" limit)
      (setq num limit))
    (cons num ch)))

(defun myvi-handle-command (type)
  "Handle vi-like commands for TYPE (d or c)."
  (let* ((first-ch (myvi-read-char))
         (num-and-ch (if (and first-ch (<= ?1 first-ch ?9))
                        (myvi-read-number first-ch)
                      (cons 1 first-ch)))
         (num (car num-and-ch))
         (cmd (cdr num-and-ch)))
    (if (null cmd)
        (message "Unknown command")
      (cond
       ((char-equal cmd type) (myvi-delete-line num type))
       ((char-equal cmd ?w) (myvi-delete-word num type))
       ((char-equal cmd ?b) (myvi-delete-backward-word num type))
       ((char-equal cmd ?$) (myvi-delete-to-eol type))
       ((char-equal cmd ?0) (myvi-delete-to-bol type))
       (t (message "Unknown command"))))))

;; yank
(defun myvi-yank-region (orig beg end)
  "Yank (copy) the region from BEG to END and restore cursor to ORIG."
  (goto-char beg)
  (push-mark end t t)
  (sit-for 0.15) 
  (kill-ring-save beg end)
  (goto-char orig))        

;; delete/yank N lines
(defun myvi-delete-line (n type)
  "Delete/yank N lines based on TYPE."
  (let ((orig (point))
        (beg (progn (beginning-of-line) (point)))
        (end (progn (forward-line n) (point))))
    (if (eq type ?y)
        (myvi-yank-region orig beg end)
      (progn
        (goto-char beg)
        (kill-line n)
        (when (eq type ?c)
          (open-line 1))))))

;; delete/yank to beginning of line
(defun myvi-delete-to-bol (type)
  "Delete/yank to beginning of line based on TYPE."
  (let ((beg (line-beginning-position))
        (end (point)))
    (if (eq type ?y)
        (myvi-yank-region end beg end)
      (kill-region beg end))))

;; delete/yank to end of line
(defun myvi-delete-to-eol (type)
  "Delete/yank to end of line based on TYPE."
  (let ((beg (point))
        (end (line-end-position)))
    (if (eq type ?y)
	(myvi-yank-region beg beg end)
      (kill-region beg end))))

;; delete/yank N words backward
(defun myvi-delete-backward-word (n type)
  "Delete/yank N words backward based on TYPE."
  (let ((end (point))
	(beg (progn
               (backward-word n)
               (point))))
    (if (eq type ?y)
        (myvi-yank-region end beg end)
      (kill-region beg end))))

;; delete/yank N words forward to their end
(defun myvi-delete-word (n type)
  "Delete N words forward to their end."
  (let ((beg (point))
        (end (progn
               (forward-word n)
               (point))))
    (if (eq type ?y)
          (myvi-yank-region beg beg end)
      (kill-region beg end))))


(provide 'myvi-extra)
;;; myvi-extra.el ends here
