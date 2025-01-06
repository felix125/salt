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
       ((char-equal cmd ?w) (myvi-delete-word num))
       ((char-equal cmd ?b) (myvi-delete-backward-word num))
       ((char-equal cmd ?e) (myvi-delete-to-word-end num))
       ((char-equal cmd ?$) (myvi-delete-to-eol))
       ((char-equal cmd ?0) (myvi-delete-to-bol))
       (t (message "Unknown command"))))))


(defun myvi-delete-line (n type)
  "Delete N lines. If TYPE is ?c, insert a new line."
  (beginning-of-line)
  (kill-line n)
  (when (eq type ?c)
    (open-line 1)))

(defun myvi-delete-to-bol ()
  "Delete to beginning of line."
  (kill-region (line-beginning-position) (point)))

(defun myvi-delete-to-eol ()
  "Delete to end of line."
  (kill-region (point) (line-end-position)))

(defun myvi-delete-word (n)
  "Delete N words forward."
  (kill-region (point)
               (progn (forward-word (+ n 1))
		      (backward-word)
		      (point))))

(defun myvi-delete-backward-word (n)
  "Delete N words backward."
  (kill-region (point)
               (progn (backward-word n) (point))))

(defun myvi-delete-to-word-end (n)
  "Delete N words forward to their end."
  (kill-region (point)
               (progn 
                 (forward-word n)
                 (backward-word n)
                 (forward-word n)
                 (point))))


(provide 'myvi-extra)
;;; myvi-extra.el ends here
