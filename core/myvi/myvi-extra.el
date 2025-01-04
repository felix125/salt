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

(provide 'myvi-extra)
;;; myvi-extra.el ends here
